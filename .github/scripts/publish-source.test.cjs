const assert = require('node:assert/strict');
const crypto = require('node:crypto');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const test = require('node:test');
const publishSource = require('./publish-source.cjs');

function fixture(t, options = {}) {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'source-publish-test-'));
  t.after(() => fs.rmSync(directory, { recursive: true, force: true }));
  const bytes = Buffer.from('verified source archive fixture');
  const metadata = {
    package: 'cgltf', version: '1.15.0-0', branch: 'master', sha: 'a'.repeat(40),
    run_id: 123, run_number: 12, run_attempt: 1,
    filename: 'cgltf_1.15.0-0_master_aaaaaaaa_123-1.tar.gz',
    sha256: crypto.createHash('sha256').update(bytes).digest('hex'),
    ...options.metadata,
  };
  fs.writeFileSync(path.join(directory, 'source-build.json'), JSON.stringify(metadata));
  fs.writeFileSync(path.join(directory, path.basename(metadata.filename)), bytes);
  const calls = [];
  let branchReads = 0;
  const assetTag = options.releaseMissing || options.existingDraft ? 'untagged-draft-id' : 'source-master';
  const asset = { id: 9, name: metadata.filename, browser_download_url: `https://github.com/example/repo/releases/download/${assetTag}/${encodeURIComponent(metadata.filename)}` };
  const repos = {
    getBranch: async () => ({ data: { commit: { sha: ++branchReads > 1 && options.advanceDuringUpload ? 'b'.repeat(40) : options.branchSha || metadata.sha } } }),
    getReleaseByTag: async () => {
      if (options.releaseMissing || options.existingDraft) throw Object.assign(new Error('Not found'), { status: 404 });
      return { data: { id: 5, body: `<!-- source-build-run: ${options.publishedRun || 11} -->` } };
    },
    createRelease: async args => { calls.push(['create', args]); return { data: { id: 5 } }; },
    listReleases: async () => {},
    listReleaseAssets: async () => {},
    uploadReleaseAsset: async args => { calls.push(['upload', args]); return { data: asset }; },
    getReleaseAsset: async () => {
      calls.push(['download']);
      if (options.downloadFails) throw new Error('download failed');
      return { data: options.badDownload ? Buffer.from('corrupted') : bytes };
    },
    updateRelease: async args => { calls.push(['update', args]); return { data: args }; },
  };
  const github = { rest: { repos }, paginate: async method => {
    if (method === repos.listReleases) return options.existingDraft ? [{ id: 5, tag_name: 'source-master', draft: true, body: 'Upload in progress.' }] : [];
    return options.existingAsset ? [asset] : [];
  } };
  const core = { notice: message => calls.push(['notice', message]), summary: { addRaw: text => ({ write: async () => calls.push(['summary', text]) }) } };
  const context = {
    eventName: 'push', ref: 'refs/heads/master', sha: 'a'.repeat(40), runId: 123,
    repo: { owner: 'example', repo: 'repo' }, serverUrl: 'https://github.com',
    ...options.context,
  };
  return { args: { github, core, context, directory }, calls };
}

test('uploads and downloads before changing the latest pointer', async t => {
  const { args, calls } = fixture(t);
  await publishSource(args);
  assert.deepEqual(calls.map(call => call[0]), ['upload', 'download', 'update', 'summary']);
  assert.match(calls[2][1].body, /cgltf 1\.15\.0-0/);
  assert.match(calls[2][1].body, /source-build-run: 12/);
  assert.equal(calls[2][1].make_latest, 'false');
});

test('first release stays draft until the download is verified', async t => {
  const { args, calls } = fixture(t, { releaseMissing: true });
  await publishSource(args);
  assert.equal(calls[0][1].draft, true);
  assert.deepEqual(calls.map(call => call[0]), ['create', 'upload', 'download', 'update', 'summary']);
  assert.equal(calls[3][1].draft, false);
});

test('resumes an interrupted first draft instead of creating another', async t => {
  const { args, calls } = fixture(t, { existingDraft: true });
  await publishSource(args);
  assert.deepEqual(calls.map(call => call[0]), ['upload', 'download', 'update', 'summary']);
  assert.equal(calls[2][1].draft, false);
});

for (const options of [
  { releaseMissing: true },
  { existingDraft: true },
  { existingDraft: true, existingAsset: true },
]) {
  test(`uses the published tag URL instead of a temporary draft URL ${JSON.stringify(options)}`, async t => {
    const { args, calls } = fixture(t, options);
    await publishSource(args);
    const expectedUrl = 'https://github.com/example/repo/releases/download/source-master/cgltf_1.15.0-0_master_aaaaaaaa_123-1.tar.gz';
    const body = calls.find(call => call[0] === 'update')[1].body;
    const summary = calls.find(call => call[0] === 'summary')[1];
    for (const text of [body, summary]) {
      assert.ok(text.includes(`](${expectedUrl})`));
      assert.doesNotMatch(text, /untagged-/);
    }
  });
}

test('encodes asset names and uses the current repository and server in download links', async t => {
  const { args, calls } = fixture(t, {
    releaseMissing: true,
    metadata: { filename: 'cgltf_1.15.0+dev_master_aaaaaaaa_123-1.tar.gz' },
    context: { serverUrl: 'https://github.example.com', repo: { owner: 'another', repo: 'package' } },
  });
  await publishSource(args);
  const expectedUrl = 'https://github.example.com/another/package/releases/download/source-master/cgltf_1.15.0%2Bdev_master_aaaaaaaa_123-1.tar.gz';
  assert.ok(calls.find(call => call[0] === 'update')[1].body.includes(`](${expectedUrl})`));
  assert.ok(calls.find(call => call[0] === 'summary')[1].includes(`](${expectedUrl})`));
});

for (const context of [{ eventName: 'pull_request' }, { ref: 'refs/heads/feature' }]) {
  test(`rejects unauthorized context ${JSON.stringify(context)}`, async t => {
    const { args, calls } = fixture(t, { context });
    await assert.rejects(publishSource(args), /restricted/);
    assert.equal(calls.length, 0);
  });
}

for (const options of [{ branchSha: 'b'.repeat(40) }, { publishedRun: 13 }]) {
  test(`skips stale run ${JSON.stringify(options)}`, async t => {
    const { args, calls } = fixture(t, options);
    await publishSource(args);
    assert.deepEqual(calls.map(call => call[0]), ['notice']);
  });
}

for (const options of [{ badDownload: true }, { downloadFails: true }]) {
  test(`keeps old download on verification failure ${JSON.stringify(options)}`, async t => {
    const { args, calls } = fixture(t, options);
    await assert.rejects(publishSource(args));
    assert.equal(calls.some(call => call[0] === 'update'), false);
  });
}

test('does not promote when master advances during the upload', async t => {
  const { args, calls } = fixture(t, { advanceDuringUpload: true });
  await publishSource(args);
  assert.deepEqual(calls.map(call => call[0]), ['upload', 'download', 'notice']);
});

test('reuses an existing immutable asset only after downloading and verifying it', async t => {
  const { args, calls } = fixture(t, { existingAsset: true });
  await publishSource(args);
  assert.deepEqual(calls.map(call => call[0]), ['download', 'update', 'summary']);
});

for (const metadata of [{ branch: 'feature' }, { run_id: 124 }, { filename: '../unexpected.tar.gz' }, { sha256: 'b'.repeat(64) }]) {
  test(`rejects invalid metadata ${JSON.stringify(metadata)}`, async t => {
    const { args, calls } = fixture(t, { metadata });
    await assert.rejects(publishSource(args));
    assert.equal(calls.length, 0);
  });
}
