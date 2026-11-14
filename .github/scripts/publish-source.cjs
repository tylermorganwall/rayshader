// Called only in the master-branch publication job, after the read-only build.
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');

module.exports = async function publishSource({ github, context, core, directory }) {
  if (!['push', 'workflow_dispatch'].includes(context.eventName) ||
      context.ref !== 'refs/heads/master') {
    throw new Error('Source publication is restricted to master-branch pushes and manual runs');
  }

  const repo = context.repo;
  const metadata = JSON.parse(fs.readFileSync(path.join(directory, 'source-build.json'), 'utf8'));
  if (metadata.branch !== 'master' || metadata.sha !== context.sha ||
      metadata.run_id !== context.runId ||
      !Number.isSafeInteger(metadata.run_number) || metadata.run_number < 1 ||
      !Number.isSafeInteger(metadata.run_attempt) || metadata.run_attempt < 1 ||
      !/^[A-Za-z][A-Za-z0-9.]*$/.test(metadata.package) ||
      !/^[0-9][A-Za-z0-9.+-]*$/.test(metadata.version) ||
      !/^[A-Za-z0-9._+-]+\.tar\.gz$/.test(metadata.filename) ||
      !/^[0-9a-f]{64}$/.test(metadata.sha256)) {
    throw new Error('Source build metadata does not match this run');
  }
  const bytes = fs.readFileSync(path.join(directory, metadata.filename));
  const checksum = crypto.createHash('sha256').update(bytes).digest('hex');
  if (checksum !== metadata.sha256) throw new Error('Source tarball checksum mismatch');

  const branch = await github.rest.repos.getBranch({ ...repo, branch: 'master' });
  if (branch.data.commit.sha !== context.sha) {
    core.notice('A newer master commit exists; keeping the latest successful download unchanged.');
    return;
  }

  const tag = 'source-master';
  let release;
  try {
    release = (await github.rest.repos.getReleaseByTag({ ...repo, tag })).data;
  } catch (error) {
    if (error.status !== 404) throw error;
    // The tag endpoint can omit drafts. Resume an interrupted first upload
    // instead of creating another draft with the same tag.
    const releases = await github.paginate(github.rest.repos.listReleases, {
      ...repo, per_page: 100,
    });
    release = releases.find(item => item.tag_name === tag);
  }
  if (release) {
    const publishedRun = release.body?.match(/<!-- source-build-run: (\d+) -->/);
    if (publishedRun && Number(publishedRun[1]) > metadata.run_number) {
      core.notice('A newer run has already published; refusing to replace it.');
      return;
    }
  } else {
    // Keep a first release private until its uploaded tarball has been verified.
    release = (await github.rest.repos.createRelease({
      ...repo, tag_name: tag, target_commitish: context.sha,
      name: 'Latest master source build', draft: true, prerelease: true,
      make_latest: 'false',
      body: 'Source build upload in progress.',
    })).data;
  }

  const assets = await github.paginate(github.rest.repos.listReleaseAssets, {
    ...repo, release_id: release.id, per_page: 100,
  });
  let asset = assets.find(item => item.name === metadata.filename);
  if (!asset) {
    asset = (await github.rest.repos.uploadReleaseAsset({
      ...repo, release_id: release.id, name: metadata.filename, data: bytes,
      headers: { 'content-type': 'application/gzip', 'content-length': bytes.length },
    })).data;
  }
  // Download what GitHub stored before changing the latest-successful pointer.
  const downloaded = await github.rest.repos.getReleaseAsset({
    ...repo, asset_id: asset.id, headers: { accept: 'application/octet-stream' },
  });
  const downloadedChecksum = crypto.createHash('sha256')
    .update(Buffer.from(downloaded.data)).digest('hex');
  if (downloadedChecksum !== checksum) throw new Error('Uploaded tarball failed download verification');

  const currentBranch = await github.rest.repos.getBranch({ ...repo, branch: 'master' });
  if (currentBranch.data.commit.sha !== context.sha) {
    core.notice('master advanced during upload; leaving the previous download selected.');
    return;
  }

  const repositoryUrl = `${context.serverUrl}/${repo.owner}/${repo.repo}`;
  const runUrl = `${repositoryUrl}/actions/runs/${context.runId}`;
  // Draft assets can have temporary untagged-* URLs. Publishing changes them,
  // so link to the final tag and asset name in both the release and run summary.
  const downloadUrl = `${repositoryUrl}/releases/download/${encodeURIComponent(tag)}/${encodeURIComponent(asset.name)}`;
  const body = [
    `Download the latest successfully built **${metadata.package} ${metadata.version}** source package from **master**:`,
    '', `[**Download the R source tarball**](${downloadUrl})`, '',
    `Commit: [${context.sha}](${repositoryUrl}/commit/${context.sha})`,
    `Build: [GitHub Actions run ${metadata.run_number}](${runUrl})`,
    `SHA-256: \`${checksum}\``, '',
    'This is a development source build, not a CRAN release or a compiled binary.',
    'Use the linked .tar.gz asset above, not GitHub’s automatically generated Source code archives.',
    'Older build assets are retained so previously shared download links keep working.', '',
    `<!-- source-build-run: ${metadata.run_number} -->`,
  ].join('\n');
  await github.rest.repos.updateRelease({
    ...repo, release_id: release.id, name: 'Latest master source build',
    body, draft: false, prerelease: true, make_latest: 'false',
  });
  await core.summary.addRaw([
    `### ${metadata.package} ${metadata.version}`, '',
    `[Download the source tarball](${downloadUrl})`, '',
    `[Latest successful master build](${repositoryUrl}/releases/tag/${tag})`, '',
    `Branch: master · Commit: ${context.sha}`, '', `SHA-256: ${checksum}`, '',
  ].join('\n')).write();
};
