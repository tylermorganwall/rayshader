#!/usr/bin/env node
'use strict';
const assert = require('node:assert/strict');
const crypto = require('node:crypto');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { spawnSync } = require('node:child_process');
const { test } = require('node:test');

// Test the actual YAML blocks, rather than a second implementation of them.
const workflow = path.resolve(process.env.SOURCE_BUILD_WORKFLOW || '.github/workflows/source-build.yaml');
const yaml = fs.readFileSync(workflow, 'utf8');
function runBlock(name) {
  const lines = yaml.split('\n');
  const start = lines.findIndex(line => line.trim() === `- name: ${name}`);
  assert.notEqual(start, -1, `Missing step: ${name}`);
  const begin = lines.findIndex((line, i) => i > start && line.trim() === 'run: |');
  assert.notEqual(begin, -1, `Missing run block: ${name}`);
  const end = lines.findIndex((line, i) => i > begin && line.trim() && !line.startsWith('          '));
  return lines.slice(begin + 1, end === -1 ? undefined : end).map(line => line.slice(10)).join('\n');
}
const build = runBlock('Build the triggering commit');
const label = runBlock('Verify and label the source package');
const verify = runBlock('Verify the downloaded tarball and metadata');
const sha256 = bytes => crypto.createHash('sha256').update(bytes).digest('hex');
const description = `Package: testpkg
Version: 1.2.3-4
Title: Source Build Test Fixture
Description: Provides a minimal package for workflow regression tests.
Authors@R: person("Test", "Runner", email = "test@example.com", role = c("aut", "cre"))
License: MIT
Encoding: UTF-8
`;
function command(program, args, options = {}) {
  const result = spawnSync(program, args, { encoding: 'utf8', ...options });
  if (result.error) throw result.error;
  return result;
}
function passed(result) {
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return result;
}
function fixture(t, branch = 'main') {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'source-workflow-test-'));
  t.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const source = path.join(root, 'source');
  const runner = path.join(root, 'runner');
  fs.mkdirSync(path.join(source, 'R'), { recursive: true });
  fs.mkdirSync(path.join(runner, 'source-build'), { recursive: true });
  fs.mkdirSync(path.join(runner, 'source-output'));
  fs.writeFileSync(path.join(source, 'DESCRIPTION'), description);
  fs.writeFileSync(path.join(source, 'NAMESPACE'), 'export(test_value)\n');
  fs.writeFileSync(path.join(source, 'R', 'value.R'), 'test_value <- function() 42L\n');
  const env = { ...process.env, GITHUB_WORKSPACE: source, RUNNER_TEMP: runner,
    GITHUB_REF_NAME: branch, GITHUB_SHA: 'a'.repeat(40), GITHUB_RUN_ID: '123456',
    GITHUB_RUN_NUMBER: '987', GITHUB_RUN_ATTEMPT: '2' };
  const f = { root, source, runner, env, output: path.join(runner, 'source-output'),
    download: path.join(runner, 'source-download'), archive: path.join(runner, 'source-build', 'testpkg_1.2.3-4.tar.gz') };
  f.run = block => command('bash', ['--noprofile', '--norc', '-c', block], { env, cwd: source });
  f.tar = (contents = description, member = 'testpkg/DESCRIPTION') => {
    const staging = path.join(root, 'staging');
    fs.rmSync(staging, { recursive: true, force: true });
    fs.mkdirSync(path.dirname(path.join(staging, member)), { recursive: true });
    fs.writeFileSync(path.join(staging, member), contents);
    passed(command('tar', ['-czf', f.archive, '-C', staging, member]));
  };
  f.label = () => passed(f.run(label));
  f.metadata = () => JSON.parse(fs.readFileSync(path.join(f.output, 'source-build.json'), 'utf8'));
  f.downloadArtifact = () => fs.cpSync(f.output, f.download, { recursive: true });
  return f;
}

for (const [name, block] of [['build', build], ['label', label], ['verify', verify]]) {
  test(`Bash syntax: ${name}`, () => passed(command('bash', ['-n'], { input: block })));
}
test('no Python helper calls and read-only build', () => {
  assert.doesNotMatch(yaml, /build-source\.py/);
  assert.match(yaml, /permissions:\n  contents: read/);
});
test('real R CMD build, complete metadata and downloaded integrity', t => {
  const f = fixture(t);
  passed(f.run(build));
  f.label();
  const m = f.metadata();
  const filename = `testpkg_1.2.3-4_main-${sha256('main').slice(0, 12)}_${'a'.repeat(40)}_123456-2.tar.gz`;
  const bytes = fs.readFileSync(path.join(f.output, filename));
  assert.deepEqual(m, { package: 'testpkg', version: '1.2.3-4', branch: 'main', sha: 'a'.repeat(40), run_id: 123456, run_number: 987, run_attempt: 2, filename, sha256: sha256(bytes) });
  assert.equal(fs.readFileSync(path.join(f.output, filename + '.sha256'), 'utf8'), `${m.sha256}  ${filename}\n`);
  f.downloadArtifact();
  passed(f.run(verify));
});
for (const branch of ['feature/topic', 'feature-topic', '42/merge', 'feat/é☃"$(touch pwned)', 'feature/' + 'a'.repeat(100)]) {
  test(`safe deterministic filename and JSON for branch ${JSON.stringify(branch)}`, t => {
    const f = fixture(t, branch);
    f.tar(); f.label();
    const m = f.metadata();
    const slug = Array.from(branch, ch => /^[A-Za-z0-9._-]$/.test(ch) ? ch : '-').join('').slice(0, 60);
    assert.equal(m.branch, branch);
    assert.equal(m.filename, `testpkg_1.2.3-4_${slug}-${sha256(branch).slice(0, 12)}_${'a'.repeat(40)}_123456-2.tar.gz`);
    assert.match(m.filename, /^[A-Za-z0-9._+-]+\.tar\.gz$/);
    assert.equal(fs.existsSync(path.join(f.source, 'pwned')), false);
    f.downloadArtifact(); passed(f.run(verify));
  });
}
for (const [name, mutate] of [
  ['no tarball', f => fs.unlinkSync(f.archive)],
  ['multiple tarballs', f => fs.copyFileSync(f.archive, f.archive + '.extra.tar.gz')],
  ['hidden extra tarball', f => fs.copyFileSync(f.archive, path.join(path.dirname(f.archive), '.extra.tar.gz'))],
  ['mismatched version', f => f.tar(description.replace('1.2.3-4', '9.0'))],
  ['mismatched package', f => f.tar(description.replace('testpkg', 'otherpkg'))],
  ['missing archive DESCRIPTION', f => f.tar(description, 'testpkg/OTHER')],
  ['multiple archived DESCRIPTION records', f => f.tar(description + '\n' + description)],
  ['missing source version', f => fs.writeFileSync(path.join(f.source, 'DESCRIPTION'), description.replace('Version: 1.2.3-4\n', ''))],
  ['unsafe package', f => fs.writeFileSync(path.join(f.source, 'DESCRIPTION'), description.replace('Package: testpkg', 'Package: ../testpkg'))],
  ['unsafe version', f => fs.writeFileSync(path.join(f.source, 'DESCRIPTION'), description.replace('Version: 1.2.3-4', 'Version: ../1'))],
  ['invalid commit SHA', f => { f.env.GITHUB_SHA = '../oops'; }],
  ['invalid run ID', f => { f.env.GITHUB_RUN_ID = '{"oops":1}'; }],
  ['zero run number', f => { f.env.GITHUB_RUN_NUMBER = '0'; }],
  ['invalid attempt', f => { f.env.GITHUB_RUN_ATTEMPT = 'no'; }],
]) {
  test(`rejects ${name}`, t => {
    const f = fixture(t); f.tar(); mutate(f);
    const result = f.run(label);
    assert.notEqual(result.status, 0, `Unexpected success: ${result.stdout}`);
  });
}
for (const [name, mutate] of [
  ['changed tarball', (f, m) => fs.appendFileSync(path.join(f.download, m.filename), 'tampered')],
  ['changed metadata', f => fs.appendFileSync(path.join(f.download, 'source-build.json'), '\n')],
  ['changed checksum', (f, m) => fs.writeFileSync(path.join(f.download, m.filename + '.sha256'), `${'b'.repeat(64)}  ${m.filename}\n`)],
  ['renamed tarball', (f, m) => fs.renameSync(path.join(f.download, m.filename), path.join(f.download, 'renamed.tar.gz'))],
  ['missing tarball', (f, m) => fs.unlinkSync(path.join(f.download, m.filename))],
  ['extra tarball', (f, m) => fs.copyFileSync(path.join(f.download, m.filename), path.join(f.download, 'extra.tar.gz'))],
  ['missing checksum', (f, m) => fs.unlinkSync(path.join(f.download, m.filename + '.sha256'))],
  ['extra checksum', (f, m) => fs.copyFileSync(path.join(f.download, m.filename + '.sha256'), path.join(f.download, 'extra.sha256'))],
  ['missing metadata', f => fs.unlinkSync(path.join(f.download, 'source-build.json'))],
  ['forged tarball, metadata and checksum', (f, m) => {
    const bytes = Buffer.from('forged'); const digest = sha256(bytes);
    fs.writeFileSync(path.join(f.download, m.filename), bytes);
    fs.writeFileSync(path.join(f.download, m.filename + '.sha256'), `${digest}  ${m.filename}\n`);
    fs.writeFileSync(path.join(f.download, 'source-build.json'), JSON.stringify({ ...m, sha256: digest }, null, 2) + '\n');
  }],
]) {
  test(`rejects downloaded artifact with ${name}`, t => {
    const f = fixture(t); f.tar(); f.label(); f.downloadArtifact(); mutate(f, f.metadata());
    const result = f.run(verify);
    assert.notEqual(result.status, 0, `Unexpected success: ${result.stdout}`);
  });
}
