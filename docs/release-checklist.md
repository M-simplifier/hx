# Release Checklist

This checklist is for public pre-beta tags.

## Product

- Confirm the support matrix in `README.md` still matches reality
- Confirm the current milestone in `docs/roadmap.md`
- Confirm non-goals are still explicit enough for pre-beta users
- Confirm the intended package version in `hx.cabal`
- Confirm `hx version` reports that package version
- Confirm `STATUS.md` separates implemented, verified, and not claimed

## Verification

- Run `scripts/release-preflight.sh` or `scripts/release-preflight.sh <tag>`
- Run `scripts/public-ci.sh`
- Run `scripts/release-sdist-check.sh`
- Run `cabal build all`
- Run `cabal test hx-smoke`
- Run `cabal run hx -- doctor`
- Run `cabal run hx -- build --help`
- Run `cabal run hx -- run --help`

## Documentation

- Review `README.md` install instructions
- Review `README.md` feedback instructions
- Review `STATUS.md`
- Review `AGENTS.md`
- Review `docs/vision.md` and `docs/roadmap.md` for obvious drift
- Review `docs/release-process.md`
- Review `docs/release-notes-template.md`
- Review `docs/known-limitations.md`
- Review `.github/ISSUE_TEMPLATE/bug-report.md`

## Release Preparation

- Confirm `.github/workflows/ci.yml` still reflects the supported Linux toolchain
- Confirm `.github/workflows/pages.yml` still deploys only the static `site/` artifact
- Confirm the GitHub issue template still asks for `hx doctor` output
- Confirm the tag name matches the policy in `docs/release-process.md`
- Confirm the release tarball under `dist-newstyle/sdist/` matches the current package version
- Summarize notable changes since the previous tag
- Note any known limitations that pre-beta users should expect

## Publish

- Create the tag
- Draft release notes
- Fill `docs/release-notes-template.md`
- If useful, generate a draft with `scripts/create-release-draft.sh <tag>`
- Include install instructions
- Include support matrix / non-goals
- Include known limitations from `docs/known-limitations.md`
