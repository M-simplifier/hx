# Release Process

This document defines the lightweight release process for the public pre-beta
series.

## Versioning

- Use `v<package-version>-pre.N` tags for pre-beta source releases
- The first intended public tag is `v0.1.0-pre.1`
- Keep `hx.cabal` as the source of truth for the package version
- `hx version` should reflect `hx.cabal` automatically

## Before tagging

- Make sure Linux CI is green
- Run `scripts/release-preflight.sh` and the checks in `docs/release-checklist.md`
- Run `scripts/public-ci.sh`
- Make sure the source distribution remains releasable via `scripts/release-sdist-check.sh`
- Review `docs/release-notes-template.md`
- Review `docs/known-limitations.md`
- Confirm the intended version in `hx.cabal`

## Tagging

- Create the annotated tag using the chosen pre-beta version
- Draft release notes from `docs/release-notes-template.md`
- Optionally scaffold a concrete draft with `scripts/create-release-draft.sh <tag>`
- Copy the current relevant items from `docs/known-limitations.md`
- Treat the generated source tarball in `dist-newstyle/sdist/` as part of the release evidence

Example:

```bash
scripts/public-ci.sh
scripts/release-preflight.sh v0.1.0-pre.1
scripts/create-release-draft.sh v0.1.0-pre.1
git tag -a v0.1.0-pre.1 -m "hx v0.1.0-pre.1"
```

## Publishing

- Publish the tag
- Publish release notes
- Include install instructions
- Note that pre-beta installs are still source-oriented rather than binary-installer based
- Include support matrix and non-goals
- Include feedback instructions that ask for `hx doctor` output

## After publishing

- Watch incoming bug reports for missing environment details
- Update `docs/known-limitations.md` when a limitation is confirmed
- Fold repeated issue patterns back into `hx-smoke` where feasible
