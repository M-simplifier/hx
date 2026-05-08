# Pre-Beta Release Notes Template

Use this as the starting point for public pre-beta release notes.

## Summary

State in a short paragraph what this pre-beta release is for.

## Included in this release

- `hx init`
- `hx status`
- `hx add`
- `hx doctor`
- `hx build`
- `hx run`
- `hx test`
- `hx ci`
- current build/run profiles: `dev`, `release`, `server`

## What improved since the previous tag

- item 1
- item 2
- item 3

## Verification

- `scripts/public-ci.sh`
- `scripts/release-preflight.sh <tag>`
- `cabal build all`
- `cabal test hx-smoke`
- manual spot checks if any

## Support matrix

- host environments:
  - Linux
  - WSL with Linux userland
- project focus:
  - Cabal v2-style workflows
  - repo-first source installs

## Known limitations

Link to `docs/known-limitations.md` and copy the release-relevant items.

## Install

```bash
cabal install exe:hx --installdir="$HOME/.local/bin" --overwrite-policy=always
```

## Feedback

Ask users to include:

- host environment
- `ghc --numeric-version`
- `cabal --numeric-version`
- `hx version`
- `hx doctor` output
- the exact command they ran
