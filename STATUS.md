# Status

`hx` is a public pre-beta candidate.

## Implemented

- Cabal-first project snapshot model
- `hx init` for `cli`, `library`, and `service` project shapes
- `hx add` dependency planning and application for Cabal components
- `hx status` project inspection
- `hx doctor` host and project diagnostics
- `hx build`, `hx run`, `hx test`, and `hx ci`
- JSON output for project status, diagnostics, init plans, add plans, and CI/test
  evidence
- Smoke tests covering the core CLI, dry-run orchestration, dependency planning,
  dependency application, and preflight blockers
- Public export, public audit, and public CI scripts
- Static official site under `site/`

## Verified Locally

Current verification commands:

```bash
cabal build all
cabal test hx-smoke
scripts/public-ci.sh
scripts/release-preflight.sh v0.1.0-pre.1
```

## Current Claim Boundary

`hx` is a Cabal-first command plane for Haskell lifecycle work:

- project creation
- dependency changes
- run/test/doctor/ci
- AI-readable operation plans

## Not Claimed

- replacing Cabal
- replacing Hackage
- full package-manager parity with Cargo or Go
- production-stable toolchain management
- Stack-first workflows
- binary installers
- native Windows or macOS support commitments

## Known Local Environment

The maintainer development machine currently verifies on WSL Ubuntu with:

- GHC 9.6.7
- cabal 3.12.1.0
- GHCup present
- GNU `ld` present
- `pkg-config`, `mold`, and `ld.lld` absent

The missing optional tools are expected to appear as warnings for this repo, not
as blockers.
