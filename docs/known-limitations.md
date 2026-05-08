# Known Limitations

This document tracks limitations that public pre-beta users should expect
today.

## Platform

- Linux is the primary automated target today
- WSL is a supported target in principle, but still relies on manual
  verification rather than CI
- Native macOS and native Windows are not public pre-beta commitments yet
- Pre-beta installs are currently source-oriented; there are no binary installers
  or platform-specific packaged releases yet

## Project model

- The current focus is Cabal v2-style workflows in the current working tree
- `hx add` currently performs conservative line-oriented Cabal edits rather
  than full format-preserving Cabal AST rewriting
- Multi-package workspaces may require explicit targets and still need deeper
  fixture coverage
- Stack-first workflows are not a first-class target yet
- Reproducibility features such as toolchain pinning and environment sync are
  intentionally out of scope for the first pre-beta

## Runtime and build orchestration

- `hx build` and `hx run` are still thin wrappers around `cabal`
- Linux CI covers smoke-level regressions, not exhaustive build matrices
- Some advanced Cabal shapes may still need explicit user guidance when
  multiple runnable components or custom native toolchains are involved

## Native dependencies and linkers

- `hx doctor` / `hx build` provide actionable `pkg-config` and linker guidance,
  but package-name heuristics are still curated rather than exhaustive
- Host-specific linker and native-library edge cases beyond the current fixture
  set may still appear in real projects
