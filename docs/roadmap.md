# Roadmap

## M0: Lifecycle Wedge

Status: implemented for public pre-beta.

Goal: prove that `hx` can make a Haskell project lifecycle feel like one
command plane without pretending to replace Cabal.

Included:

- `hx init`
- `hx status`
- `hx add`
- `hx run`
- `hx test`
- `hx doctor`
- `hx ci`
- JSON plans and evidence output for AI operators

## M1: Project Mutation Trust

Goal: make mutating commands trustworthy across more Cabal project shapes.

Likely work:

- richer Cabal formatting preservation
- package-qualified targets in multi-package workspaces
- dependency bounds policy
- better duplicate and stanza handling
- more fixture-backed mutation tests

## M2: Existing Project Adoption

Goal: make `hx` useful in real existing Haskell projects without forcing
project layout changes.

Likely work:

- `hx adopt` or equivalent adoption report
- native dependency guidance from actual field projects
- linker and runtime profile recommendations as plans
- workspace-aware status summaries

## M3: Toolchain And Editor Plane

Goal: coordinate GHCup, HLS, Cabal, and editor-facing needs without becoming a
fragile global toolchain manager.

Likely work:

- recommended toolchain plans
- HLS compatibility checks
- project-local toolchain notes
- non-mutating setup plans before apply

## M4: Release And Distribution

Goal: move beyond repo-first source installs once the command contracts are
stable.

Likely work:

- binary installer research
- signed release artifacts
- broader Linux coverage
- macOS and native Windows decisions

## Claim Discipline

Do not advance the public claim from pre-beta command plane to mature package
manager until independent project adoption and broader project-shape evidence
exist.
