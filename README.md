# hx

`hx` is a Cabal-first command plane for Haskell project lifecycle work.

It is not a Cabal replacement and it is not a new package registry. The first
public pre-beta focuses on making the common Haskell project loop feel like one
coherent tool:

```bash
hx init my-app --kind=cli
cd my-app
hx add text --plan
hx add text --apply
hx run
hx test
hx doctor
hx linker status
hx ci
```

The same model is meant to work in existing Cabal projects:

```bash
hx status
hx add aeson --target=exe:my-app --plan --json
hx linker use auto --plan --json
hx ci --json
```

## Why

Haskell already has strong tools: GHC, Cabal, GHCup, HLS, Hackage,
`pkg-config`, linkers, and host package managers. The problem is that day-to-day
responsibility is split across them.

`hx` exists to provide one lifecycle surface above those tools:

- create a project
- inspect the project model
- plan and apply dependency changes
- run, test, diagnose, and verify
- detect, select, and persist fast-linker settings
- expose stable machine-readable plans for AI operators

The internal product shape is:

```text
Project Snapshot -> Plan -> Apply -> Evidence
```

Humans get short commands and readable output. AI agents get `--plan`, `--json`,
side-effect summaries, and verification evidence.

## Current Surface

Implemented in this pre-beta:

- `hx init`
  creates a small Cabal project with CI-ready structure
- `hx status`
  inspects packages, components, and build dependencies
- `hx add`
  plans or applies Cabal dependency edits
- `hx doctor`
  reports host toolchain, linker, native dependency, and project signals
- `hx linker`
  inspects the active linker plan and can write an hx-managed
  `cabal.project.local` block for local fast-linker use
- `hx build`
  runs profile-aware `cabal build`
- `hx run`
  runs profile-aware `cabal run`
- `hx test`
  runs the default Cabal test flow
- `hx ci`
  runs the default build and test verification flow

Machine-readable outputs currently exist for:

```bash
hx status --json
hx doctor --json
hx linker status --json
hx linker use auto --plan --json
hx init my-app --plan --json
hx add text --plan --json
hx ci --json
hx test --json
```

The JSON schemas are intentionally small and versioned with names such as
`hx.project.v1`, `hx.add-plan.v1`, and `hx.diagnostics.v1`.

## Install

Until binary installers exist, install from a source checkout:

```bash
cabal install exe:hx --installdir="$HOME/.local/bin" --overwrite-policy=always
```

Then verify:

```bash
hx help
hx doctor
hx linker status
hx ci
```

## Fast Linkers

`hx build`, `hx run`, `hx ci`, and `hx test` share the same linker preflight.
If the current project has no explicit linker selection and `mold` or `ld.lld`
is on `PATH`, `hx` passes the matching GHC option for that invocation.
Some GHC versions print `Warning: Couldn't figure out linker information!` when
linking with `mold`; `hx` surfaces that caveat before running Cabal so it is not
mistaken for a fatal hx error when Cabal exits successfully.

Inspect the current decision:

```bash
hx linker status
```

Persist the local decision for raw Cabal commands too:

```bash
hx linker use auto --apply
```

That writes an hx-managed block to `cabal.project.local`, which should stay
local to the machine. Remove it with:

```bash
hx linker clear --apply
```

## Support Matrix

The first public pre-beta is intentionally narrow.

- primary host target: Linux
- manual target: WSL with Linux userland
- project model: Cabal v2-style projects rooted in the current working tree
- toolchain focus: GHC 9.6-era workflows first
- workflow focus: source installs and repo-first evaluation

Not claimed yet:

- Cabal replacement
- Hackage replacement
- Stack-first workflows
- native Windows support commitment
- native macOS support commitment
- production-stable toolchain manager
- binary installer distribution

## Verification

Local project verification:

```bash
cabal build all
cabal test hx-smoke
scripts/public-ci.sh
scripts/release-preflight.sh v0.1.0-pre.1
```

`scripts/public-ci.sh` exports the public artifact, audits it, and verifies the
exported tree.

## Public Operation

`hx` follows the same public/private operating pattern used for the current
flagship OSS line:

- private workshop repo: `hx-dev`
- public artifact repo: `hx`
- public contribution posture: issue-first
- license: MIT
- public claims follow executable evidence
- maintainer changes use short-lived branches and PRs
- public `main` should remain green and publishable

See:

- [STATUS.md](STATUS.md)
- [AGENTS.md](AGENTS.md)
- [CONTRIBUTING.md](CONTRIBUTING.md)
- [MAINTAINERS.md](MAINTAINERS.md)
- [docs/architecture.md](docs/architecture.md)
- [docs/dx-philosophy.md](docs/dx-philosophy.md)
- [docs/roadmap.md](docs/roadmap.md)
