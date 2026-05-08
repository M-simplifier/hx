# Architecture

The current implementation is intentionally small. It keeps existing Haskell
tools in charge and adds a lifecycle command plane above them.

## Core Model

```text
Project Snapshot -> Plan -> Apply -> Evidence
```

## Project Snapshot

`Hx.Project` walks the current working tree, finds Cabal files, and extracts:

- package files
- package names and versions
- components
- component targets
- `build-depends` names

This is a lightweight reader, not a full Cabal parser. It is sufficient for the
current lifecycle wedge and should be replaced or deepened only when real
project shapes demand it.

## Plans

Mutating commands should produce explicit plans before changing files.

Current examples:

- `hx init <name> --plan`
- `hx add <package> --plan`

Plans are rendered for humans and as versioned JSON for AI operators.

## Apply

Apply steps should be narrow and explainable:

- generated project files for `hx init`
- Cabal dependency edits for `hx add`

The current dependency edit is conservative and line-oriented. This is an
acceptable pre-beta wedge, not the final Cabal formatting engine.

## Evidence

Verification commands should be visible. Current evidence commands include:

- `cabal build all`
- `cabal test all`
- `cabal test hx-smoke`
- `scripts/public-ci.sh`

## Legacy Prototype Surface

The first `hx` prototype produced useful `doctor`, `build`, and `run` behavior.
Those commands remain, but the architecture center has shifted from wrappers to
project lifecycle plans.
