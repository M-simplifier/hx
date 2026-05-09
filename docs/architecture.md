# Architecture

The current implementation is intentionally small. It keeps existing Haskell
tools in charge and adds a lifecycle command plane above them.

## Core Model

```text
Project Snapshot -> Plan -> Apply -> Evidence
```

The product and command-surface philosophy is captured in
[DX Philosophy](dx-philosophy.md). New commands should earn their place by
reducing operational context, not by mirroring every underlying Cabal or
toolchain command.

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
- `hx linker use auto --plan`

Plans are rendered for humans and as versioned JSON for AI operators.

## Apply

Apply steps should be narrow and explainable:

- generated project files for `hx init`
- Cabal dependency edits for `hx add`
- hx-managed local linker settings in `cabal.project.local` for `hx linker`

The current dependency edit is conservative and line-oriented. This is an
acceptable pre-beta wedge, not the final Cabal formatting engine.

## Linker Plan

`hx` treats fast-linker use as an execution plan, not as hidden global state.
`hx build`, `hx run`, `hx ci`, and `hx test` share the same preflight:

- respect explicit project linker settings
- fail early when a configured linker is missing
- auto-select `mold` or `ld.lld` per invocation when no project setting exists
- expose `hx linker use ... --apply` for local persistence through
  `cabal.project.local`

## Evidence

Verification commands should be visible. Current evidence commands include:

- `hx ci`
- `hx test`
- `cabal test hx-smoke`
- `scripts/public-ci.sh`

## Legacy Prototype Surface

The first `hx` prototype produced useful `doctor`, `build`, and `run` behavior.
Those commands remain, but the architecture center has shifted from wrappers to
project lifecycle plans.
