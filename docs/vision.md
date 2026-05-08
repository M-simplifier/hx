# Vision

`hx` exists to make Haskell project work feel like one coherent command plane.

## Thesis

Haskell does not need `hx` to replace Cabal, GHC, GHCup, Hackage, or HLS.
Haskell needs a stronger operational surface above them.

The product model is:

```text
Project Snapshot -> Plan -> Apply -> Evidence
```

That model should serve both humans and AI operators:

- humans get short commands and readable summaries
- AI operators get stable JSON, side-effect lists, plan/apply boundaries, and
  verification evidence

## Desired Experience

New project:

```bash
hx init my-app --kind=cli
cd my-app
hx add text --apply
hx run
hx test
hx ci
```

Existing project:

```bash
hx status
hx add aeson --target=exe:server --plan --json
hx doctor --json
hx ci --json
```

## Public Pre-Beta Boundary

The first public version claims only a Cabal-first lifecycle wedge:

- initialize small Cabal projects
- inspect Cabal packages and components
- plan/apply simple dependency edits
- run build/test/doctor/ci flows
- expose AI-readable plans and diagnostics

It does not claim package registry replacement, Cabal replacement, Stack-first
support, or production-grade toolchain management.
