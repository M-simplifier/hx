# AI Operator Guide

This file is public, repo-local guidance for AI agents working on `hx`.

## Project Contract

`hx` is a Cabal-first command plane. Keep public behavior aligned with:

```text
Project Snapshot -> Plan -> Apply -> Evidence
```

Do not widen public claims to Cabal replacement, Hackage replacement, or
production-stable toolchain management unless matching evidence exists.

## Default Commands

Use these before proposing that a change is ready:

```bash
cabal build all
cabal test hx-smoke
scripts/public-ci.sh
```

For release checks:

```bash
scripts/release-preflight.sh v0.1.0-pre.1
```

## Public Safety

- Keep generated output out of git.
- Keep local paths, private notes, credentials, and unpublished claims out of
  public files.
- Use `scripts/public-export.sh` and `scripts/public-audit.sh` before public
  publication work.
- Treat `docs/private/` as workshop-only material; it must not appear in the
  public export.

## Coding Shape

- Prefer small explicit modules over hidden global behavior.
- Keep plan rendering separate from file mutation.
- Every mutating command should expose the intended side effects before applying
  them.
- Machine-readable output should use stable schema names and avoid prose-only
  contracts.

## Public Claim Discipline

When changing README, STATUS, site copy, or release notes, ask:

- Is this implemented?
- Is it verified?
- Does a public user need to know it now?
- Would this sound like a mature guarantee rather than a pre-beta claim?
