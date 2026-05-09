# DX Philosophy

`hx` should not become a bag of Cabal wrappers. Its job is to reduce the amount
of operational context a human or AI operator must keep in working memory while
developing Haskell projects.

The design center is:

```text
Project Snapshot -> Plan -> Apply -> Evidence
```

This is both a human UX model and an AI cooperation model. Humans get short
commands with readable intent. AI operators get stable JSON, explicit side
effects, and verification evidence.

## Product Aesthetic

`hx` should feel like one coherent command plane above Cabal, GHC, GHCup, HLS,
native dependency tools, and linkers. It should keep those tools in charge, but
absorb the routine decisions and confusing failure modes around them.

The standard is not "add every useful command." The standard is "make the next
right action obvious."

## Command Design Rules

Add a top-level command only when it represents a daily development intent:

- create or adopt a project
- inspect the project
- plan or apply a narrow mutation
- build, run, test, or enter the project
- diagnose the environment
- produce verification evidence

Do not add a top-level command merely because Cabal or another tool has a
matching command. If the operation is a detail, keep it as a flag, a subcommand
under an existing namespace, a doctor/adopt recommendation, or a documented
fallback to the underlying tool.

## Mutation Rules

Any operation that changes source, config, or local project state should have:

- a human-readable plan
- machine-readable JSON when useful for AI operators
- a clear `--apply` boundary unless the command is already an execution command
- a side-effect summary
- a way to avoid hiding important local state

Build-cache cleanup is allowed to be more direct than source mutation, but it
should still explain what will be removed before destructive actions when a
dry-run is requested.

## Evidence Rules

Commands that execute work should report:

- profile or mode
- selected target
- linker/toolchain decision
- warnings with actionable next steps
- exact underlying command where useful
- exit status, and eventually elapsed time in JSON evidence

Warnings should be treated as diagnostics, not prose. A good warning says what
happened, whether it is fatal, why it matters, and what to do next.

## Human And AI Cooperation

The interface should be pleasant for humans and more powerful for AI. This does
not mean hiding reality from humans or dumping raw tool output into JSON.

For every significant feature, ask:

- What is the shortest human command that expresses the intent?
- What stable structure does an AI need to reason about the result?
- What side effects must be explicit?
- What should the next action be after success or failure?

## Strong Additions

These fit the current philosophy:

- `hx clean`
  - remove Cabal build artifacts that `hx` can explain
  - support `--dry-run` and JSON
  - never delete source or user-authored project config
- `hx build --fresh`
  - clean the build artifacts relevant to the selected build, then build
  - avoid adding `hx rebuild` as a separate top-level command until needed
- `hx build --json` and `hx run --json`
  - report command, profile, target, linker plan, warnings, exit status, and
    eventually elapsed time
- `hx repl`
  - Haskell's REPL is a first-class daily workflow
  - should reuse target resolution, preflight, and diagnostics
- `hx adopt`
  - produce a non-mutating adoption report for existing projects
  - summarize project shape, runnable/test targets, native dependencies,
    linker posture, and CI readiness
- `hx deps ...`
  - collect dependency exploration under one namespace instead of spreading
    `update`, `outdated`, `why`, and `tree` across top-level help
- diagnostic IDs and repair plans
  - make doctor/build warnings stable enough for AI and issue reports
  - prefer `--fix-plan` before automatic fixes

## Add Later Or Keep Out

These may become useful, but should not be rushed into top-level help:

- `hx fmt` and `hx lint`
  - wait until project-local formatter/linter detection is trustworthy
- `hx publish`
  - too much release and registry responsibility for the current pre-beta
- `hx install`
  - should wait for a real binary distribution strategy
- `hx watch`
  - useful only after build/test/repl contracts settle
- `hx hls`
  - likely belongs first in doctor/adopt/toolchain plans rather than as a
    standalone top-level surface

## Next Product Bet

The next coherent DX goal is:

```text
Build Hygiene and Evidence Loop
```

That means `hx clean`, `hx build --fresh`, and JSON evidence for build/run. The
goal is not more command surface. The goal is to make "rebuild this project
with the right linker and tell me what happened" a single obvious experience for
both humans and AI operators.
