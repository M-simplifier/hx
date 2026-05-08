# Maintainer Workflow

`main` should remain publishable: green, explainable, and safe to show.

## Default Flow

- Manage owner-facing work as explicit Goals.
- Use short-lived branches for non-trivial changes.
- Open a pull request before merging to `main`.
- Use squash merge.
- Delete merged branches.
- Keep direct `main` pushes for urgent publication, repo, or CI repair only.

## Pull Request Gate

Every pull request should state:

- what changed
- which owner-facing Goal or public-boundary cleanup it supports
- whether public claims changed
- which checks ran
- whether public files are safe to publish

## Required Checks

Local checks:

```bash
cabal build all
cabal test hx-smoke
scripts/public-ci.sh
```

Public CI should run equivalent checks on pull requests and pushes to `main`.

## Separate Review Loop

For substantial implementation or publication changes:

1. Implement on a short-lived branch.
2. Open a pull request.
3. Review from a separate context.
4. Fix blockers and rerun checks.
5. Squash-merge when green.
