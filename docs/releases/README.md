# Release Drafts

This directory holds concrete release-note drafts generated from the release
template and reviewed before public tags.

Recommended workflow:

1. Run `scripts/public-ci.sh`
2. Run `scripts/release-preflight.sh <tag>`
3. Run `scripts/create-release-draft.sh <tag>` when a fresh draft is needed
4. Edit the generated draft with the actual change summary and limitations
5. Use that draft when publishing the GitHub release
