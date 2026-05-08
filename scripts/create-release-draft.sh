#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
usage: scripts/create-release-draft.sh <tag>

Creates a release-note draft under docs/releases/ for the provided pre-beta tag.
EOF
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

if [[ "$#" -ne 1 ]]; then
  usage >&2
  exit 1
fi

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
cd "$repo_root"

tag_name=$1
package_version=$(
  sed -n 's/^version:[[:space:]]*//p' hx.cabal | head -n 1 | tr -d '[:space:]'
)

case "$tag_name" in
  "v$package_version"|"v$package_version"-pre.*)
    ;;
  *)
    echo "Tag '$tag_name' does not match package version $package_version." >&2
    echo "Expected either v$package_version or v$package_version-pre.N" >&2
    exit 1
    ;;
esac

mkdir -p docs/releases
output_path="docs/releases/$tag_name.md"

if [[ -e "$output_path" ]]; then
  echo "Release draft already exists: $output_path" >&2
  exit 1
fi

today=$(date +%F)

cat >"$output_path" <<EOF
# $tag_name

Draft date: $today

## Summary

This release is a public pre-beta focused on Cabal-first Haskell lifecycle
workflows.

## Included in this release

- \`hx init\`
- \`hx status\`
- \`hx add\`
- \`hx doctor\`
- \`hx build\`
- \`hx run\`
- \`hx test\`
- \`hx ci\`

## What improved since the previous tag

- fill in the headline improvements here

## Verification

- \`scripts/public-ci.sh\`
- \`scripts/release-preflight.sh $tag_name\`

## Known limitations

Review and copy the release-relevant items from \`docs/known-limitations.md\`.

## Install

\`\`\`bash
cabal install exe:hx --installdir="\$HOME/.local/bin" --overwrite-policy=always
\`\`\`

## Feedback

When reporting a problem, include host environment, toolchain versions,
\`hx version\`, \`hx doctor\` output, and the exact command.
EOF

echo "Created release draft: $output_path"
