#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
usage: scripts/release-preflight.sh [tag]

Runs the public pre-beta release verification flow and validates the optional
tag name against the current package version from hx.cabal.
EOF
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

if [[ "$#" -gt 1 ]]; then
  usage >&2
  exit 1
fi

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
cd "$repo_root"

package_version=$(
  sed -n 's/^version:[[:space:]]*//p' hx.cabal | head -n 1 | tr -d '[:space:]'
)

if [[ -z "$package_version" ]]; then
  echo "Could not determine package version from hx.cabal." >&2
  exit 1
fi

requested_tag="${1:-}"
suggested_tag="v${package_version}-pre.1"

validate_tag() {
  case "$1" in
    "v$package_version"|"v$package_version"-pre.*)
      return 0
      ;;
    *)
      echo "Tag '$1' does not match the release policy for package version $package_version." >&2
      echo "Expected either v$package_version or v$package_version-pre.N" >&2
      return 1
      ;;
  esac
}

run_step() {
  echo
  echo "==> $1"
  shift
  "$@"
}

if [[ -n "$requested_tag" ]]; then
  validate_tag "$requested_tag"
fi

echo "Preparing hx binary for version check..."
cabal build exe:hx >/dev/null
hx_binary=$(cabal list-bin exe:hx)
actual_binary_version=$("$hx_binary" version)
expected_binary_version="hx $package_version"

if [[ "$actual_binary_version" != "$expected_binary_version" ]]; then
  echo "Version mismatch." >&2
  echo "hx.cabal: $package_version" >&2
  echo "hx version: $actual_binary_version" >&2
  exit 1
fi

echo "Package version: $package_version"
echo "hx version: $actual_binary_version"
if [[ -n "$requested_tag" ]]; then
  echo "Requested tag: $requested_tag"
else
  echo "Suggested pre-beta tag: $suggested_tag"
fi

run_step "cabal build all" cabal build all
run_step "cabal test hx-smoke" cabal test hx-smoke
run_step "hx status --json" "$hx_binary" status --json
run_step "hx doctor --json" "$hx_binary" doctor --json
run_step "hx linker status --json" "$hx_binary" linker status --json
run_step "hx init preflight-demo --kind=cli --plan --json" "$hx_binary" init preflight-demo --kind=cli --plan --json
run_step "hx add text --plan --json" "$hx_binary" add text --plan --json
run_step "scripts/public-ci.sh" scripts/public-ci.sh
run_step "scripts/release-sdist-check.sh" scripts/release-sdist-check.sh

echo
echo "Release preflight passed."
echo "Next review documents:"
echo "  STATUS.md"
echo "  docs/release-checklist.md"
echo "  docs/release-process.md"
echo "  docs/release-notes-template.md"
echo "  docs/known-limitations.md"
