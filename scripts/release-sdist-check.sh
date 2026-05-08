#!/bin/sh

set -eu

usage() {
  cat <<'EOF'
usage: scripts/release-sdist-check.sh

Builds the package source distribution, unpacks it into a temporary directory,
and verifies that the unpacked artifact can build and pass hx-smoke.
EOF
}

if [ "${1:-}" = "--help" ] || [ "${1:-}" = "-h" ]; then
  usage
  exit 0
fi

if [ "$#" -ne 0 ]; then
  usage >&2
  exit 1
fi

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
cd "$repo_root"

package_name=$(
  sed -n 's/^name:[[:space:]]*//p' hx.cabal | head -n 1 | tr -d '[:space:]'
)
package_version=$(
  sed -n 's/^version:[[:space:]]*//p' hx.cabal | head -n 1 | tr -d '[:space:]'
)

if [ -z "$package_name" ] || [ -z "$package_version" ]; then
  echo "Could not determine package name or version from hx.cabal." >&2
  exit 1
fi

run_step() {
  echo
  echo "==> $1"
  shift
  "$@"
}

run_step "cabal sdist all" cabal sdist all

tarball_path="dist-newstyle/sdist/${package_name}-${package_version}.tar.gz"
if [ ! -f "$tarball_path" ]; then
  echo "Expected source tarball was not created: $tarball_path" >&2
  exit 1
fi

tmpdir=$(mktemp -d /tmp/hx-sdist-check.XXXXXX)
trap 'rm -rf "$tmpdir"' EXIT HUP INT TERM

run_step "unpack $tarball_path" tar -xzf "$tarball_path" -C "$tmpdir"

sdist_root="$tmpdir/${package_name}-${package_version}"
if [ ! -d "$sdist_root" ]; then
  echo "Expected unpacked source directory was not created: $sdist_root" >&2
  exit 1
fi

run_step "cabal build all (sdist)" sh -eu -c 'cd "$1" && cabal build all' sh "$sdist_root"
run_step "cabal test hx-smoke (sdist)" sh -eu -c 'cd "$1" && cabal test hx-smoke' sh "$sdist_root"

echo
echo "Source distribution check passed."
echo "Tarball: $repo_root/$tarball_path"
