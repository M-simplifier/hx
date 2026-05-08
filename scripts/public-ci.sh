#!/usr/bin/env bash

set -euo pipefail

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
export_root="$repo_root/target/public-export/hx"

cd "$repo_root"

echo "==> cabal build all"
cabal build all

echo
echo "==> cabal test hx-smoke"
cabal test hx-smoke

echo
echo "==> scripts/public-export.sh"
scripts/public-export.sh

echo
echo "==> scripts/public-audit.sh target/public-export/hx"
scripts/public-audit.sh "$export_root"

echo
echo "==> exported cabal build all"
(cd "$export_root" && cabal build all)

echo
echo "==> exported cabal test hx-smoke"
(cd "$export_root" && cabal test hx-smoke)

echo
echo "==> clean generated output from export"
rm -rf "$export_root/dist-newstyle"

echo
echo "==> final public audit"
scripts/public-audit.sh "$export_root"

echo
echo "Public CI passed."
