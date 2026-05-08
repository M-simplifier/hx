#!/usr/bin/env bash

set -euo pipefail

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
export_root="$repo_root/target/public-export/hx"

rm -rf "$export_root"
mkdir -p "$export_root"

copy_path() {
  local path=$1
  if [[ -e "$repo_root/$path" ]]; then
    mkdir -p "$export_root/$(dirname "$path")"
    cp -R "$repo_root/$path" "$export_root/$path"
  fi
}

for path in \
  .github \
  app \
  docs \
  scripts \
  site \
  src \
  test \
  .gitignore \
  AGENTS.md \
  CHANGELOG.md \
  CONTRIBUTING.md \
  LICENSE \
  MAINTAINERS.md \
  README.md \
  SECURITY.md \
  STATUS.md \
  cabal.project \
  hx.cabal
do
  copy_path "$path"
done

rm -rf "$export_root/docs/private"
rm -rf "$export_root/dist-newstyle" "$export_root/target"

echo "Exported public artifact: $export_root"
