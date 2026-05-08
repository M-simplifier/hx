#!/usr/bin/env bash

set -euo pipefail

target=${1:-.}

if [[ ! -d "$target" ]]; then
  echo "Audit target is not a directory: $target" >&2
  exit 1
fi

if [[ -e "$target/docs/private" ]]; then
  echo "Public export includes docs/private." >&2
  exit 1
fi

home_path='/''home/masaya'
workspace_path='free''-exp'
token_prefixes='gh[pousr]_|github_''pat_|AK''IA|xox''[baprs]-'
private_key='BEGIN [A-Z ]*PRIVATE KEY'
deny_terms="(${home_path}|${workspace_path}|${token_prefixes}|${private_key})"

matches=$(grep -RInE "$deny_terms" "$target" \
  --exclude-dir=.git \
  --exclude-dir=dist-newstyle \
  --exclude-dir=target \
  --exclude='*.tar.gz' || true)

if [[ -n "$matches" ]]; then
  echo "Public audit found denied content:" >&2
  echo "$matches" >&2
  exit 1
fi

echo "Public audit passed: $target"
