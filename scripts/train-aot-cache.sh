#!/usr/bin/env bash
# Writes the JDK 25 AOT cache of a Linux app image and points its launcher at it: scripts/train-aot-cache.sh
# dist/Serenity [timeout-seconds]. One --smoke-test run records what start-up loads and links, and the cache is created
# as that run exits. The launcher option is added only afterwards because the JVM refuses -XX:AOTCache for a file that
# does not exist yet. Run it under xvfb-run on a headless box; scripts/check-aot-cache.sh then verifies the result.
set -euo pipefail

image="${1:?usage: $0 path/to/app-image [timeout-seconds]}"
limit="${2:-120}"
launcher="$image/bin/Serenity"
appdir="$image/lib/app"
cfg="$appdir/Serenity.cfg"
cache="$appdir/serenity.aot"
option='java-options=-XX:AOTCache=$APPDIR/serenity.aot'

if [[ ! -x "$launcher" || ! -f "$cfg" ]]; then
  echo "Not a Linux app image: $image" >&2
  exit 1
fi
if grep -q 'AOTCache' "$cfg"; then
  echo "$cfg already names an AOT cache; train a freshly built image." >&2
  exit 1
fi

work_dir="$(mktemp -d)"
trap 'rm -rf "$work_dir"' EXIT
status=0
(cd "$work_dir" && JAVA_TOOL_OPTIONS="-XX:AOTCacheOutput=$cache" timeout "$limit" "$launcher" --smoke-test --safe-mode) \
  >"$work_dir/training.log" 2>&1 || status=$?

if (( status != 0 )); then
  cat "$work_dir/training.log" >&2
  echo "The training run exited with status $status." >&2
  exit 1
fi
if [[ ! -s "$cache" ]]; then
  cat "$work_dir/training.log" >&2
  echo "The training run did not write $cache." >&2
  exit 1
fi

tmp_cfg="$work_dir/Serenity.cfg"
awk -v option="$option" '{ print } /^\[JavaOptions\]$/ { print option }' "$cfg" >"$tmp_cfg"
cat "$tmp_cfg" >"$cfg"

echo "AOT cache written: $(du -h "$cache" | cut -f1) at $cache"
