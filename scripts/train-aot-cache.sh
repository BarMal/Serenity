#!/usr/bin/env bash
# Writes the JDK 25 AOT cache of a Linux app image or macOS bundle and points its launcher at it: scripts/train-aot-cache.sh
# dist/Serenity [timeout-seconds]. A --smoke-test run records what start-up loads and links, and the cache is created as
# that run exits (-XX:AOTCacheOutput). A macOS bundle's runtime has no bin/java for that option to fork, so the bundle is
# recorded and then assembled in two launcher runs. The launcher option is added only afterwards because the JVM refuses
# -XX:AOTCache for a file that does not exist yet. Train a bundle before signing it. Run under xvfb-run on a headless Linux
# box; scripts/check-aot-cache.sh then verifies the result.
set -euo pipefail

image="${1:?usage: $0 path/to/app-image [timeout-seconds]}"
limit="${2:-120}"
image="$(cd "$image" 2>/dev/null && pwd)" || { echo "No such app image: ${1}" >&2; exit 1; }
source "$(dirname "${BASH_SOURCE[0]}")/aot-image-layout.sh"
cfg="$appdir/Serenity.cfg"
cache="$appdir/serenity.aot"
recording="$appdir/serenity.aotconf"
option='java-options=-XX:AOTCache=$APPDIR/serenity.aot'

if [[ ! -f "$cfg" ]]; then
  echo "Not an app image: $image has no $cfg" >&2
  exit 1
fi
if grep -q 'AOTCache' "$cfg"; then
  echo "$cfg already names an AOT cache; train a freshly built image." >&2
  exit 1
fi

work_dir="$(mktemp -d)"
trap 'rm -rf "$work_dir" "$recording"' EXIT

# Runs the launcher once with the given JVM options; reports how it ended, and fails when it did not exit cleanly.
training_run() {
  local name="$1" options="$2" status=0
  (cd "$work_dir" && export JAVA_TOOL_OPTIONS="$options" && run_limited "$limit" "$launcher" --smoke-test --safe-mode) \
    >"$work_dir/$name.log" 2>&1 || status=$?
  if (( status == 124 )); then
    cat "$work_dir/$name.log" >&2
    echo "The training run timed out after ${limit}s." >&2
    exit 1
  fi
  if (( status != 0 )); then
    cat "$work_dir/$name.log" >&2
    echo "The training run exited with status $status." >&2
    exit 1
  fi
}

if (( assemble_in_two_steps )); then
  training_run record "-XX:AOTMode=record -XX:AOTConfiguration=$recording"
  training_run create "-XX:AOTMode=create -XX:AOTConfiguration=$recording -XX:AOTCache=$cache"
else
  training_run record "-XX:AOTCacheOutput=$cache"
fi

if [[ ! -s "$cache" ]]; then
  cat "$work_dir"/*.log >&2
  echo "The training run did not write $cache." >&2
  exit 1
fi

tmp_cfg="$work_dir/Serenity.cfg"
awk -v option="$option" '{ print } /^\[JavaOptions\]$/ { print option }' "$cfg" >"$tmp_cfg"
cat "$tmp_cfg" >"$cfg"

echo "AOT cache written: $(du -h "$cache" | cut -f1) at $cache"
