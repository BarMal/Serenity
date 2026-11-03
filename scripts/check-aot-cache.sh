#!/usr/bin/env bash
# Checks the AOT cache of a trained Linux app image: scripts/check-aot-cache.sh dist/Serenity [timeout-seconds].
# A valid cache must be opened without an [error][aot] line. A missing or stale cache (the jar changed after training,
# which the JVM rejects for good) must still leave an app that starts, paints and exits cleanly, because a package that
# only starts with its cache would be a regression from the AppCDS archive it replaces. The image is put back as found.
set -euo pipefail

image="${1:?usage: $0 path/to/app-image [timeout-seconds]}"
limit="${2:-120}"
image="$(cd "$image" 2>/dev/null && pwd)" || { echo "No such app image: ${1}" >&2; exit 1; }
launcher="$image/bin/Serenity"
appdir="$image/lib/app"
cache="$appdir/serenity.aot"
jar="$appdir/Serenity.jar"
ready_line="SERENITY_SMOKE_READY"

if [[ ! -f "$cache" ]]; then
  echo "No AOT cache at $cache; run scripts/train-aot-cache.sh first." >&2
  exit 1
fi

work_dir="$(mktemp -d)"
log="$work_dir/output.log"
jar_time="$work_dir/jar-time"
touch -r "$jar" "$jar_time"
restore() {
  [[ -f "$work_dir/serenity.aot.hidden" ]] && mv "$work_dir/serenity.aot.hidden" "$cache"
  touch -r "$jar_time" "$jar"
  rm -rf "$work_dir"
}
trap restore EXIT

launch() {
  local status=0
  (cd "$work_dir" && JAVA_TOOL_OPTIONS="${1:-}" timeout "$limit" "$launcher" --smoke-test --safe-mode) >"$log" 2>&1 || status=$?
  return "$status"
}

fail() {
  cat "$log" >&2
  echo "$1" >&2
  exit 1
}

starts_cleanly() {
  launch "" || fail "A $1 AOT cache stopped the app image starting (exit status $?)."
  grep -Fxq "$ready_line" "$log" || fail "A $1 AOT cache left the app image without printing $ready_line."
}

launch "-Xlog:aot" || fail "The app image failed with its valid AOT cache."
grep -q 'Opened AOT cache' "$log" || fail "The valid AOT cache was not used: no 'Opened AOT cache' line."
if grep -Fq '[error][aot]' "$log"; then
  fail "The valid AOT cache logged an [error][aot] line."
fi

mv "$cache" "$work_dir/serenity.aot.hidden"
starts_cleanly missing
mv "$work_dir/serenity.aot.hidden" "$cache"

touch -d 'now + 1 hour' "$jar"
starts_cleanly stale

echo "AOT cache checks passed: valid cache opened, missing and stale caches start cleanly."
