#!/usr/bin/env bash
# Launches a packaged Serenity app image with --smoke-test and checks that it starts, paints a first frame and exits
# cleanly. Usable locally: scripts/smoke-test-app-image.sh dist/Serenity/bin/Serenity (under xvfb-run on a headless
# Linux box). Runs in safe mode, so it reads and writes nothing in the user's own configuration or session.
#
# Fails when the launcher is missing, exits non-zero, never prints the ready line, prints an uncaught exception, leaves
# a JVM crash log (hs_err_pid*.log), or does not finish within the time limit.
set -euo pipefail

launcher="${1:?usage: $0 path/to/launcher [timeout-seconds]}"
limit="${2:-90}"
ready_line="SERENITY_SMOKE_READY"

if [[ ! -x "$launcher" ]]; then
  echo "Launcher is missing or not executable: $launcher" >&2
  exit 1
fi
launcher="$(cd "$(dirname "$launcher")" && pwd)/$(basename "$launcher")"

work_dir="$(mktemp -d)"
log="$work_dir/output.log"
pid=""
cleanup() {
  if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then
    kill -9 "$pid" 2>/dev/null || true
  fi
  rm -rf "$work_dir"
}
trap cleanup EXIT

fail() {
  echo "$1" >&2
  cat "$log" >&2
  exit 1
}

# The working directory is where a crashing JVM leaves hs_err_pid*.log.
(cd "$work_dir" && exec "$launcher" --smoke-test --safe-mode) >"$log" 2>&1 &
pid=$!

elapsed=0
while kill -0 "$pid" 2>/dev/null; do
  if (( elapsed >= limit )); then
    fail "Smoke test timed out after ${limit}s; the app image did not finish starting and quitting."
  fi
  sleep 1
  elapsed=$((elapsed + 1))
done

status=0
wait "$pid" || status=$?
pid=""

if (( status != 0 )); then
  fail "App image exited with status $status."
fi
if ! grep -Fxq "$ready_line" "$log"; then
  fail "App image exited without printing $ready_line."
fi
if grep -Eq '^Exception in thread|^Caused by:' "$log"; then
  fail "App image printed an uncaught exception."
fi
if compgen -G "$work_dir/hs_err_pid*.log" >/dev/null; then
  cat "$work_dir"/hs_err_pid*.log >&2
  fail "App image left a JVM crash log."
fi

echo "Smoke test passed in ${elapsed}s: $launcher"
