# Sourced by train-aot-cache.sh and check-aot-cache.sh. Given an app image or bundle in "$image", sets "launcher" and
# "appdir" for the Linux layout (bin/Serenity, lib/app), the Windows layout (Serenity.exe, app) or a macOS bundle
# (Contents/MacOS/Serenity, Contents/app), and "assemble_in_two_steps" to 1 for the bundle; fails for anything else.
# Also defines run_limited.
assemble_in_two_steps=0
if [[ -x "$image/bin/Serenity" && -d "$image/lib/app" ]]; then
  launcher="$image/bin/Serenity"
  appdir="$image/lib/app"
elif [[ -x "$image/Serenity.exe" && -d "$image/app" ]]; then
  launcher="$image/Serenity.exe"
  appdir="$image/app"
elif [[ -x "$image/Contents/MacOS/Serenity" && -d "$image/Contents/app" ]]; then
  launcher="$image/Contents/MacOS/Serenity"
  appdir="$image/Contents/app"
  # The bundle's runtime has no bin/java, which -XX:AOTCacheOutput forks to assemble the cache (#2522 comment 6625934280).
  assemble_in_two_steps=1
else
  echo "Not an app image: $image" >&2
  exit 1
fi

# macOS has no timeout(1). Runs the command, killing it after $1 seconds and returning 124, else returning its status.
run_limited() {
  local limit_ticks=$(( $1 * 10 )) pid ticks=0 status=0
  shift
  "$@" &
  pid=$!
  while kill -0 "$pid" 2>/dev/null; do
    if (( ticks >= limit_ticks )); then
      kill -9 "$pid" 2>/dev/null || true
      wait "$pid" 2>/dev/null || true
      return 124
    fi
    sleep 0.1
    ticks=$((ticks + 1))
  done
  wait "$pid" || status=$?
  return "$status"
}
