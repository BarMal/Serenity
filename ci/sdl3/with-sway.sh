#!/usr/bin/env bash
# Runs a command with WAYLAND_DISPLAY pointing at a headless sway (software rendering), then stops sway (#2380).
# usage: ci/sdl3/with-sway.sh COMMAND [ARGS...]     needs: sway
set -u
work="$(mktemp -d)"
export XDG_RUNTIME_DIR="$work/run"
mkdir -p "$XDG_RUNTIME_DIR" && chmod 700 "$XDG_RUNTIME_DIR"
export WLR_BACKENDS=headless WLR_LIBINPUT_NO_DEVICES=1 WLR_RENDERER=pixman LIBGL_ALWAYS_SOFTWARE=1
printf 'output HEADLESS-1 mode 1920x1080 scale 1\ndefault_border none\n' > "$work/sway.conf"
sway -c "$work/sway.conf" > "$work/sway.log" 2>&1 &
sway_pid=$!
for _ in $(seq 1 50); do
  socket="$(ls "$XDG_RUNTIME_DIR" 2>/dev/null | grep -E '^wayland-[0-9]+$' | head -1 || true)"
  [ -n "$socket" ] && break
  sleep 0.2
done
if [ -z "${socket:-}" ]; then
  echo "sway did not start" >&2; cat "$work/sway.log" >&2; kill "$sway_pid" 2>/dev/null; exit 1
fi
export WAYLAND_DISPLAY="$socket"
"$@"; status=$?
kill "$sway_pid" 2>/dev/null; wait "$sway_pid" 2>/dev/null
exit "$status"
