#!/usr/bin/env bash
# Throwaway (#2380, never merge). usage: run_gl.sh JAVA CP OUTDIR PROFILE SWAP MODE SCALE
set -u
JAVA="$1"; CP="$2"; OUT="$3"; PROFILE="$4"; SWAP="$5"; MODE="${6:-1920x1080}"; SCALE="${7:-1}"
LABEL="$(basename "$OUT")"; mkdir -p "$OUT"; rm -f "$OUT"/*.log "$OUT"/*.png
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/tmp/xdg-glspike}"; mkdir -p "$XDG_RUNTIME_DIR"; chmod 700 "$XDG_RUNTIME_DIR"
export WLR_BACKENDS=headless WLR_LIBINPUT_NO_DEVICES=1 WLR_RENDERER=pixman LIBGL_ALWAYS_SOFTWARE=1
rm -f "$XDG_RUNTIME_DIR"/wayland-* "$XDG_RUNTIME_DIR"/sway-ipc*
printf 'output HEADLESS-1 mode %s scale %s\ndefault_border none\n' "$MODE" "$SCALE" > "$OUT/sway.conf"
(sway -c "$OUT/sway.conf" > "$OUT/sway.log" 2>&1 &); sleep 4
export WAYLAND_DISPLAY="$(ls "$XDG_RUNTIME_DIR" | grep -E '^wayland-[0-9]+$' | head -1)"
unset DISPLAY
SDL_VIDEO_DRIVER=wayland "$JAVA" --enable-native-access=ALL-UNNAMED -Djava.awt.headless=true -cp "$CP" GlSpike "$OUT" "$PROFILE" "$SWAP" "${SHIM:--}" ${MODE5:-} > "$OUT/app.log" 2> "$OUT/err.log" &
APP=$!
for i in $(seq 1 600); do grep -qE '^(PHASE hold|FAIL)' "$OUT/app.log" && break; kill -0 $APP 2>/dev/null || break; sleep 0.2; done
if grep -q '^PHASE hold' "$OUT/app.log"; then
  sleep 1.5; grim "$OUT/hold.png" 2>>"$OUT/grim.log"
  python3 -I -W ignore - "$OUT/hold.png" <<'PY' | sed "s/^/[$LABEL] /"
import sys
from PIL import Image
im = Image.open(sys.argv[1]).convert('RGB'); px = list(im.getdata())
bg = sum(1 for p in px if p == (0x1e,0x1e,0x24)); ink = sum(1 for p in px if p[0] > 0x90 and abs(p[0]-p[1]) < 8)
print("SCREENSHOT", im.size, "background-exact", bg, "light-glyph-pixels", ink)
PY
fi
wait $APP 2>/dev/null
sed "s/^/[$LABEL] /" "$OUT/app.log"; grep -v '^Picked' "$OUT/err.log" | head -15 | sed "s/^/[$LABEL] stderr: /"
swaymsg exit >/dev/null 2>&1 || true; pkill sway || true; sleep 1
