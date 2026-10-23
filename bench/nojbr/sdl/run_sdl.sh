#!/usr/bin/env bash
# Throwaway (#2380, never merge). Drives SdlSpike under headless sway.
# usage: run_sdl.sh JAVA CLASSPATH OUTDIR OUTPUT_MODE SCALE [DEBUG]
set -u
JAVA="$1"; CP="$2"; OUT="$3"; MODE="$4"; SCALE="$5"; DEBUG="${6:-0}"; VPTR="${VPTR:-vptr}"
LABEL="$(basename "$OUT")"
mkdir -p "$OUT"; rm -f "$OUT"/go.* "$OUT"/*.log
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/tmp/xdg-spike}"; mkdir -p "$XDG_RUNTIME_DIR"; chmod 700 "$XDG_RUNTIME_DIR"
export WLR_BACKENDS=headless WLR_LIBINPUT_NO_DEVICES=1 WLR_RENDERER=pixman
rm -f "$XDG_RUNTIME_DIR"/wayland-* "$XDG_RUNTIME_DIR"/sway-ipc*
printf 'output HEADLESS-1 mode %s scale %s\ndefault_border none\n' "$MODE" "$SCALE" > "$OUT/sway.conf"
(sway -c "$OUT/sway.conf" > "$OUT/sway.log" 2>&1 &) ; sleep 4
export WAYLAND_DISPLAY="$(ls "$XDG_RUNTIME_DIR" | grep -E '^wayland-[0-9]+$' | head -1)"
export SWAYSOCK="$(ls "$XDG_RUNTIME_DIR"/sway-ipc*.sock | head -1)"
echo "[$LABEL] sway outputs: $(swaymsg -t get_outputs | python3 -I -c 'import sys,json; [print(o["name"], o["current_mode"]["width"], o["current_mode"]["height"], "scale", o["scale"]) for o in json.load(sys.stdin)]')"
echo "[$LABEL] globals check: $(WAYLAND_DEBUG=1 true 2>&1 | head -0)"

if [ "$DEBUG" = 1 ]; then export WAYLAND_DEBUG=1; fi
SDL_VIDEO_DRIVER=wayland "$JAVA" --enable-native-access=ALL-UNNAMED -Dstdout.encoding=UTF-8 -Dfile.encoding=UTF-8 -Djava.awt.headless=true -cp "$CP" SdlSpike "$OUT" "$SCALE" > "$OUT/app.log" 2> "$OUT/wire.log" &
APP=$!
unset WAYLAND_DEBUG

await() { # pattern [seconds]
  for i in $(seq 1 $((${2:-60} * 5))); do grep -q -E "$1" "$OUT/app.log" 2>/dev/null && return 0; kill -0 $APP 2>/dev/null || return 1; sleep 0.2; done; return 1
}
shot() { grim "$OUT/$1.png" 2>> "$OUT/grim.log"; }

if ! await '^SURFACE' 90; then echo "[$LABEL] FAIL: app never reached SURFACE"; tail -20 "$OUT/app.log"; tail -5 "$OUT/wire.log"; pkill sway; exit 1; fi
grep -E '^(JAVA|DRIVER|SCALE|SURFACE)' "$OUT/app.log" | sed "s/^/[$LABEL] /"
swaymsg -t get_tree | python3 -I -c '
import sys,json
def walk(n):
    if n.get("pid") or n.get("app_id"): print("tree node:", n.get("name"), n.get("app_id"), n.get("rect"), n.get("window_rect"))
    for c in n.get("nodes",[])+n.get("floating_nodes",[]): walk(c)
walk(json.load(sys.stdin))' | sed "s/^/[$LABEL] /"

# ---- Gate 1: present ----
for k in 1 2 3 4; do
  await "^PHASE $k " 60 || { echo "[$LABEL] FAIL: phase $k not reached"; break; }
  line="$(grep "^PHASE $k " "$OUT/app.log" | head -1)"; colour="$(echo "$line" | awk '{print $3}')"; expect="$(echo "$line" | sed -E 's/.*expect=//')"
  sleep 1.5; shot "p$k"
  n="$(python3 -I -W ignore - "$OUT/p$k.png" "$colour" <<'PY'
import sys
from PIL import Image
path, rgb = sys.argv[1], tuple(map(int, sys.argv[2].split(',')))
im = Image.open(path).convert('RGB')
print(sum(1 for px in im.getdata() if px == rgb), im.size)
PY
)"
  echo "[$LABEL] GATE1 phase $k colour=$colour expect=$expect screenshot-exact-pixels(size)=$n"
  touch "$OUT/go.$k"
done

if grep -q '^PHASE 4b' "$OUT/app.log" 2>/dev/null || await '^PHASE 4b|^TIMING' 20; then
  if grep -q '^PHASE 4b' "$OUT/app.log"; then
    sleep 1.5; shot p4b
    n="$(python3 -I -W ignore - "$OUT/p4b.png" 30,200,200 <<'PY'
import sys
from PIL import Image
path, rgb = sys.argv[1], tuple(map(int, sys.argv[2].split(',')))
im = Image.open(path).convert('RGB')
print(sum(1 for px in im.getdata() if px == rgb), im.size)
PY
)"
    echo "[$LABEL] GATE6b skia-direct-into-sdl-surface colour=30,200,200 screenshot-exact-pixels(size)=$n"
    touch "$OUT/go.41"
  fi
fi
# ---- Gate 2/6: timing + coexistence ----
await '^PHASE input-ready' 400
grep -E '^(TIMING|COEXIST)' "$OUT/app.log" | sed "s/^/[$LABEL] /"

# ---- Gate 4: input ----
await '^PHASE input-ready' 60 || echo "[$LABEL] FAIL: input-ready not reached"
# A held virtual keyboard keeps the seat's keyboard capability (and the window's focus) alive between wtype calls.
wtype -s 600000 > "$OUT/anchor.log" 2>&1 &
ANCHOR=$!; sleep 1
mkfifo "$OUT/ptr.fifo"; "$VPTR" < "$OUT/ptr.fifo" > "$OUT/vptr.log" 2>&1 &
VP=$!; exec 3> "$OUT/ptr.fifo"; sleep 1
echo "abs 500 500 1000 1000" >&3; sleep 0.5
echo "abs 520 520 1000 1000" >&3; sleep 0.4
wtype -s 1000 "abc" -s 400 -M ctrl a -m ctrl -s 400 -M shift -k Left -m shift -s 400 -P x -s 1500 -p x ; sleep 0.5
echo "click L" >&3; sleep 0.3
echo "click R" >&3; sleep 0.3
echo "scroll 3" >&3; sleep 0.3
echo "scroll -3" >&3; sleep 0.5
touch "$OUT/go.5"
await '^CURSOR' 30 || echo "[$LABEL] FAIL: cursor step not reached"
touch "$OUT/go.6"

# ---- Gate 5: clipboard ----
await '^PHASE clip-ready' 30 || echo "[$LABEL] FAIL: clip-ready not reached"
echo -n "from-wl-copy" | wl-copy ; echo -n "from-wl-primary" | wl-copy --primary ; sleep 0.7
touch "$OUT/go.7"
await '^PHASE clip-set' 30 || echo "[$LABEL] FAIL: clip-set not reached"
echo "[$LABEL] GATE5 app-read: $(grep '^CLIP get' "$OUT/app.log")"
echo "[$LABEL] GATE5 app-write: $(grep '^CLIP set' "$OUT/app.log")"
echo "[$LABEL] GATE5 wl-paste clipboard='$(timeout 5 wl-paste -n 2>&1)' primary='$(timeout 5 wl-paste -n --primary 2>&1)'"
touch "$OUT/go.8"
await '^DONE' 20
sleep 1

echo "[$LABEL] GATE4 events (key/text/mouse/clipboard):"
grep -E '^EV ' "$OUT/app.log" | grep -vE 'mouse_motion' | head -60 | sed "s/^/[$LABEL]   /"
echo "[$LABEL] GATE4 mouse_button count: $(grep -c 'mouse_button' "$OUT/app.log")"
echo "[$LABEL] GATE4 mouse_motion count: $(grep -c 'mouse_motion' "$OUT/app.log")  wheel count: $(grep -c 'mouse_wheel' "$OUT/app.log")  repeat-key count: $(grep -c 'repeat=true' "$OUT/app.log")"
if [ "$DEBUG" = 1 ]; then
  W="$OUT/wire.log"
  echo "[$LABEL] WIRE bound globals:"; grep -E 'wl_registry@[0-9]+\.bind' "$W" | sed -E 's/.*bind\(([0-9]+), "([^"]+)", ([0-9]+), new id.*/  \2 v\3/' | sort -u | sed "s/^/[$LABEL]/"
  for p in 'zwp_text_input_v3@[0-9]+\.enable' 'zwp_text_input_v3@[0-9]+\.set_cursor_rectangle' 'zwp_text_input_v3@[0-9]+\.commit' 'wp_cursor_shape_device_v1@[0-9]+\.set_shape' 'wp_viewport@[0-9]+\.set_destination' 'wp_fractional_scale_v1@[0-9]+\.preferred_scale' 'zwp_primary_selection_source_v1@[0-9]+\.offer' 'wl_data_source@[0-9]+\.offer' 'wl_surface@[0-9]+\.damage_buffer' 'wl_surface@[0-9]+\.commit' 'wl_surface@[0-9]+\.set_buffer_scale' 'xdg_toplevel_decoration_v1@[0-9]+\.set_mode' 'xdg_toplevel_decoration_v1@[0-9]+\.configure'; do
    echo "[$LABEL] WIRE count '$p': $(grep -cE "$p" "$W")"
  done
  echo "[$LABEL] WIRE set_shape calls: $(grep -E 'set_shape' "$W" | sed -E 's/.*set_shape\(([0-9]+), ([0-9]+)\)/shape=\2/' | tr '\n' ' ')"
  grep -E 'wp_fractional_scale_v1@[0-9]+\.preferred_scale|wp_cursor_shape_device_v1@[0-9]+\.set_shape|xdg_toplevel_decoration_v1@[0-9]+\.configure|xdg_toplevel_decoration_v1@[0-9]+\.set_mode' "$W" | sort | uniq -c | head -12 | sed "s/^/[$LABEL] WIRE sample /"
fi
exec 3>&-; kill $ANCHOR $VP >/dev/null 2>&1; kill $APP >/dev/null 2>&1; swaymsg exit >/dev/null 2>&1 || true; pkill sway || true; sleep 1
