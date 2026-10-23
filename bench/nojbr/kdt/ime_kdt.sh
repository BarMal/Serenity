#!/usr/bin/env bash
# Throwaway (#2380, never merge): does SDL3 on Wayland deliver pre-edit from a real input method (fcitx5 + pinyin) under sway?
# usage: APP_LAUNCH=<command using $OUT, with -Dspike.mode=ime> ime_kdt.sh OUTDIR
set -u
OUT="$1"; LABEL="$(basename "$OUT")"
mkdir -p "$OUT"; rm -f "$OUT"/go.* "$OUT"/*.log
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/tmp/xdg-spike}"; mkdir -p "$XDG_RUNTIME_DIR"; chmod 700 "$XDG_RUNTIME_DIR"
export WLR_BACKENDS=headless WLR_LIBINPUT_NO_DEVICES=1 WLR_RENDERER=pixman
rm -f "$XDG_RUNTIME_DIR"/wayland-* "$XDG_RUNTIME_DIR"/sway-ipc*
export HOME="$OUT/home"; mkdir -p "$HOME/.config/fcitx5"
cat > "$HOME/.config/fcitx5/profile" <<'P'
[Groups/0]
Name=Default
Default Layout=us
DefaultIM=pinyin

[Groups/0/Items/0]
Name=keyboard-us
Layout=

[Groups/0/Items/1]
Name=pinyin
Layout=

[GroupOrder]
0=Default
P
printf 'output HEADLESS-1 mode 1280x720 scale 1\ndefault_border none\n' > "$OUT/sway.conf"
eval "$(dbus-launch --sh-syntax)"
(sway -c "$OUT/sway.conf" > "$OUT/sway.log" 2>&1 &) ; sleep 4
export WAYLAND_DISPLAY="$(ls "$XDG_RUNTIME_DIR" | grep -E '^wayland-[0-9]+$' | head -1)"
export SWAYSOCK="$(ls "$XDG_RUNTIME_DIR"/sway-ipc*.sock | head -1)"
fcitx5 -D --verbose '*=3' > "$OUT/fcitx5.log" 2>&1 &
FC=$!; sleep 4
echo "[$LABEL] fcitx5 running: $(kill -0 $FC 2>/dev/null && echo yes || echo NO); remote: $(fcitx5-remote 2>&1)"
eval "$APP_LAUNCH" > "$OUT/app.log" 2> "$OUT/wire.log" &
APP=$!
for i in $(seq 1 300); do grep -q '^PHASE ime-ready' "$OUT/app.log" 2>/dev/null && break; sleep 0.2; done
wtype -s 800 -M shift -m shift > /dev/null 2>&1 &   # create the virtual keyboard, give focus time to settle
wtype -s 600 -M shift -m shift ; sleep 1
echo "[$LABEL] fcitx5-remote -s pinyin: $(fcitx5-remote -s pinyin 2>&1); -o: $(fcitx5-remote -o 2>&1); state now: $(fcitx5-remote 2>&1)"
sleep 1
wtype -s 500 "nihao" -s 800 -P space -p space -s 800 "shijie" -s 800 -k Return -s 500
sleep 1
touch "$OUT/go.99"; sleep 2
echo "[$LABEL] text events:"
grep -E ' (text_editing|text_input|text_editing_candidates)' "$OUT/app.log" | sed "s/^/[$LABEL]   /"
echo "[$LABEL] key events: $(grep -c ' key down' "$OUT/app.log")"
echo "[$LABEL] input-method wire: $(grep -cE 'zwp_text_input_v3@[0-9]+\.(preedit_string|commit_string|done)' "$OUT/wire.log")"
kill $APP $FC >/dev/null 2>&1; swaymsg exit >/dev/null 2>&1 || true; pkill sway || true; kill $DBUS_SESSION_BUS_PID >/dev/null 2>&1 || true
