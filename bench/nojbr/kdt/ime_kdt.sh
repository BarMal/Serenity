#!/usr/bin/env bash
# Throwaway (#2380, never merge): does SDL3 on Wayland deliver pre-edit from a real input method (fcitx5 + pinyin) under sway?
# usage: APP_LAUNCH=<command using $OUT, with -Dspike.mode=ime> ime_kdt.sh OUTDIR
set -u
OUT="$1"; LABEL="$(basename "$OUT")"
trap 'kill $WATCHDOG $ANCHOR $APP $FC >/dev/null 2>&1; pkill sway >/dev/null 2>&1; pkill -x java >/dev/null 2>&1; pkill -x wtype >/dev/null 2>&1' EXIT
( sleep 300; echo "[$LABEL] WATCHDOG fired"; tail -n 40 "$OUT/app.log"; tail -n 30 "$OUT/wire.log"; tail -n 30 "$OUT/native.log" 2>&1; kill -9 $$ ) > /dev/stderr 2>&1 &
WATCHDOG=$!
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
# A held virtual keyboard gives the seat its keyboard (and the window its focus) before any text is typed.
wtype -s 600000 > "$OUT/anchor.log" 2>&1 &
ANCHOR=$!
for i in $(seq 1 100); do grep -q 'window_focus_gained' "$OUT/app.log" 2>/dev/null && break; sleep 0.2; done
echo "[$LABEL] window focused: $(grep -c window_focus_gained "$OUT/app.log")"
sleep 1
fcitx5-remote -s pinyin >/dev/null 2>&1
for i in $(seq 1 15); do fcitx5-remote -o >/dev/null 2>&1; st="$(fcitx5-remote 2>&1)"; [ "$st" = 2 ] && break; sleep 1; done
echo "[$LABEL] fcitx5-remote state after retries: $st (2 = input method active, 1 = inactive)"
sleep 1
wtype -s 500 "nihao" -s 800 -P space -p space -s 800 "shijie" -s 800 -k Return -s 500
sleep 1
touch "$OUT/go.99"; sleep 2
echo "[$LABEL] text events:"
grep -E ' (text_editing|text_input|text_editing_candidates)' "$OUT/app.log" | sed "s/^/[$LABEL]   /"
echo "[$LABEL] fcitx5 current IM: $(fcitx5-remote -n 2>&1)"; echo "[$LABEL] fcitx5 log (wayland/frontend/input-method lines):"; grep -iE "wayland|frontend|zwp|input.?method|text.?input|virtual" "$OUT/fcitx5.log" | grep -v portalsetting | head -25 | sed "s/^/[$LABEL]   /"
echo "[$LABEL] key events: $(grep -c ' key down' "$OUT/app.log")"
echo "[$LABEL] input-method wire: $(grep -cE 'zwp_text_input_v3@[0-9]+\.(preedit_string|commit_string|done)' "$OUT/wire.log")"
kill $ANCHOR $APP $FC >/dev/null 2>&1; swaymsg exit >/dev/null 2>&1 || true; pkill sway || true; kill $DBUS_SESSION_BUS_PID >/dev/null 2>&1 || true
