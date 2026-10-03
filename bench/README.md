# Laptop benchmark

The app-level benchmark behind the A/B/C comparisons in #1798 and #1812. It must run on the target laptop (Intel UHD 620, Hyprland/XWayland, 2x scale), because CI is 5-7x faster than that hardware and understates what users feel.

It launches the jar with an isolated `-Duser.home`, so your real `~/.serenity` is never touched, then drives a fixed xdotool input script against a 300-paragraph lorem document. Segments: settle 10s, 400 characters at 10/s, a 200-character burst, 20x PageDown and PageUp, 30 wheel steps, 50x Down and Up. It collects the `[FRAME]` timing lines (`ui.render.frame_timing`) and reports the median p50 and p95 per phase over 3 runs.

```bash
python3 bench/gen-lorem.py > bench/lorem-bench.txt
nix-shell -p xdotool wmctrl python3 xorg.xwininfo --run "bench/bench-laptop.sh /path/to/Serenity.jar <label>"
```

Results land in `bench/bench-results/<label>-*.txt`. Keep the CPU governor and power source the same across labels (baseline A used `powersave` on AC), and leave the keyboard and mouse alone while it runs, since the input is injected into the focused window.
