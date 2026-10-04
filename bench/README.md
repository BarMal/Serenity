# Laptop benchmark

The app-level benchmark behind the A/B/C comparisons in #1798 and #1812. It must run on the target laptop (Intel UHD 620, Hyprland/XWayland, 2x scale), because CI is 5-7x faster than that hardware and understates what users feel.

It launches the jar with an isolated `-Duser.home`, so your real `~/.serenity` is never touched, then drives a fixed xdotool input script against a 300-paragraph lorem document. Segments: settle 10s, 400 characters at 10/s, a 200-character burst, 20x PageDown and PageUp, 30 wheel steps, 50x Down and Up. It collects the `[FRAME]` timing lines (`ui.render.frame_timing`) and reports the median p50 and p95 per phase over 3 runs.

```bash
python3 bench/gen-lorem.py > bench/lorem-bench.txt
nix-shell -p xdotool wmctrl python3 xorg.xwininfo --run "bench/bench-laptop.sh /path/to/Serenity.jar <label>"
```

Results land in `bench/bench-results/<label>-*.txt`. Keep the CPU governor and power source the same across labels (baseline A used `powersave` on AC), and leave the keyboard and mouse alone while it runs, since the input is injected into the focused window.

## Profiling the per-keystroke state path with JFR

`com.serenity.perf.TypingProfile` (test scope, not run by CI) replays one keystroke scenario against a live `StateManager` so a Java Flight Recorder recording has enough samples to show where the time goes. Scenarios: `typing_random`, `typing_long_paragraph`, `cold_typing`, `move_down_up`, `page_down_up`; the second argument is seconds (keys for `cold_typing`). Flags: `--warmup-s=10` (warm-up before timing), `--pace-ms=100` and `--warm-ms=0` (`cold_typing` only: pause between keys, and a startup warm-up burst on a separate `StateManager` first) and `--real-warm-ms=0` (`cold_typing` only: runs the app's own `StartupWarmUp.run`, offscreen frames included, and stops it after that many ms as a first keystroke would; needs a display).

```bash
# 1. Export the test classpath (one line, no sbt banner)
sbt -batch "export Test/fullClasspath" | tail -1 > /tmp/serenity-cp.txt

# 2. Record a scenario (the editor needs a display; xvfb-run provides one headless)
xvfb-run -a java \
  -XX:StartFlightRecording:filename=typing_random.jfr,settings=profile,jdk.ExecutionSample#period=1ms \
  -cp "$(cat /tmp/serenity-cp.txt)" com.serenity.perf.TypingProfile typing_random 30

# 3. Aggregate: phase shares, then the top self and inclusive frames
python3 bench/jfr-aggregate.py typing_random.jfr --top 20
```

`TypingProfile` also prints `RESULT` lines (p50, p95, mean ms per event and allocated bytes per event); `cold_typing` prints the mean and p95 per keystroke bucket (1-50, 51-100, 101-200, 201-400, 401-600) and runs in a fresh JVM with no warm-up, for example `... TypingProfile cold_typing 300 --pace-ms=100 --warm-ms=1500`.

The numbers are only comparable like-for-like: on the same machine, with the same JVM flags. Run the laptop recording with exactly the flags above, and do not compare it with a CI or workstation recording.
