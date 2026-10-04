# Laptop benchmark

The app-level benchmark behind the A/B/C comparisons in #1798 and #1812. It must run on the target laptop (Intel UHD 620, Hyprland/XWayland, 2x scale), because CI is 5-7x faster than that hardware and understates what users feel.

## Getting prebuilt jars (the laptop never builds)

The laptop is too slow to build, so the manual `bench-artifacts.yml` workflow builds everything on GitHub Actions. It packs one artifact, `bench-<7-char sha>`, containing:

- `Serenity.jar`, the app, from the same `sbt assembly` as a normal build;
- `serenity-perf.jar`, a fat jar of the Test classpath, so `com.serenity.perf.*` runs with plain `java`;
- `bench/`, these scripts;
- `MANIFEST.txt`, with the commit, the build JDK and the exact run commands;
- with `spike_ref`: `spike/serenity-spike.jar` (Skiko and its linux-x64 native runtime included) and the spike's `README.md`.

```bash
gh workflow run bench-artifacts.yml -R BarMal/Serenity -f ref=<sha> [-f spike_ref=spike/skiko-renderer]
gh run list -R BarMal/Serenity --workflow=bench-artifacts.yml -L 1   # note the run id
gh run watch <run-id> -R BarMal/Serenity
gh run download <run-id> -R BarMal/Serenity -n bench-<sha> -D bench-<sha>
cd bench-<sha> && cat MANIFEST.txt
```

`ref` may be any branch, tag or SHA, including commits from before the workflow existed: the Test-jar settings live in `bench/ci/perf-assembly.sbt` and are copied into the checkout only for that run. Artifact zips lose the executable bit, so call scripts through `bash` (or `chmod +x bench/*.sh`). Run every command below from the unpacked directory. `<sha>` in the artifact name is the first 7 characters of the commit; the run uploads only this artifact, so `gh run download <run-id>` without `-n` fetches it too.

## Running the laptop benchmark

It launches the jar with an isolated `-Duser.home`, so your real `~/.serenity` is never touched, then drives a fixed xdotool input script against a 300-paragraph lorem document. Segments: settle 10s, 400 characters at 10/s, a 200-character burst, 20x PageDown and PageUp, 30 wheel steps, 50x Down and Up. It collects the `[FRAME]` timing lines (`ui.render.frame_timing`) and reports the median p50 and p95 per phase over 3 runs.

```bash
python3 bench/gen-lorem.py > bench/lorem-bench.txt
nix-shell -p xdotool wmctrl python3 xorg.xwininfo --run "bash bench/bench-laptop.sh $PWD/Serenity.jar <label>"
```

The in-process benchmarks run the same way; the argument is a name-prefix filter:

```bash
java -cp serenity-perf.jar com.serenity.perf.PerformanceBenchmarks laptop.
```

Results land in `bench/bench-results/<label>-*.txt`. Keep the CPU governor and power source the same across labels (baseline A used `powersave` on AC), and leave the keyboard and mouse alone while it runs, since the input is injected into the focused window.

## Profiling the per-keystroke state path with JFR

`com.serenity.perf.TypingProfile` (test scope, not run by CI) replays one keystroke scenario against a live `StateManager` so a Java Flight Recorder recording has enough samples to show where the time goes. Scenarios: `typing_random`, `typing_long_paragraph`, `typing_in_long_document` (5,000 paragraphs), `cold_typing`, `move_down_up`, `page_down_up`; the second argument is seconds (keys for `cold_typing`). Flags: `--warmup-s=10` (warm-up before timing), `--pace-ms=100` and `--warm-ms=0` (`cold_typing` only: pause between keys, and a startup warm-up burst on a separate `StateManager` first).

```bash
# 1. Record a scenario (the editor needs a display; on a headless box wrap it in xvfb-run -a)
java \
  -XX:StartFlightRecording:filename=typing_random.jfr,settings=profile,jdk.ExecutionSample#period=1ms \
  -cp serenity-perf.jar com.serenity.perf.TypingProfile typing_random 30

# 2. Aggregate: phase shares, then the top self and inclusive frames
python3 bench/jfr-aggregate.py typing_random.jfr --top 20
```

On a machine that does build, `-cp "$(sbt -batch "export Test/fullClasspath" | tail -1)"` replaces `-cp serenity-perf.jar`.

`TypingProfile` also prints `RESULT` lines (p50, p95, mean ms per event and allocated bytes per event); `cold_typing` prints the mean and p95 per keystroke bucket (1-50, 51-100, 101-200, 201-400, 401-600) and runs in a fresh JVM with no warm-up, for example `... TypingProfile cold_typing 300 --pace-ms=100 --warm-ms=1500`.

The numbers are only comparable like-for-like: on the same machine, with the same JVM flags. Run the laptop recording with exactly the flags above, and do not compare it with a CI or workstation recording.
