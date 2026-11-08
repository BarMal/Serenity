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

`bench-artifacts.yml` also runs nightly on master. `bench/pull_nightly.py` downloads the newest of those bundles on a timer, runs the machine's benchmarks and compares with the previous night: see `docs/nightly-bench-pull.md`.

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

`BENCH_JAVA_OPTS` passes JVM options to every launch and is recorded in the conditions file, for example
`BENCH_JAVA_OPTS="-Xlog:gc:file=$PWD/gc-%p.log:uptime -Dskiko.fps.longFrames.show=true -Dskiko.fps.longFrames.millis=20"`
(`%p` is the JVM's process id, so each run gets its own GC log; without it the JVM rotates the previous run's log away). `-Dserenity.skia.layerFrames=image` makes the Skia layer draw CPU-painted images instead of replaying recorded pictures on the event thread; compare it with the default by the `edt-lag` and `key_to_screen` figures. Each run's whole app log and stdout are
kept beside its run file as `<label>-run<N>.applog` and `.stdout`, so the per-key `[LATENCY]` lines and anything Skiko
prints are there afterwards. A JVM that is still alive after the quit key (it has been seen to log `Quitting` and then never exit) is thread-dumped to
`<label>-run<N>.threads`, sent SIGTERM and, after `BENCH_QUIT_GRACE` seconds (15), killed, so one hang costs seconds and
leaves a stack instead of blocking the batch. The conditions file also records each monitor's mode with its refresh rate and scale
(`hyprctl`) and the OpenGL renderer (`glxinfo -B`) when those tools exist.

`bench-laptop.sh` sources `bench/bench-lib.sh` (keep the two together). It finds the app's log under either `$HOME/.local/state/serenity/` or `$HOME/.serenity/`, reads AC power from whichever `AC*` supply reports it, and exits non-zero when no run produced a `[FRAME]` timing line, so a batch of failed launches is not mistaken for a result. `scripts/test_bench_laptop.py` covers these.

The run's output records how the quit key was sent and how many `[STATE MODAL]` lines the app logged; the run types text first, so a `ctrl+q` that reaches the app raises the unsaved-changes modal instead of quitting, and it is the `SIGTERM` in `stop_app` that ends it. A JVM still alive after `SIGTERM` is reported as `HUNG:` in the run's output, and its `stdout.log` and app log are copied beside the thread dump as `<label>-run<N>-hang.stdout.log` and `-hang.app.log` (#2507).

## Time to first frame of a packaged image

`bench/ttfw.py <launcher> <runs> <label> [warm-up-runs]` launches a packaged app image with `--smoke-test --safe-mode` and reports the median wall time to the first painted frame (the `SERENITY_SMOKE_READY` line) and to exit. It needs a display and the image from a `desktop-release.yml` run (artifact `Serenity-linux-x64`, the zip of `dist/Serenity`); the laptop does not build. The warm-up run is discarded, which lets an AppCDS image write its archive.

### Start-up as benchmark rows (#2420)

`bench/startup.py <launcher> <runs> [--discard N] [--csv out.csv]` writes `startup.cold.first_frame` and `startup.warm.first_frame` in the `PerformanceBenchmarks` CSV layout, so `check_perf_regression.py`, `compare_perf_runs.py` and `abba_compare.py` read them like any other row. Warm starts the image with its AOT cache, cold with the cache moved aside (the OS file cache is warm for both); launches alternate so drift falls on both. The first `--discard` launches per mode (default 1) are not in the rows; their count is recorded as `context,discarded_runs,N` and their times as `context,discarded_warm_ms,...` and `context,discarded_cold_ms,...`, so a first-launch penalty stays visible. With `--interactive` it launches with the hidden `--startup-probe` flag instead: the app injects one key after its first frame and prints `SERENITY_PROBE_INTERACTIVE` once that key has been painted, so one launch yields `startup.*.first_frame` and `startup.*.interactive`. It uses `--safe-mode` like `ttfw.py`, so it is the floor for first frame: session restore, language servers and the dictionary are not on this path.

### How much start-up varies (#2420)

`bench/startup-variance.sh <app-image> <runs-per-round> <out-dir>` runs `ttfw.py` in six rounds ordered warm, cold, cold, warm, warm, cold, so drift falls on both modes alike. "Cold" means the image's AOT cache is moved aside (the OS file cache stays warm). `bench/startup-spread.py` pools the rounds into n, median, coefficient of variation and max/min per mode. The manual `startup-variance.yml` workflow runs it on a GitHub runner against the Linux image of a Desktop Package Check run (`image_run_id`), to decide whether start-up rows can gate in CI or only track a trend.

### ABBA over two images (#2420)

`bench/abba-startup.sh <A-launcher> <B-launcher> <out-dir> [runs]` is `abba.sh` for start-up: it runs `startup.py` against the two images' launchers in the order A B B A A B B A, writes `a-1..4.csv` and `b-1..4.csv`, and applies the protocol's blocking rule with `scripts/abba_compare.py`. The time floor is 100 ms (`ABBA_STARTUP_MIN_DELTA_MS`; `abba_compare.py --time-delta-ms`), not the 0.03 ms of the in-process rows, which would block on noise at a start-up scale. The 100 ms is a guess, not a measurement: two runs of one commit on a GitHub runner differed by 77 ms (warm) and 114 ms (cold), and nothing has been measured on the laptop, so run an A/A there before trusting it. Set `ABBA_STARTUP_REPORT_ONLY=1` for a CI job: findings become `::warning::` lines and the script exits 0 whatever happens, because start-up is tracked as a trend on CI and only the laptop run gates. Both images need a trained AOT cache. Build the two images from the merge base and the candidate with `desktop-release.yml`.

## Opening a large project (#2420)

`index.walk.cold.100k` and `index.walk.warm.100k` time `ProjectFileIndex.listing` (what "Go to File" does) over a synthetic tree: from a fresh index (a full walk, the first open) and from one that already walked it (a revalidation, every later open). They are `PerformanceBenchmarks` rows, so the paired-ratio gate judges them like the others; run just these with `java -cp serenity-perf.jar com.serenity.perf.PerformanceBenchmarks index.walk.`. The tree is written the first time a row needs it. Warm-up (1) and iteration counts (5 cold, 20 warm) are the runner's mechanics, not targets.

`SyntheticProjectTree` (test scope) writes the tree deterministically from a seed: 100,000 visible files in 10,000 directories is the size in #2420. Depth 8 and the log-normal spread of files per directory (sigma 1.6) were fitted to four real trees measured on 2026-10-30: deepest file 6 to 9 levels, median files per directory 0.11 to 0.28 of the mean (sigma 1.6 gives 0.28). The counts of ignored directories (20 roots, 50 files each) and rule-ignored files (500) are assumptions, not measurements: the real trees had none, and the walker never reads them, so they only change how long the tree takes to write.

## Typing while a project is indexed (#2420)

`com.serenity.perf.TypingWhileIndexingProfile` types the same keystrokes idle and while cold walks of the 100,000-file tree run back to back on other threads, in idle, loaded, loaded, idle blocks so drift falls on both alike. It reports `count`, `p50_ms`, `p95_ms` and `max_ms` per mode, the loaded/idle ratios and how many walks finished while typing. A keystroke here is the edit plus the damaged frame drawn for it: the in-process part of input-to-paint, so a lower bound on what a user sees. It needs a display.

```bash
java -cp serenity-perf.jar com.serenity.perf.TypingWhileIndexingProfile            # --keys=300 --pace-ms=100 --warmup-keys=200 --small
```

On a runner use the manual `typing-while-indexing.yml` workflow (it must be on master to dispatch). Defaults: 100 ms between keys is the 10 keys per second `bench-laptop.sh` types at; 300 keys per block and 200 warm-up keys are how long to measure for, not targets. **There is no threshold on purpose.** The proposal's "p95 at most 1.5x idle" was a guess, and the standing rule (#2028) is that a guess is not a bound. Once there are numbers from the laptop and a runner, with their spread, the owner can decide whether and where it gates. No numbers have been taken yet.

## Per-keystroke latency trace

`[FRAME]` reports each phase's percentiles over its own population (every frame, every input chain), so its p50s do not add up to `input-to-paint`. `ui.render.latency_trace = true` (default off) follows each keystroke instead and logs one line per painted keystroke, then a summary every 5 seconds:

```
[LATENCY] seq=41 when_to_edt=3 edt_to_enqueue=0.01 queue=0.21 dispatch=0.08 apply=4.90 damage=0.31 frame_wait=0.00 pacing_wait=7.95 pre_render=0.05 render=14.80 publish_to_paint=12.40 paint=2.90 total=43.61 ms
[LATENCY] summary window=5.0s keys=48 unpainted=0 | when_to_edt p50=... p95=... | ... | total p50=... p95=... | key_to_screen p50=... p95=... ms
```

The stages run back to back, so they add up to `total`: the event thread receiving the key to queueing it (`edt_to_enqueue`), waiting for the input loop (`queue`), the batch's earlier work before its dispatch (`dispatch`), applying it (`apply`), diffing and emitting damage (`damage`), waiting for the render loop to start a fast frame, including a frame already under way (`frame_wait`), the frame deadline (`pacing_wait`), the resize check before the frame reads the model (`pre_render`), drawing until the image is published (`render`), waiting for Swing to start painting it (`publish_to_paint`) and the paint itself (`paint`). `when_to_edt` is the wall-clock gap from the OS event time (`KeyEvent.getWhen`) to the event thread, outside `total`; `key_to_screen` adds it to each keystroke's `total` before taking the percentiles, which is what the user waited (a percentile of sums, not the sum of percentiles). Nothing after `paintComponent` returns (Swing's back-buffer blit, the compositor) is visible to the app. `unpainted` counts keystrokes whose frame changed nothing on screen. The per-keystroke lines are written in a batch with each summary.

To trace a bench run, add `ui.render.latency_trace = true` to the config `bench-laptop.sh`'s `run_once` seeds (next to `ui.render.frame_timing = true`), and keep the lines before the script deletes its isolated home, for example by adding `grep "\[LATENCY\]" "$log_file" >> "$out" || true` beside its `[FRAME]` grep. Outside the script, set it in the `config.conf` the jar reads and use `~/.serenity/serenity.log`. Then summarise:

```bash
python3 bench/latency-summary.py bench/bench-results/<label>-run1.txt
```

It prints p50/p95/max per stage over every keystroke, and each stage's share of the summed p50s.

## Profiling the per-keystroke state path with JFR

`com.serenity.perf.TypingProfile` (test scope, not run by CI) replays one keystroke scenario against a live `StateManager` so a Java Flight Recorder recording has enough samples to show where the time goes. Scenarios: `typing_random`, `typing_long_paragraph`, `cold_typing`, `move_down_up`, `page_down_up`; the second argument is seconds (keys for `cold_typing`). Flags: `--warmup-s=10` (warm-up before timing), `--pace-ms=100` and `--warm-ms=0` (`cold_typing` only: pause between keys, and a startup warm-up burst on a separate `StateManager` first) and `--real-warm-ms=0` (`cold_typing` only: runs the app's own `StartupWarmUp.run`, offscreen frames included, and stops it after that many ms as a first keystroke would; needs a display).

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

## Key to screen, per backend

`bench/key-to-screen-abba.sh <jar> <label>` runs `bench-laptop.sh` as java2d, skia, skia, java2d (`bench-laptop.sh <jar> <label> skia-raster` runs the Skia CPU raster the same way) (each its usual 3 runs, with `--renderer=<backend>` and `ui.render.latency_trace = true`) and reduces the tagged `[LATENCY] summary` and `[FRAME]` lines with `bench/key_to_screen_summary.py`: p50 and p95 of each keystroke's `total`, and of `input-to-paint`, per backend.

```bash
nix-shell -p xdotool wmctrl python3 xorg.xwininfo --run "bash bench/key-to-screen-abba.sh $PWD/Serenity.jar <label>"
```

The clock is `System.nanoTime`, from the key's arrival at the AWT listener on the event thread to the end of the frame's presentation. For Java2D and the Skia raster that is the end of the canvas paint; for the Skia layer it is the end of Skiko's render callback, **not GPU completion and not display scan-out** (the lines say so with `presented=`). `when_to_edt` is the gap from `KeyEvent.getWhen` to the event thread, in wall-clock milliseconds on a time base the platform decides; it is shown on its own and is not part of `total`, but it is part of `key_to_screen` (and so of the `key-to-screen` columns of `key_to_screen_summary.py`). The OS input path and the display's scan-out are outside these figures.

## Scaling with many open files and split panes (#2478)

`bench/bench-scaling.sh <jar> <label> <a|b|c|d> [java2d|skia]` is the laptop side of the harness rows that ask how
performance scales: `a` ctrl+Tab through 1 and through 20 open files, `b` typing, PageDown and the wheel with 1, 2 and 4
panes (a different file in each), `c` the heap in use after a GC with 1 file and with 20, `d` the time to the last of four
scrollbars, which the app's logs cannot show (the harness row `scrollbar.four_panes.all_bars` measures it). The documents
come from `bench/gen-scaling-docs.py` (distinct lines per file, prose or code with `BENCH_FILE_KIND`), so the app's text
caches cannot share entries between files. `--dry-run` prints what each mode would do without a jar or a display.

It was written where xdotool and a desktop session are not available: the documents, the pane key plans and the argument
handling are tested (`scripts/test_bench_scaling.py`), the driving of the app is not. Read the first result file before
trusting a table. The shape cache's lookups, misses and evictions are not in the app's logs: run
`java -cp serenity-perf.jar com.serenity.perf.ScalingShapeCacheBenchmarks`.
