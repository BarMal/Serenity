# Diagnostics: startup black frame & stale chapter after PageDown

**master `7c334c3b6`**, GUI, run on the owner's laptop against their real `~/.serenity`
config and their real document (`/home/barney/.Git/books/Convictions of a Mage.rtf`).
`config.conf` was backed up before any edit and restored + verified byte-identical
(`md5sum`) afterward; no other files under `~/.serenity` were modified. One
`java`/`systemd-inhibit` process at a time throughout.

## A. Startup black frame / missing layout prompt

Config state (read-only, before any launch):
```
$ ls -l ~/.serenity/layout-override-reviewed ~/.serenity/config.conf.bak-*
-rw-r--r-- 1 barney users 10260 Oct 12 21:17 /home/barney/.serenity/config.conf.bak-20261012T201748161Z
-rw-r--r-- 1 barney users    34 Oct 15 20:36 /home/barney/.serenity/layout-override-reviewed

$ grep -nE 'ui\.layout|line_numbers|pane_headers|status\.placement|editor\.scrollbar|tab_strip' ~/.serenity/config.conf
37:editor.line_numbers = false
45:editor.pane_headers = false
82:status.placement = pinned
```
No `ui.layout`, `editor.scrollbar`, or `tab_strip` key was present in the config at test
time (both are config-default).

Two launches, no file argument, no key press, each screenshotted at ~3s and ~5s:

| launch | ~3s | ~5s |
|---|---|---|
| 1 | Start page painted (`welcome-*.png` not saved separately — see `startup-launch1-3s.png`, already showing the menu) | painted (`startup-launch1-5s.png`) |
| 2 | **Fully black window** (`startup-launch2-3s.png`) | painted (`startup-launch2-5s.png`) |

**Log-derived timing** (more precise than the ~3s/~5s screenshots, which have a few
hundred ms of shell/screenshot overhead on top of the launch command):

| launch | `[LOGS] Writing to...` | `Starting Serenity text editor` | `Initial render completed` | total |
|---|---|---|---|---|
| 1 | 20:54:27.339 | 20:54:30.320 (+2.98s) | 20:54:31.940 (+1.62s) | **4.60s** |
| 2 | 20:55:26.270 | 20:55:29.419 (+3.15s) | 20:55:31.108 (+1.69s) | **4.84s** |

The two launches take almost identical total time to first paint (4.60s vs 4.84s) — the
"launch 1 painted by 3s, launch 2 didn't" difference we saw visually is most likely just
where the ~3s screenshot landed relative to that ~4.6-4.8s window, not a real run-to-run
difference in startup behavior. **In both cases the start page painted well within 5s with
no key press**, so the premise of "missing layout prompt" didn't reproduce as a stuck/blank
state — what's really happening is that cold start consistently takes ~4.6-4.8s before
anything paints, which is enough to show as a black frame if you check too early. Worth
treating as a startup-latency question rather than a rendering-correctness bug, unless
there's a case where it never paints at all (not observed here).

Two side notes visible in both logs, confirming earlier session findings carried through to
this commit:
- `[MEMORY] Applied for the G1 collector: MinHeapFreeRatio=10, MaxHeapFreeRatio=30,
  G1PeriodicGCInterval=30000` — matches the "Variant B" GC flags from the Job 7 JVM-flags A/B
  benchmark (see `claude/bench-results-jobs-5-6-7`); looks like that recommendation made it
  into the shipped defaults.
- `[FILE_DIALOG] Swing chooser (org.gtk.Settings.FileChooser is not installed and GTK would
  abort the process; ui.file_dialog = native overrides)` — the app now detects the missing
  GTK schema itself and falls back to a Swing file chooser, avoiding the `Ctrl+O` SIGABRT
  crash found during the Job 5 white-runner-fix diagnostics.

No errors or warnings in either log.

## B. Stale chapter after PageDown

Reproduced using the owner's real RTF file. Screenshotting this bug through a relay is
awkward: the stale content clears as soon as the Serenity window loses focus, so switching
to report it (even to tell me "it's showing") normally clears it before a screenshot can be
taken. The owner worked around this by reproducing it on the laptop and messaging from their
phone, keeping the laptop window focused.

| condition | result | evidence |
|---|---|---|
| (1) as-is | **Stale chapter reproduces.** Chapter 5 → Chapter 6 → Chapter 5's text again, duplicated | `pagedown-cond1-stale.png` |
| (2) `editor.scrollbar = false` added | **Still reproduces**, identical duplicate pattern | `pagedown-cond2-stale.png` |
| (3) `ui.layout = conventional` instead (scrollbar key removed) | **Does not reproduce** — clean Chapter 5 → Chapter 6, no duplicate | `pagedown-cond3-clean.png` |

`editor.scrollbar = false` has no effect on this bug; `ui.layout = conventional` avoids it
entirely. That points at the layout/rendering path specific to the non-conventional
(default) layout mode as the likely source of the stale paint, rather than anything to do
with the scrollbar itself.

`config.conf` was restored to its pre-test state and verified via `md5sum` match
immediately after condition 3 closed.
