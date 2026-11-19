# Choosing a renderer

`serenity --optimize-renderer` measures the renderer modes on your machine, says which one is clearly fastest, and
leaves your settings alone unless you ask it to write the answer (#2618).

## The modes

| Mode | Settings | What it is |
|---|---|---|
| layer+image | `ui.render.backend = skia`, `ui.render.skia_frames = image` | The GPU window layer, handed an image for each frame. The default. |
| skia-raster | `ui.render.backend = skia-raster` | Skia painting on the CPU. |
| layer+picture | `ui.render.backend = skia`, `ui.render.skia_frames = picture` | The GPU window layer, replaying recorded pictures. Only measured with `--thorough`. |

## Running it

```
serenity --optimize-renderer              # about 12 minutes, 4 trials
serenity --optimize-renderer --thorough   # about 18 minutes, 6 trials: layer+picture too
serenity --optimize-renderer --apply      # also write a recommended mode to config.conf
```

The time counts three minutes a trial, which starting and stopping each window included: a run of the four trials
measured 12 minutes 43 seconds on a virtual display.

It opens a window for each trial, sizes it up to the screen (less the 80 pixels its resize step grows it by), and types
into it, scrolls it, resizes it and changes its theme. **Leave the keyboard
and mouse alone while it runs**; it says so, and waits five seconds before it starts (Ctrl+C cancels). Every mode runs
twice, once on the way out and once on the way back, so a machine that warms up or throttles during the run favours
none of them.

Each trial is a separate process in safe mode, on a generated document, so your own files and session are never touched.

## What it recommends

It switches only when a mode is clearly better: at least 15% lower typing median than the others, no full-window row
(page-down, full redraw) more than 15% worse, and a gap more than twice the spread between that mode's own two runs.
Otherwise it says so and keeps your mode. A trial that fails is run once more, and says so as it does; a mode with a
run that still fails is reported, never chosen.

Without `--apply` it prints the lines to set. With `--apply` it writes `ui.render.backend` (and `ui.render.skia_frames`
for the layer) and nothing else, and only when it recommends a switch. A change needs a restart.

The measurements are kept in `~/.serenity/render-benchmark.json`, beside `config.conf`, with the machine they were
taken on (operating system, session type, display size and scale, JVM and Skiko versions, power source). A result taken
on a different setup reads as stale and is measured again, never reused.

## The 16.7 ms target, and what it does not do

The editor's goal for typing latency (#2489) is a 95th percentile of 16.7 ms from key to screen at 2x. The report says
where each mode stands against it. **No mode reaches it on the reference laptop today** (the best is around 46 ms at
2x), and the report says so plainly when that is the case: choosing among the renderers that exist cannot close a gap
that none of them closes. The target is judged only at 2x, and only when the measuring clock is fine enough for it (see
below); otherwise the report says it was not judged and why.

"2x" is judged by the pixels the trial really drew: the target applies when the trial's canvas had at least 5
megapixels, whatever scale the JVM reported. The scale is not trusted because an X11 toolkit under XWayland reports 1.0
on a 2x monitor. Each run keeps both in the result file, the canvas size as `canvasPixels` and the scale the JVM reported
as `jvmScale`. A trial on a smaller canvas is reported as not judged, with how many megapixels it had and how many the
target needs, rather than judged against a lighter load than 2x.

## How it measures, and where that stops

The trial watches a small region of the screen with `java.awt.Robot` and times how long a typed key, a page-down, a
resize or a theme change takes to show there, so it does not depend on the renderer's own hooks. Before the run it
measures its own cost: the clock cannot say a change showed sooner than one screen grab takes, and that figure
(`clockResolutionMs`) is kept with the result. The target is not judged when the resolution is coarser than a quarter of
it (4.2 ms).

- **Linux only for now** (an X11 session, or XWayland under Wayland); the window needs a display.
- **Where the screen cannot be read.** On some Wayland sessions the X11 screen `Robot` reads comes back all black. When
  none of a trial's first five typed characters shows, it stops watching the screen and measures again by the app's own
  stamps of when each step was painted (the clock `bench-laptop.sh` reads for key to screen). The report marks those
  figures `clock=in-app` and says what they leave out: they are lower bounds, because a key the trial submits has no
  OS-to-event-thread hop, and the resize cannot be stamped, so the full-redraw row is theme toggles only. The clock is
  kept with each run in the result file. If the app's own clock sees no keystroke either, the trial fails and says so.
- A resize is watched by the pixels across the edge the window moves. Where the desktop behind the window is the same
  colour as the editor's background, resizes cannot be seen and count as unmeasured; theme changes still are.
- The GL renderer is part of the machine key: the adapter the layer window reports (the name Skiko gives its
  analytics hook, the GL renderer string on OpenGL, `Software` for the software renderer). It comes from the first trial
  that reports one, even a failed trial, and stays `unknown` when none does (a run of the raster mode alone has no
  layer, so no adapter).
- Run on AC power and with nothing else busy: a notification, a battery profile or a busy machine can change a ranking,
  which is why the spread between a mode's two runs is part of the rule.
