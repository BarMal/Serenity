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
serenity --optimize-renderer              # about 8 minutes, 4 trials
serenity --optimize-renderer --thorough   # about 12 minutes, 6 trials: layer+picture too
serenity --optimize-renderer --apply      # also write a recommended mode to config.conf
```

It opens a window for each trial and types into it, scrolls it, resizes it and changes its theme. **Leave the keyboard
and mouse alone while it runs**; it says so, and waits five seconds before it starts (Ctrl+C cancels). Every mode runs
twice, once on the way out and once on the way back, so a machine that warms up or throttles during the run favours
none of them.

Each trial is a separate process in safe mode, on a generated document, so your own files and session are never touched.

## What it recommends

It switches only when a mode is clearly better: at least 15% lower typing median than the others, no full-window row
(page-down, full redraw) more than 15% worse, and a gap more than twice the spread between that mode's own two runs.
Otherwise it says so and keeps your mode. A mode with any failed run is reported, never chosen.

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

## How it measures, and where that stops

The trial watches a small region of the screen with `java.awt.Robot` and times how long a typed key, a page-down, a
resize or a theme change takes to show there, so it does not depend on the renderer's own hooks. Before the run it
measures its own cost: the clock cannot say a change showed sooner than one screen grab takes, and that figure
(`clockResolutionMs`) is kept with the result. The target is not judged when the resolution is coarser than a quarter of
it (4.2 ms).

- **Linux only for now** (an X11 session, or XWayland under Wayland); the window needs a display.
- A resize is watched by the pixels across the edge the window moves. Where the desktop behind the window is the same
  colour as the editor's background, resizes cannot be seen and count as unmeasured; theme changes still are.
- The GL renderer is not part of the machine key, so a graphics driver update alone does not mark a result stale.
- Run on AC power and with nothing else busy: a notification, a battery profile or a busy machine can change a ranking,
  which is why the spread between a mode's two runs is part of the rule.
