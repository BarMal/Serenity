# Skiko spike (throwaway, #1812 / Skia plan step 2)

This branch is not for merging. It answers one question: does Skiko meet #1812's go criteria on the laptop?

The go criteria are:

- full frame ≤ 8 ms
- damaged frame ≤ 4 ms
- input-to-paint p50 ≤ 20 ms
- idle CPU ≈ 0
- crisp 2× text
- IME works

The spike is a separate sbt project, `spike/`. The root project does not aggregate it, so `sbt test`, the gate and `Serenity.jar` are unchanged.

## What it is

`com.serenity.spike.SkikoSpike` draws a realistic editor frame:

- the 300-paragraph lorem document from `bench/gen-lorem.py`;
- a 1500×1000 logical window at 2× (3000×2000 device pixels);
- a gutter with line numbers, a current-line band, a selection, wrapped prose and a caret on a vertically centred row.

**Wrapping is Serenity's own.** It uses `TextLayoutSnapshot.wrapRows`, the same `ParagraphMeasurement` + `GlyphAdvances` + UAX#14 path, with Java2D measurement of `Serif` 14. The `private[layout]` access goes through `spike/.../ui/layout/SpikeLayoutBridge.scala`. Skia only shapes and paints the rows.

**Shaping uses one of three strategies,** chosen with `--shaper`:

- `explicit` (default): HarfBuzz through `Shaper.shape`, given the runs directly, with the glyphs kept as a `TextBlob` plus a caret array.
- `reused`: `Shaper.shapeLine` on one shared shaper.
- `textline`: `TextLine.make` per row.

Shaped rows are cached by their text, so a frame reshapes only rows whose text changed.

> Caution, here be imagine dragons: because wrapping is measured with Java2D and painting with Skia, row breaks and painted glyphs come from two different measurers. That is the measurement-parity risk the plan names. See the parity numbers below: the painted text is about 2.6% narrower than the wrap assumed.

### Modes

| `--mode` | What it measures |
|---|---|
| `headless` | No window. Rasterises into an offscreen raster `Surface` on the calling thread. Measures full frame (shaped rows cached), full frame with every row reshaped, fills only, and simulated typing: state update (insert + Serenity rewrap of one line), damaged-region frame, and input to rendered. |
| `window` | A `JFrame` with a `SkiaLayer`. Measures 120 forced full frames, then 200 keys paced at 60 ms (input → state update → frame → end of `onRender`), then idle CPU over `--idle-s`. |
| `interactive` | The same window, typed by hand. It measures input-to-paint per key and logs IME events. Close the window to print the summary. |
| `shaping` | Headless microbenchmark of shaping and measuring the 300 paragraphs: Serenity's `GlyphAdvances` and full wrap against every Skiko text API, plus parity checks. |

### Pipelines (`--pipeline`, window modes)

- `direct`: draw inside `SkiaLayer`'s render callback, on the EDT.
- `picture`: record an `SkPicture` off the EDT and replay it on the EDT.
- `raster`: rasterise off the EDT into one of two persistent raster surfaces, drawing only the damage (this frame's damage plus the previous frame's), then blit the snapshot on the EDT.

Only `raster` draws less than a full frame per key. `SkiaLayer` does not promise to keep the previous frame's pixels, so `direct` and `picture` redraw everything.

### Render API

Set the render API with the `SKIKO_RENDER_API` environment variable or `--render-api=` (which sets `skiko.renderApi`). The values are `SOFTWARE_FAST`, `SOFTWARE_COMPAT`, `OPENGL` and `VULKAN`. The `RESULT window.*.setup` line prints the API that was actually picked, and `render_info` prints the adapter.

### What is timed, and what isn't

**`onRender` does not draw.** Skiko's `SkiaLayer.update` records the `onRender` callback into an `SkPicture`; this was read from the 0.150.1 bytecode (`pictureRecorder.beginRecording`, before the delegate call). The redrawer rasterises that picture afterwards. So:

- **`on_render`** is recording time only.
- **`edt_frame_task`** runs from the start of `onRender` to an `invokeLater` posted inside it, so it includes Skiko's raster and present whenever they run in the same EDT task.

> Caution, here be imagine dragons: that they always share one EDT task is inferred, not verified.

Further limits:

- **GL timings measure the CPU, not the GPU.** On GL, the frame task covers CPU command submission and possibly the swap, not GPU completion.
- **`input_to_paint` is a proxy.** It is key → state update → frame → end of the EDT frame task, which is the closest available measure of #1812's input-to-paint, not the same thing.
- **Direct and picture already record twice.** Because Skiko itself records every frame, the "direct" pipeline is already record-then-replay on the EDT. The "picture" pipeline records off-EDT, and Skiko then records the replay again.

## Skiko version

The brief asked for 0.150.2, but **0.150.2 is not on Maven Central.** It is published only as release assets on the Skiko releases page. The spike uses **0.150.1**, the newest 0.150.x on Central (`org.jetbrains.skiko:skiko-awt` and `org.jetbrains.skiko:skiko-awt-runtime-linux-x64`). For macOS or Windows, swap the runtime artifact (`-macos-arm64`, `-windows-x64`, ...).

## Per-glyph positions: what Skiko exposes

| API | Per-glyph positions | Caret stops per UTF-16 offset | Cost in the container (300 paragraphs, 163k chars) |
|---|---|---|---|
| `TextLine.make` / `Shaper.shapeLine` | `getPositions()` (x,y pairs), `getGlyphs()` | `getCoordAtOffset(i)`, one JNI call each; `getOffsetAtCoord` for hit-testing | **≈1.1–1.2 s**: about 3.7 ms fixed per call, whatever the text length |
| `Shaper.shape(text, font)` → `TextBlob` | `TextBlob.getPositions()`, `getGlyphs()`, `getClusters()` | from the clusters | ≈1.15 s (same fixed per-call cost) |
| `Shaper.shape(..., RunHandler)` with **caller-supplied run iterators** (`FontRun`, `BidiRun`, `ScriptRun`, `LanguageRun`) | `commitRun(info, glyphs, positions, clusters)` | from the clusters (UTF-16 indices, checked with é and a CJK character) | **≈60 ms**, ≈65 ms including building the `TextBlob` and caret array |
| `paragraph.Paragraph` | none exposed as an array | `getRectsForRange(i, i+1)` per character, or `getGlyphPositionAtCoordinate` | layout ≈34 ms (cache off); caret read-back by rects ≈135 ms |
| `Font.getWidths(getStringGlyphs(s))` | nominal advances only, with no shaping | prefix sums | ≈9 ms |
| Serenity `GlyphAdvances` (Java2D) | — | `caretsFrom` | ≈11 ms; full Serenity wrap ≈30 ms |

The 3.7 ms per-call cost comes from the default iterators: it is the same for a 5-character string, and also for `makePrimitive`, which skips HarfBuzz. Supplying the runs yourself removes it.

> Caution, here be imagine dragons: that it is ICU and font-fallback iterator construction is an inference from the numbers above, not something read in Skia's source.

Production code would have to build real script, bidi and fallback runs itself (#1866).

## Container results (NOT comparable to the laptop)

Container: 4 cores, Xvfb, Mesa software GL, Ubuntu OpenJDK 21. For calibration, the same container runs Serenity's own Java2D `laptop.render.prose_frame_2x` at **p50 6.47 ms / p95 7.37 ms** (`xvfb-run -a sbt "Test/runMain com.serenity.perf.PerformanceBenchmarks laptop.render.prose_frame_2x"`).

These numbers were measured in the container at the branch head (`spike.sh`-equivalent runs: `java -Xmx2g -cp <exported classpath>`, window runs under `xvfb-run -s "-screen 0 3200x2200x24"` with `-Dsun.java2d.uiScale=2`, 150 keys at a 60 ms pace, 10 s idle). All values are ms (p50 / p95) unless marked otherwise.

**Headless raster** (`--mode=headless`, default `--shaper=explicit`):

| Measure | p50 | p95 |
|---|---|---|
| Full frame, rows already shaped | 20.9 | 23.3 |
| Full frame, fills only (no text) | 1.3 | 1.4 |
| Full frame, every row reshaped | 24.8 | 28.2 |
| Typing: state update (insert + Serenity rewrap of one line) | 0.26 | 0.66 |
| Typing: damaged frame (clip = the edited line's rows) | **1.8** | **2.4** |
| Typing: input to rendered (no present) | 2.1 | 2.8 |

- With `--shaper=reused` (`Shaper.shapeLine`), the damaged frame is 6.1 / 9.9 and a full reshape is 258: about 3.7 ms per newly shaped row.
- `--subpixel-text=false` makes no measurable difference (20.9 full frame).
- **Text rasterisation is about 95% of a full frame.**
- **Same box, Java2D:** Serenity's own `laptop.render.prose_frame_2x` runs at **6.5 / 7.4**. Skia's software raster of a comparable frame is about 3× slower here.

> Caution, here be imagine dragons: the two scenes are close but not identical, and Java2D's prose frame may draw less text.

**Window, `SOFTWARE_FAST`** (Xvfb, 3000×2000 device px):

| Pipeline | Full frame (worker + EDT frame task) | EDT frame task while typing | Damaged frame | Input-to-paint | Idle CPU over 10 s |
|---|---|---|---|---|---|
| direct | 31.2 / 34.8 | 33.8 / 36.9 | 33.8 / 36.9 (full redraw) | **35.2 / 39.3** | 0.1% of one core; 0 `onRender` calls |
| picture | 33.9 / 42.0 (record 0.3) | 33.9 / 37.6 | 34.4 / 38.2 (full redraw) | **35.9 / 40.8** | 0.2%; 0 calls |
| raster | 39.8 / 44.1 (worker 23.3 + EDT 16.3) | 19.7 / 22.4 | 22.4 / 25.2 (worker 2.5) | **24.0 / 27.5** | 0.1%; 0 calls |

How to read these:

- **`onRender` itself costs 0.02–0.2 ms in every pipeline,** because Skiko only records it. The cost is in Skiko's rasterisation of that recording, plus the copy to the X window.
- **Raster has a floor of about 16 ms per frame:** that is the EDT cost of blitting a 3000×2000 image through Skiko's software redrawer, even when only one line changed.

> Caution, here be imagine dragons: the split of that floor between Skia's `drawImage` and the X11 put-image is not measured.

**Window, `OPENGL`:** not measurable here. Skiko logged `RenderException: Cannot create Linux GL context` under Xvfb/Mesa and **fell back to `SOFTWARE_FAST`**, and its `setup` line printed `render_api=SOFTWARE_FAST`. The three "OPENGL" runs therefore repeat the software numbers (direct 34.1, picture 34.9, raster 22.5 input-to-paint p50). Every GL number has to come from the laptop.

**Shaping** (`--mode=shaping`, one pass over all 300 paragraphs):

| Path | p50 |
|---|---|
| Serenity `GlyphAdvances` + carets | 10.5–11.7 |
| Serenity full wrap | 28–38 |
| Skia `TextLine.make` / `Shaper.shapeLine` / `Shaper.shape` → `TextBlob` / `RunHandler` | 1,107–1,247 |
| … the same per-call cost on 300 five-character strings | 1,130 |
| `Shaper.shape` with caller-supplied runs | **60** |
| … plus `TextBlob` and caret array | 65 |
| `Paragraph` build + layout, cache off | 34 |
| `Paragraph` caret read-back via `getRectsForRange` | 135 |
| Nominal `Font.getWidths` | 9 |

**Clusters are UTF-16 indices**, for both the run handler and the `TextBlob` (checked with é and a CJK character).

**Parity, AWT against Skia** (both DejaVu Serif, 14 px):

- **With Skia's default hinting,** the painted text is **2.6% narrower** than the Java2D measurement the wrap used: 0.30 px per character, up to 173 px over a paragraph.
- **With Skia set to unhinted linear metrics,** the gap is 0.004 px per character and the worst caret across a paragraph is ≤ 6.6 px. That remaining drift is most likely HarfBuzz kerning, which `GlyphAdvances`' nominal path does not apply.
- **Row counts:** Skia `Paragraph`'s own wrap disagrees with Serenity's on 129 of 300 paragraphs.
- **Unresolved discrepancy:** the spike's own caret array (from the explicit runs) differs from `Shaper.shapeLine`'s `getCoordAtOffset` by up to 5.0 px in most paragraphs, and by 0 on a 27-character probe and on paragraph 0.

**Go criteria, container view:**

- Full frame ≤ 8: **no**. 21 headless, 31+ in the window.
- Damaged frame ≤ 4: **yes headless** (1.8), but **no end-to-end in the window** (22+, because of the blit floor).
- Input-to-paint p50 ≤ 20: **no** (24 at best, raster).
- Idle CPU ≈ 0: **yes** (0.1–0.2%, no `onRender` calls).
- Crisp 2× text: the layer's own pixels are crisp at `content_scale=2.0` (screenshots checked), but the compositor path is unverified.
- IME: **not testable** in the container.

None of this decides the go/no-go: this box has no GPU, and the go criteria assume the UHD 620 GL path.

## Running on the laptop (NixOS, Hyprland, UHD 620)

### Build once

```bash
git fetch origin spike/skiko-renderer && git checkout spike/skiko-renderer
sbt -batch spike/compile "export spike/Runtime/fullClasspath" | tail -1 > /tmp/spike-cp.txt
python3 bench/gen-lorem.py > bench/lorem-bench.txt
CP="$(cat /tmp/spike-cp.txt)"
MAIN="com.serenity.spike.SkikoSpike --doc=bench/lorem-bench.txt"
```

Keep the same power profile as baselines A and B (powersave governor, on AC).

### Native libraries on NixOS

Skiko unpacks `libskiko-linux-x64.so` into `~/.skiko/` and loads it. On NixOS that library needs `libGL`, `libX11` and `fontconfig` on the loader path. For GL it also needs the driver from `/run/opengl-driver/lib`:

```bash
export LD_LIBRARY_PATH=/run/opengl-driver/lib:$(nix-build '<nixpkgs>' -A libGL --no-out-link)/lib:$(nix-build '<nixpkgs>' -A xorg.libX11 --no-out-link)/lib:$(nix-build '<nixpkgs>' -A fontconfig.lib --no-out-link)/lib
```

> Caution, here be imagine dragons: the exact set of libraries `libskiko` needs on NixOS has not been checked. If loading fails, run `ldd ~/.skiko/*/libskiko-linux-x64.so | grep 'not found'` and add what is missing.

### Toolkit and scale

- **Toolkit:** the spike calls Serenity's own `ToolkitSelection.install` first, and prints `RESULT env.toolkit`.
  - On stock OpenJDK, AWT always uses XToolkit (XWayland).
  - On a JetBrains Runtime, `SERENITY_TOOLKIT=x11` forces XToolkit and `SERENITY_TOOLKIT=wayland` forces WLToolkit.
- **SkiaLayer and WLToolkit:** `SkiaLayer`'s JVM backend is X11/GLX (#1812), so **expect WLToolkit to fail**. Run it once anyway to record how it fails.
- **2× scale:** pass `-Dsun.java2d.uiScale=2`, and check that `RESULT window.*.setup` prints `content_scale=2.0`.
  - Under XWayland at 2×, Hyprland upscales 1× X11 clients, which makes text blurry.
  - Set `xwayland { force_zero_scaling = true }` in `hyprland.conf` so the X11 client renders at native resolution.

### The runs (paste one at a time; each prints `RESULT` lines)

```bash
# 1. Shaping microbenchmark (no window)
java -Xmx2g -cp "$CP" $MAIN --mode=shaping --iterations=20 | tee spike-shaping.txt

# 2. Headless raster frame costs (no window)
java -Xmx2g -cp "$CP" $MAIN --mode=headless --screenshot=spike-headless.png | tee spike-headless.txt

# 3. Window: software raster, each pipeline
for p in direct picture raster; do
  SKIKO_RENDER_API=SOFTWARE_FAST SERENITY_TOOLKIT=x11 java -Xmx2g -Dsun.java2d.uiScale=2 -cp "$CP" $MAIN \
    --mode=window --pipeline=$p --screenshot=spike-sw-$p.png | tee spike-sw-$p.txt
done

# 4. Window: OpenGL (UHD 620), each pipeline
for p in direct picture raster; do
  SKIKO_RENDER_API=OPENGL SERENITY_TOOLKIT=x11 java -Xmx2g -Dsun.java2d.uiScale=2 -cp "$CP" $MAIN \
    --mode=window --pipeline=$p --screenshot=spike-gl-$p.png | tee spike-gl-$p.txt
done

# 5. Only on a JetBrains Runtime: WLToolkit, to record how it fails
SKIKO_RENDER_API=OPENGL SERENITY_TOOLKIT=wayland /path/to/jbr/bin/java -Xmx2g -cp "$CP" $MAIN --mode=window --pipeline=direct

# 6. Shaping-strategy variant of the best pipeline from 3/4 (rows shaped by TextLine.make, as a naive port would)
SKIKO_RENDER_API=OPENGL java -Xmx2g -Dsun.java2d.uiScale=2 -cp "$CP" $MAIN --mode=window --pipeline=raster --shaper=textline
```

Leave the keyboard and mouse alone during the `window` runs. The idle phase (10 s at the end) measures CPU with nothing changing. `onRender_calls` should be 0.

### IME check

Run the window by hand with an input method active:

```bash
# fcitx5 (or ibus: XMODIFIERS=@im=ibus)
XMODIFIERS=@im=fcitx GTK_IM_MODULE=fcitx SKIKO_RENDER_API=OPENGL SERENITY_TOOLKIT=x11 \
  java -Xmx2g -Dsun.java2d.uiScale=2 -cp "$CP" $MAIN --mode=interactive --pipeline=raster
```

Type through the input method. Use pinyin (`nihao` then space), or a compose sequence (`Compose ' e` → é).

**Pass:** the committed text appears at the caret, and the console logs `IME event: committed=...`, or the character arrives as a plain key.

The spike does not implement `InputMethodRequests`, so the candidate window may appear at the window corner rather than at the caret. That is expected here. It is a host-integration task for the real backend, not a Skiko limitation.

### 2× crispness check

1. Compare `spike-gl-direct.png`, the layer's own pixels from `SkiaLayer.screenshot()`, with what the compositor shows: `grim -g "$(slurp)" spike-screen.png` over the same region.
2. Zoom both to 400% and compare glyph edges.

**Pass:** the screen capture is as sharp as the layer's own pixels. If the compositor upscaled, the screen capture shows soft, doubled edges.

Also compare against the same text in Serenity proper (Java2D) at the same zoom.
