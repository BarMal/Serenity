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
| `headless` | No window. Rasterises into an offscreen raster `Surface` on the calling thread. Measures full frame (shaped rows cached), full frame with every row reshaped, fills only, and simulated typing: state update (insert + Serenity rewrap of one line), damaged-region frame, and input to rendered. Then scrolling: `scroll.wheel` (`--wheel-rows`, default 3, per event) and `scroll.page` (a viewport less one row per event, like PageDown), each half down and half back up. With `--scroll-blit=true` a scroll of at most half the viewport reuses the previous frame (see "Follow-up" below). |
| `window` | A `JFrame` with a `SkiaLayer`. Measures 120 forced full frames back to back, the same full frame paced at `--pace-ms` (`full_frame_paced`), 200 keys paced at 60 ms (input → state update → frame → end of `onRender`), paced `scroll.wheel` and `scroll.page` (full repaints), then idle CPU over `--idle-s`. |
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

Measured at 65c44cba, before the follow-up below. The follow-up's own container numbers are in "Container numbers per variant".

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

## Follow-up after the GO decision (#1812 comment 5983240820)

### Interactive repaint: what was wrong

**Symptom (laptop, GL, X11 toolkit):** `--mode=interactive --pipeline=direct` received every key, but the window kept showing its first frame.

**Cause: the scene ignored the layer's real size.**
- The renderer drew a fixed 1500×1000 logical scene, with the caret row centred at y ≈ 500.
- It ignored the `width`/`height` that `onRender` receives.
- Under Hyprland at 2× without `force_zero_scaling`, XWayland gives X11 clients a 1500×1000 logical screen. The JVM, at `uiScale=2`, asks for a 3000×2000 X window, and Hyprland tiles it down to the workspace. So the layer is about 740×470 logical.
- That shows only the top-left quarter of the scene. The caret row, and every edit on it, sit just below the visible area.

**Reproduced under Xvfb with `SOFTWARE_FAST` and `--window=740x470`.** Screenshots of the X root window, taken before and after typing 24 characters with `java.awt.Robot`:
- before the fix: **0 pixels differ**;
- after the fix: **18,717 pixels differ**, and the typed text shows at the caret.

At the default 1500×1000 size the old code repainted correctly. That is why the container runs never showed the bug.

> Caution, here be imagine dragons: that the laptop window was tiled to about 740×470 is inferred from Hyprland's XWayland scaling. It was not measured. The new `layer_px=`/`viewport_logical=` fields in `RESULT window.*.setup` now print the real size.

**Fix:**
- In the direct pipeline, `onRender` calls `renderer.resize(width / scale, height / scale)`, and the anchor row is centred in the real viewport.
- The picture and raster producers size themselves to the layer as first painted.
- Interactive mode now publishes an initial frame for the picture and raster pipelines. Before, those windows stayed blank until the first key.

**Consequence for the earlier laptop numbers:** if the window was tiled, every `window.*` number in comment 5982892864 rasterised a frame of roughly a quarter of the area. Re-run the window modes with the new setup line.

**Still unexplained:** the ~200 ms input-to-paint p95 late in the interactive run.

> Caution, here be imagine dragons: candidates are key-repeat bursts, or XWayland's Present fallback timing. Neither is verified.

### The GL "full frame 31.6 ms" is probably vsync, not text

- **Two GL measurements disagree.** On GL, `window.direct.typing.edt_frame_task` was **3.0 ms**, and in the direct pipeline every typing frame is a *full* redraw. The back-to-back `full_frame` series was **31.6 ms**.
- **What Skiko does.** Its `LinuxOpenGLRedrawer` (0.150.1 sources) waits on a frame limiter, then swaps with `setSwapInterval(1)`. The frame task ends after that swap.
- **The likely cause.** 120 frames requested back to back therefore wait for vblank. 31.6 ms is about two 60 Hz intervals. Typing was paced at 60 ms, so it never waited.

> Caution, here be imagine dragons: this is an inference from the source and the two numbers. The new `full_frame_paced.total` (one full frame every `--pace-ms`) measures it directly; `--vsync=false` (sets `skiko.vsync.enabled`) is the cross-check.

### How others keep full text frames cheap (research)

Sources were read through search results; the source pages themselves could not be fetched from the container.

- **Skia itself:**
  - An `SkTextBlob` is immutable and has a `uniqueID()`. The GPU backend's text-blob cache is keyed on it, and the cache drops an entry when the blob is deleted ([SkTextBlob.h](https://api.skia.org/SkTextBlob_8h_source.html), [GrTextBlobCache review](https://codereview.chromium.org/1055843002)).
  - Glyph masks live in a GPU atlas fed by the single-threaded strike cache ([GrTextBlob.cpp](https://chromium.googlesource.com/skia/+/master/src/gpu/text/GrTextBlob.cpp)).
  - Atlas textures are at most 2048 px and 8 MB by default (`fMaxTextureAtlasSize`, `fGlyphCacheTextureMaximumBytes`, [GrContextOptions.h](https://chromium.googlesource.com/skia/+/a3e2996b08344a896884e6de050f7a2f2b80a409/include/gpu/GrContextOptions.h)).
  - **Takeaway:** reusing the *same* blob objects across frames is what makes the GPU path cheap. The spike already does this per row.
- **Chrome (cc):**
  - Paint produces a display item list, which is reused while its content is unchanged.
  - Raster happens into GPU **tiles**, invalidated by region.
  - Scrolling moves tiles and rasterises only newly exposed tiles, on the compositor thread ([How cc works](https://chromium.googlesource.com/chromium/src/+/lkgr/docs/how_cc_works.md), [RenderingNG data structures](https://developer.chrome.com/docs/chromium/renderingng-data-structures)).
- **Flutter:**
  - `RepaintBoundary` gives a subtree its own display list.
  - The engine's `RasterCache` turns a picture or layer into an image only after it has been drawn on about 3 frames (`access_threshold`).
  - It also throttles how many cache images it builds per frame ([raster_cache.h](https://github.com/flutter/engine/blob/master/flow/raster_cache.h), [engine#16545](https://github.com/flutter/engine/pull/16545)).
- **Compose and Skiko:**
  - Each `GraphicsLayer` is a Skiko `RenderNode` that records an `SkPicture` and replays it until the content changes.
  - That saves CPU command generation, not GPU work ([Compose internals: RenderNode](https://composeinternals.com/rendernode-in-android); Skiko 0.150.1 ships `org.jetbrains.skiko.node.RenderNode`).
- **Zed / GPUI:**
  - Shaping goes through the OS, cached per (text, font).
  - A line-layout cache keeps the previous frame's and the current frame's layouts, and evicts what the next frame does not reuse.
  - Glyphs are rasterised once into a GPU atlas, with up to 16 sub-pixel variants, and drawn as one instanced call ([Zed blog: 120 FPS](https://zed.dev/blog/videogame)).
- **Terminals:**
  - Alacritty rasterises each glyph once into 1024² atlases and draws the grid in about two calls ([rendering pipeline](https://deepwiki.com/alacritty/alacritty/3.4-rendering-pipeline)).
  - WezTerm adds a shape cache on top of its glyph atlas ([font system](https://deepwiki.com/wezterm/wezterm/3.2.2-font-system)).
  - VS Code's terminal uses a texture atlas, and redraws only cells whose recorded state changed ([terminal renderer](https://code.visualstudio.com/blogs/2017/10/03/terminal-renderer)).
  - VS Code's editor virtualises lines to the viewport ([GPU line cache issue](https://github.com/microsoft/vscode/issues/234433)).

**What matters for a text editor:**
1. Shape once per paragraph and keep the glyph runs (every system above does this).
2. Draw the *same* blob objects every frame, so the GPU atlas and blob cache hit.
3. Only on a CPU rasteriser, or if the GPU path proves too slow, cache *pixels*: per-paragraph images or tiles, plus scroll by moving pixels.

Display-list reuse (a picture per paragraph, a `RenderNode`) saves recording time, not raster time.

### Cache variants (`--cache=`, `--scroll-blit=`)

| Flag | What it reuses across frames |
|---|---|
| `--cache=rows` (default) | One `TextBlob` and caret array per row text: the original behaviour. |
| `--cache=blob` | (a) One `TextBlob` per paragraph, keyed by `ParagraphKey(text, wrap width, font size, scale)`, with every wrapped row's glyphs at their row offsets: one draw per paragraph. |
| `--cache=picture` | (b) One recorded `Picture` per paragraph, same key, drawn with `drawPicture` (a nested op, not inlined). |
| `--cache=image` | (d) One raster `Image` per paragraph at device scale: the text on transparent, blitted pixel-aligned. This is cc's tiles or Flutter's raster cache at paragraph granularity. |
| `--scroll-blit=true` | (c) Headless only. Two raster surfaces. A scroll of at most half the viewport draws the previous frame shifted, then paints only the exposed band. |

Notes on the variants:
- **Scroll blit is pixel-exact:** `scroll.blit_vs_full_differing_pixels` is 0 against a full repaint.
- **Pixel parity with `rows`:** `blob` and `picture` are pixel-identical. `image` differs in 24 pixels of 6 M (glyph edges at image bounds).
- **Already in place for (d):** the renderer already allocates `Font`/`Paint` once, so there was nothing to gain there. The GPU resource cache can be raised with `-Dskiko.gpu.resourceCacheLimit=512M` (Skiko's `SkiaLayerProperties.gpuResourceCacheLimit`).
- **No GPU-side scroll blit or layer cache in `SkiaLayer`.** Skiko records `onRender` into a picture, and does not expose the `DirectContext` there. So there is nowhere to keep a GPU surface between frames, short of reflection on Skiko internals.

### Container numbers per variant (NOT comparable to the laptop)

Container: 4 cores, no GPU, Ubuntu OpenJDK 21. All values are ms, p50 / p95.

**Headless raster** (3000×2000, `--shaper=explicit`):

| Variant | Full frame (warm) | Full frame, paragraph cache cold | Typing, damaged frame | Wheel scroll (3 rows) | Wheel scroll + blit | Page scroll (46 rows) |
|---|---|---|---|---|---|---|
| rows | 21.8 / 23.6 | — | 1.9 / 2.4 | 23.2 / 25.8 | **3.6 / 4.5** | 21.6 / 26.1 |
| blob | 19.6 / 21.5 | 20.9 / 22.5 | 1.9 / 2.2 | 21.5 / 23.3 | 3.5 / 4.4 | 22.1 / 26.6 |
| picture | 20.8 / 23.6 | 19.9 / 22.1 | 2.0 / 2.4 | 23.3 / 24.7 | 3.7 / 4.5 | 23.7 / 26.0 |
| image | **3.5 / 3.8** | 37.9 / 42.7 | 2.8 / 4.0 | 3.6 / 7.2 | 2.5 / 6.5 | 5.9 / **37.3** |

- Fills only: 1.2–1.3.
- Every-row reshape: 23–24.

**Window, Xvfb, `SOFTWARE_FAST`, direct pipeline** (3000×2000 layer; frame task includes Skiko's raster and the X11 put):

| Variant | Full frame (back to back) | Full frame (paced 60 ms) | Wheel scroll | Page scroll | Typing input-to-paint | Idle CPU |
|---|---|---|---|---|---|---|
| rows | 30.7 / 34.1 | 32.2 / 36.5 | 34.7 / 42.0 | 32.7 / 40.2 | 34.5 / 40.3 | 0.0% |
| blob | 31.6 / 36.9 | 33.5 / 36.8 | 34.0 / 38.3 | 33.5 / 40.3 | 34.4 / 39.0 | 0.2% |
| picture | 33.7 / 39.4 | 34.5 / 42.1 | 33.9 / 37.6 | 32.1 / 36.2 | 33.9 / 40.4 | 0.2% |
| image | **12.2 / 14.6** | 20.0 / 22.6 | 20.1 / 25.4 | 19.0 / 53.7 | **22.4 / 26.5** | 0.2% |

The window runs used 150 keys and a 5 s idle phase.

**Reading these:**
- **On a CPU rasteriser, only pixel reuse helps.** Glyph masks already come from Skia's strike cache, so per-frame cost is compositing about 60k glyph masks. Fewer draw calls (`blob`) or reused recordings (`picture`) save at most 10%.
- **The paragraph image cache cuts the warm full frame 6× (21.8 → 3.5).** The cost:
  - a cold build of about 38 ms per screenful;
  - page-jump p95 of 37 ms, when a jump brings in unseen paragraphs;
  - the memory of one ~2.8k×(rows×42) image per visible paragraph.
- **Scroll blit cuts a 3-row wheel step 6× (23 → 3.6), and is exact.**
- **The software window has a floor.** Even with `image`, the software window stays near 20 ms per paced frame: Skiko's software redrawer copies 24 MB to X11 every frame. That floor is not Skia text.

> Caution, here be imagine dragons: none of this predicts GL. On the GPU, glyphs come from the atlas, and an image cache becomes textures, which Ganesh caches by image ID (inferred, not measured). The laptop `full_frame_paced` and `scroll.*` numbers per variant decide whether GL needs any of it.

### Recommended caching design for the real renderer (pending the laptop re-run)

1. **Retain shaped glyph runs per paragraph.** Keep a `TextBlob` plus a caret array, keyed by (text, font set, wrap width, scale). Evict Zed-style: whatever the last frame did not reuse goes. This is plan step 5 (stored glyph runs) and is needed whatever happens next.
2. **Draw those same blob objects every frame on GL, with no pixel caching**, as long as the laptop's paced full frame is ≤ 8 ms. Keep the GPU resource cache large enough for the atlas: Skiko's `gpuResourceCacheLimit`.
3. **Only if GL misses 8 ms, or for the software fallback:** add a per-paragraph raster cache, Flutter-style.
   - Build an image only for paragraphs seen on 2–3 frames.
   - Build at most N per frame.
   - Add scroll-by-blit.

   Both need an offscreen surface that survives between frames. `SkiaLayer` does not give one on GPU, so this means owning the GL context or surface, which is a host-integration task.
4. **Drop per-paragraph `Picture` caching.** It costs memory and gives no raster win.

## Running on the laptop (NixOS, Hyprland, UHD 620)

### Get the prebuilt jar (no sbt on the laptop)

Run the workflow, wait for it, and download the bundle. The spike jar carries Skiko and its linux-x64 native runtime.

```bash
gh workflow run bench-artifacts.yml -R BarMal/Serenity -f ref=master -f spike_ref=spike/skiko-renderer
gh run list -R BarMal/Serenity --workflow=bench-artifacts.yml -L 1      # note the run id
gh run watch <run-id> -R BarMal/Serenity
gh run download <run-id> -R BarMal/Serenity -n bench-<sha> -D bench-<sha> && cd bench-<sha>   # <sha>: first 7 of master
grep 'spike commit' MANIFEST.txt                                        # must be this branch's head
python3 bench/gen-lorem.py > bench/lorem-bench.txt
CP="$PWD/spike/serenity-spike.jar"
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

Do the zero-scaling config change from the crispness check below **before** the window runs if you can. Otherwise Hyprland tiles the window to about 740×470 logical, and every window number is for a quarter-size frame. The `setup` line's `layer_px` shows which case you got.

```bash
# 1. Headless raster: every cache variant, scroll with and without the blit (no window)
for c in rows blob picture image; do for b in false true; do
  java -Xmx2g -cp "$CP" $MAIN --mode=headless --cache=$c --scroll-blit=$b | tee spike-headless-$c-blit$b.txt
done; done

# 2. Window, OpenGL, direct pipeline, every cache variant: full frame back to back AND paced, wheel/page scroll,
#    typing, idle. Check the setup line's layer_px / viewport_logical: the size the frame really was.
for c in rows blob picture image; do
  SKIKO_RENDER_API=OPENGL SERENITY_TOOLKIT=x11 java -Xmx2g -Dsun.java2d.uiScale=2 -cp "$CP" $MAIN \
    --mode=window --pipeline=direct --cache=$c --screenshot=spike-gl-$c.png | tee spike-gl-$c.txt
done

# 3. Vsync cross-check for the full-frame number: the same run with Skiko's vsync off
SKIKO_RENDER_API=OPENGL SERENITY_TOOLKIT=x11 java -Xmx2g -Dsun.java2d.uiScale=2 -cp "$CP" $MAIN \
  --mode=window --pipeline=direct --vsync=false | tee spike-gl-novsync.txt

# 4. Bigger GPU resource cache (matters for --cache=image, whose images become textures)
SKIKO_RENDER_API=OPENGL SERENITY_TOOLKIT=x11 java -Xmx2g -Dsun.java2d.uiScale=2 -Dskiko.gpu.resourceCacheLimit=512M \
  -cp "$CP" $MAIN --mode=window --pipeline=direct --cache=image | tee spike-gl-image-512m.txt

# 5. Interactive repaint check (the bug fixed in this follow-up): type a few words; the text must appear at the caret
SKIKO_RENDER_API=OPENGL SERENITY_TOOLKIT=x11 java -Xmx2g -Dsun.java2d.uiScale=2 -cp "$CP" $MAIN \
  --mode=interactive --pipeline=direct
```

Leave the keyboard and mouse alone during the `window` runs. The idle phase at the end measures CPU with nothing changing, and `onRender_calls` should be 0. For the earlier comparisons (software redrawer, picture and raster pipelines, shaping), the commands in comment 5982892864 still work with `CP` set as above.

### IME check (ibus running)

Start ibus first, with an engine that composes: pinyin, or any engine with dead keys. Then run the window by hand:

```bash
ibus-daemon -drx                         # skip if ibus is already running; `ibus engine` prints the active engine
XMODIFIERS=@im=ibus GTK_IM_MODULE=ibus SKIKO_RENDER_API=OPENGL SERENITY_TOOLKIT=x11 \
  java -Xmx2g -Dsun.java2d.uiScale=2 -cp "$CP" $MAIN --mode=interactive --pipeline=direct
```

Type through the input method: pinyin `nihao` then space, or a dead-key sequence that gives é.

**Pass:** the committed text appears at the caret, and the console logs `IME event: committed=...`, or the character arrives as a plain key.

The spike does not implement `InputMethodRequests`, so the candidate window may appear at the window corner rather than at the caret. That is expected here. It is a host-integration task for the real backend, not a Skiko limitation.

### 2× crispness check (owner sets the config by hand)

Hyprland 0.55 rejects `hyprctl keyword` at runtime, so set zero scaling in the config file for this run:

```
# hyprland.conf
xwayland {
  force_zero_scaling = true
}
```

Reload Hyprland (or log out and in), then:

```bash
SKIKO_RENDER_API=OPENGL SERENITY_TOOLKIT=x11 java -Xmx2g -Dsun.java2d.uiScale=2 -cp "$CP" $MAIN \
  --mode=window --pipeline=direct --screenshot=spike-gl-zero-scaling.png | tee spike-gl-zero-scaling.txt
grim -g "$(slurp)" spike-screen.png      # while the window is up: select the text area
```

1. With zero scaling, `layer_px` should match the window's physical size, and `content_scale=2.0`.
2. Compare `spike-gl-zero-scaling.png` (the layer's own pixels, from `SkiaLayer.screenshot()`) with `spike-screen.png` (the compositor's) at 400%.

**Pass:** the screen capture is as sharp as the layer's own pixels. If the compositor upscaled, the screen capture shows soft, doubled edges.

Also compare against the same text in Serenity proper (Java2D) at the same zoom. Revert the config line afterwards if the rest of the desktop needs scaled X11 apps.
