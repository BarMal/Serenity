package com.serenity.perf

import cats.effect.unsafe.implicits.global
import com.serenity.{ChromeOn, FramePixels, SkiaFrames}
import com.serenity.config.{AppConfig, AppMode, DockedCornerStyle}
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.AppState
import com.serenity.ui.layout.{CellMetrics, RichTextContext, TextLayoutSnapshot, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints}

/** The GUI cell grid -- the layout a monospaced font with ligatures off and an exact integer advance gets -- on plain
  * ASCII code, which must cost what it did, and on code with CJK, Hangul and symbols, which it now places on two cells
  * and draws in a fallback font, and with emoji and emoji clusters, which the bundled emoji face draws (#1866, #2365).
  * Courier Prime at 10 px has a 6 px advance, so it takes the cell layout.
  */
private[perf] object CellGridBenchmarks:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val Lines         = 40
  private val width         = 1200
  private val height        = 800
  private lazy val codeFont = BenchmarkFonts.courierPrimeRegular().deriveFont(10.0f)
  private lazy val cells    = CellMetrics.fromFont(codeFont)
  private lazy val viewport = ViewportSize(width / cells.charWidth, height / cells.lineHeight)
  private lazy val frc      = TextLayoutSnapshot.defaultFontRenderContext()

  private def asciiLine(index: Int): String =
    s"val value$index = compute(input, $index) // plain ASCII code line, no wide characters at all"

  private def mixedLine(index: Int): String =
    if index % 2 == 0 then asciiLine(index)
    else s"val greeting$index = \"你好，世界 こんにちは 안녕하세요 → ★\" // mixed $index"

  private def emojiLine(index: Int): String =
    if index % 2 == 0 then asciiLine(index)
    else s"val reaction$index = \"\ud83d\ude00 \ud83d\ude80 \ud83c\udf89 \u2728\" // emoji $index"

  private def clusterLine(index: Int): String =
    if index % 2 == 0 then asciiLine(index)
    else
      s"val clusters$index = \"\u2764\ufe0f \ud83d\udc4d\ud83c\udffd \ud83d\udc68\u200d\ud83d\udc69\u200d\ud83d\udc67 " +
        s"\ud83c\udff3\ufe0f\u200d\ud83c\udf08 \ud83c\uddfa\ud83c\uddf8\" // clusters $index"

  private def text(line: Int => String): String = Vector.tabulate(Lines)(line).mkString("\n")

  /** `ChromeOn.config`, like every other frame fixture: flush chrome, so these rows keep pinning the rectangles they
    * always measured. Floating, rounded chrome has its own row.
    */
  private def stateOf(content: String, chrome: AppConfig = ChromeOn.config): AppState =
    val config       = chrome.withAppMode(AppMode.Code)
    val (manager, _) = LaptopFrameBenchmarks.stateManagerHolding(content, config, grid = viewport)
    manager.getCurrentState.unsafeRunSync()

  /** A full frame of `state` at 1x, in the cell layout, drawn the way the laptop frame benchmarks draw one. */
  private def frameOf(state: AppState): () => FramePixels =
    val frame   = SkiaFrames.newFrame(width, height)
    val caches  = RenderCaches.create()
    val surface = SkiaFrames.surfaceOver(frame, cells, codeFont, width, height, 1.0, persists = false, caches)
    () =>
      RendererEntryPoints.render(
        state,
        false,
        surface,
        viewport,
        FontSpec.fromAwt(codeFont),
        FontSpec.fromAwt(codeFont),
        cells,
        None,
        caches
      )
      frame

  private def hasPixels(frame: FramePixels): Boolean =
    val pixels = frame.rgbPixels
    val row    = frame.heightPx / 4 * frame.widthPx
    (0 until frame.widthPx).exists(x => pixels(row + x) != pixels(row))

  private def layoutOf(line: Int => String): () => Int =
    val lines = Vector.tabulate(Lines)(line)
    () =>
      lines
        .map(content =>
          TextLayoutSnapshot
            .boundedVisualLinesForText(
              content,
              0,
              width,
              codeFont,
              frc,
              forceCellLayout = true,
              richText = RichTextContext.plain
            )
            .size
        )
        .sum

  /** The frame rows settle the JIT before sampling, as the layout rows do. A frame runs the chrome's code once, and
    * unsettled they timed the first few hundred frames of a JVM, where that code is still interpreted.
    */
  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    lazy val asciiFrame    = frameOf(stateOf(text(asciiLine)))
    lazy val mixedFrame    = frameOf(stateOf(text(mixedLine)))
    lazy val emojiFrame    = frameOf(stateOf(text(emojiLine)))
    lazy val clusterFrame  = frameOf(stateOf(text(clusterLine)))
    lazy val clusterLayout = layoutOf(clusterLine)
    lazy val asciiLayout   = layoutOf(asciiLine)
    lazy val mixedLayout   = layoutOf(mixedLine)
    BenchmarkRunner.Benchmark.sharing(
      (asciiFrame, mixedFrame, emojiFrame, clusterFrame, clusterLayout, asciiLayout, mixedLayout)
    )(
      List(
        BenchmarkRunner.Benchmark(
          "render.cell_grid.frame_ascii.skia_raster",
          3,
          12,
          () => assert(hasPixels(asciiFrame()), "cell-grid ASCII frame drew nothing"),
          () => asciiFrame(),
          settleJit = true
        ),
        BenchmarkRunner.Benchmark(
          "render.cell_grid.frame_mixed.skia_raster",
          3,
          12,
          () => assert(hasPixels(mixedFrame()), "cell-grid mixed frame drew nothing"),
          () => mixedFrame(),
          settleJit = true
        ),
        BenchmarkRunner.Benchmark(
          "render.cell_grid.frame_emoji.skia_raster",
          3,
          12,
          () => assert(hasPixels(emojiFrame()), "cell-grid emoji frame drew nothing"),
          () => emojiFrame(),
          settleJit = true
        ),
        BenchmarkRunner.Benchmark(
          "render.cell_grid.frame_emoji_clusters.skia_raster",
          3,
          12,
          () => assert(hasPixels(clusterFrame()), "cell-grid emoji cluster frame drew nothing"),
          () => clusterFrame(),
          settleJit = true
        ),
        BenchmarkRunner.Benchmark(
          "layout.cell_grid.emoji_cluster_lines",
          BenchmarkIterationCounts.LayoutVisibleViewport,
          BenchmarkIterationCounts.LayoutVisibleViewport,
          () => assert(clusterLayout() >= Lines),
          () => clusterLayout(),
          settleJit = true
        ),
        BenchmarkRunner.Benchmark(
          "layout.cell_grid.ascii_lines",
          BenchmarkIterationCounts.LayoutVisibleViewport,
          BenchmarkIterationCounts.LayoutVisibleViewport,
          () => assert(asciiLayout() >= Lines),
          () => asciiLayout(),
          settleJit = true
        ),
        BenchmarkRunner.Benchmark(
          "layout.cell_grid.mixed_lines",
          BenchmarkIterationCounts.LayoutVisibleViewport,
          BenchmarkIterationCounts.LayoutVisibleViewport,
          () => assert(mixedLayout() >= Lines),
          () => mixedLayout(),
          settleJit = true
        )
      )
    )

  /** The same plain ASCII frame in floating, rounded chrome, which has no flush base to compare with. Kept apart from
    * [[benchmarks]] so the rows after the cell-grid ones keep the places the warm-up order pins them to.
    */
  def floatingChromeBenchmarks(): List[BenchmarkRunner.Benchmark] =
    lazy val frame = frameOf(
      stateOf(text(asciiLine), ChromeOn.config.withDockedCornerStyle(DockedCornerStyle.Floating))
    )
    List(
      BenchmarkRunner.Benchmark.building(frame)(
        BenchmarkRunner.Benchmark(
          "render.cell_grid.frame_ascii.floating_chrome.skia_raster",
          3,
          12,
          () => assert(hasPixels(frame()), "cell-grid ASCII frame in floating chrome drew nothing"),
          () => frame(),
          settleJit = true
        )
      )
    )
