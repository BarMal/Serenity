package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.file.Paths

import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, AppMode}
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.AppState
import com.serenity.ui.layout.{CellMetrics, RichTextContext, TextLayoutSnapshot, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, Java2DRenderSurface, RendererEntryPoints}

/** The GUI cell grid -- the layout a monospaced font with ligatures off and an exact integer advance gets -- on plain
  * ASCII code, which must cost what it did, and on code with CJK, Hangul and symbols, which it now places on two cells
  * and draws in a fallback font, and with emoji and emoji clusters, which the bundled emoji face draws (#1866, #2365).
  * Courier Prime at 10 px has a 6 px advance, so it takes the cell layout.
  */
private[perf] object CellGridBenchmarks:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val Lines    = 40
  private val width    = 1200
  private val height   = 800
  private val codeFont = loadFont().deriveFont(10.0f)
  private val cells    = CellMetrics.fromFont(codeFont)
  private val viewport = ViewportSize(width / cells.charWidth, height / cells.lineHeight)
  private val frc      = TextLayoutSnapshot.defaultFontRenderContext()

  private def loadFont(): Font =
    Font.createFont(
      Font.TRUETYPE_FONT,
      Paths.get("src/main/resources/fonts/CourierPrime-Regular.ttf").toFile
    )

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

  private def stateOf(content: String): AppState =
    val config       = AppConfig.conventional.withAppMode(AppMode.Code)
    val (manager, _) = LaptopFrameBenchmarks.stateManagerHolding(content, config, grid = viewport)
    manager.getCurrentState.unsafeRunSync()

  /** A full frame of `state` at 1x, in the cell layout, drawn the way the laptop frame benchmarks draw one. */
  private def frameOf(state: AppState): () => BufferedImage =
    val image  = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val caches = RenderCaches.create()
    val surface = new Java2DRenderSurface(
      image,
      cells,
      codeFont,
      _ => (),
      logicalWidthPx = width,
      logicalHeightPx = height,
      deviceScaleX = 1.0,
      deviceScaleY = 1.0,
      textCaches = caches.surfaceText
    )
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
      image

  private def hasPixels(image: BufferedImage): Boolean =
    val row = image.getHeight / 4
    (0 until image.getWidth).exists(x => image.getRGB(x, row) != image.getRGB(0, row))

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

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    val asciiFrame    = frameOf(stateOf(text(asciiLine)))
    val mixedFrame    = frameOf(stateOf(text(mixedLine)))
    val emojiFrame    = frameOf(stateOf(text(emojiLine)))
    val clusterFrame  = frameOf(stateOf(text(clusterLine)))
    val clusterLayout = layoutOf(clusterLine)
    val asciiLayout   = layoutOf(asciiLine)
    val mixedLayout   = layoutOf(mixedLine)
    List(
      BenchmarkRunner.Benchmark(
        "render.cell_grid.frame_ascii",
        3,
        12,
        () => assert(hasPixels(asciiFrame()), "cell-grid ASCII frame drew nothing"),
        () => asciiFrame()
      ),
      BenchmarkRunner.Benchmark(
        "render.cell_grid.frame_mixed",
        3,
        12,
        () => assert(hasPixels(mixedFrame()), "cell-grid mixed frame drew nothing"),
        () => mixedFrame()
      ),
      BenchmarkRunner.Benchmark(
        "render.cell_grid.frame_emoji",
        3,
        12,
        () => assert(hasPixels(emojiFrame()), "cell-grid emoji frame drew nothing"),
        () => emojiFrame()
      ),
      BenchmarkRunner.Benchmark(
        "render.cell_grid.frame_emoji_clusters",
        3,
        12,
        () => assert(hasPixels(clusterFrame()), "cell-grid emoji cluster frame drew nothing"),
        () => clusterFrame()
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
