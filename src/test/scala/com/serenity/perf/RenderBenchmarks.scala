package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage

import com.serenity.config.{AppConfig, MarkdownViewMode}
import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.{MarkdownDocumentPreview, MarkdownPreviewCache}
import com.serenity.perf.BenchmarkFixtures.{
  editorState,
  editorStateForRichDocument,
  largeMarkdownDocument,
  largeMultilineDocument,
  largeRichTextDocument,
  scrolledToDeepViewport
}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import com.serenity.ui.renderer.{
  CharacterRenderer,
  FontSpec,
  Java2DRenderSurface,
  RendererCursorOverlay,
  RendererEntryPoints
}
import com.serenity.ui.terminal.SwingWindow
import com.serenity.ui.theme.Theme

/** Layout, Java2D frame and markdown benchmarks: everything that needs the shared fonts, frame pools and render caches.
  */
private[perf] object RenderBenchmarks:

  given Balance = Balance.default

  private val reusableFramePools = Map(
    1.0 -> new SwingWindow.ReusableImagePool,
    2.0 -> new SwingWindow.ReusableImagePool
  )

  private val monoFont         = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont         = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont           = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  val cellMetrics: CellMetrics = CellMetrics.fromFont(monoFont)
  val uiMetrics: CellMetrics   = CellMetrics.fromFont(uiFont)
  private val viewportSize     = BenchmarkFixtures.viewportSize
  private val frameWidthPx     = viewportSize.width * cellMetrics.charWidth
  private val frameHeightPx    = viewportSize.height * cellMetrics.lineHeight

  // Shared across the frame and markdown benchmarks, mirroring StateManager's cache reuse across renders (#1677).
  private val renderCaches = RenderCaches.create()

  def frameBenchmarks(cursorWindow: SwingWindow): List[BenchmarkRunner.Benchmark] =
    val longMeasuredLine = TextLayoutSnapshot.visualLineForText(
      "Wi" * 8_000,
      bufferLine = 0,
      textFont
    )
    val richState        = editorStateForRichDocument(largeRichTextDocument(lines = 6_000))
    val multilineState   = editorState(largeMultilineDocument(lines = 15_000), None)
    val commentsState    = withDocumentComments(multilineState)
    val diagnosticsState = withBenchmarkDiagnostics(commentsState)
    val plainScrollState = scrolledToDeepViewport(multilineState)
    val layoutSnapshot   = visibleViewportLayout(plainScrollState)
    prepareCursorBaseFrame(plainScrollState, cursorWindow, renderCaches)
    val fullFrame              = renderedFrame(richState, deviceScale = 1.0, renderCaches)
    val diagnosticsAndComments = renderedFrame(diagnosticsState, deviceScale = 1.0, renderCaches)
    val hidpiFrame             = renderedFrame(commentsState, deviceScale = 2.0, renderCaches)
    val longMeasuredLineFrame  = renderedLongMeasuredLine(longMeasuredLine)

    List(
      BenchmarkRunner.Benchmark(
        "layout.large_multiline.visible_viewport",
        3,
        BenchmarkIterationCounts.LayoutVisibleViewport,
        () => assert(layoutSnapshot.exists(_.visualLines.size == viewportSize.height)),
        () =>
          plainScrollState.persisted.buffers.get(BufferId(1)).foreach { buffer =>
            val _ = com.serenity.ui.layout.TextLayoutSnapshot.fromBuffer(
              buffer,
              panelWidthPx = frameWidthPx,
              monoFont,
              wordWrapEnabled = false
            )
          }
      ),
      BenchmarkRunner.Benchmark(
        "render.full_frame.java2d",
        2,
        8,
        () => assert(renderedFrameHasPixels(fullFrame)),
        () => renderedFrame(richState, deviceScale = 1.0, renderCaches)
      ),
      BenchmarkRunner.Benchmark(
        "render.long_measured_line.java2d",
        2,
        8,
        () => assert(renderedFrameHasPixels(longMeasuredLineFrame)),
        () =>
          val _ = renderedLongMeasuredLine(longMeasuredLine)
          ()
      ),
      BenchmarkRunner.Benchmark(
        "render.cursor_only.scene_reuse.java2d_overlay",
        2,
        8,
        () => assert(renderedCursorOverlay(plainScrollState, cursorWindow, renderCaches)),
        () =>
          val _ = renderedCursorOverlay(plainScrollState, cursorWindow, renderCaches)
          ()
      ),
      BenchmarkRunner.Benchmark(
        "render.diagnostics_and_comments.java2d",
        2,
        8,
        () => assert(renderedFrameHasPixels(diagnosticsAndComments)),
        () => renderedFrame(diagnosticsState, deviceScale = 1.0, renderCaches)
      ),
      BenchmarkRunner.Benchmark(
        "render.hidpi_frame.java2d",
        2,
        8,
        () =>
          assert(
            hidpiFrame.getWidth == frameWidthPx * 2 &&
              hidpiFrame.getHeight == frameHeightPx * 2 &&
              renderedFrameHasPixels(hidpiFrame)
          ),
        () => renderedFrame(commentsState, deviceScale = 2.0, renderCaches)
      )
    )

  private def withDocumentComments(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.view.mapValues { buffer =>
      buffer.copy(annotations =
        buffer.annotations.copy(documentComments =
          (10 until 3_000 by 3)
            .map(line => DocumentComment(CursorPosition(line, 0), CursorPosition(line, 20), "note"))
            .toList
        )
      )
    }.toMap))

  private def withBenchmarkDiagnostics(state: AppState): AppState =
    state.copy(runtime =
      state.runtime.copy(languageService = LanguageServiceState(diagnosticsState = benchmarkDiagnosticsState))
    )

  private def visibleViewportLayout(state: AppState): Option[TextLayoutSnapshot] =
    state.persisted.buffers
      .get(BufferId(1))
      .map(buffer =>
        TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = frameWidthPx, monoFont, wordWrapEnabled = false)
      )

  def markdownBenchmarks(): List[BenchmarkRunner.Benchmark] =
    val markdownLines  = largeMarkdownDocument(sections = 800)
    val markdownSource = markdownLines.mkString("\n")
    // Shared across every markdown benchmark below, matching a real render's cache reuse across calls (#1677).
    val markdownPreviewCache = MarkdownPreviewCache()
    val markdownStateBase    = editorState(markdownSource, Some(LanguageId.Markdown))
    val markdownState = markdownStateBase.copy(persisted =
      markdownStateBase.persisted.copy(config =
        AppConfig.default
          .withLineNumbers(false)
          .withoutStatusLine
          .withWordWrap(false)
          .withMarkdownViewMode(MarkdownViewMode.InlineLens)
      )
    )
    val markdownPreviewWindow =
      MarkdownDocumentPreview.previewWindow(
        markdownLines,
        activeLine = Some(1_200),
        fallbackTopLine = 1_000,
        cache = markdownPreviewCache,
        maxSourceLines = 80
      )
    val markdownHtmlFragment =
      MarkdownDocumentPreview.renderHtmlFragment(markdownSource.take(60_000), "benchmark", markdownPreviewCache)
    val markdownLensFrame = renderedFrame(markdownState, deviceScale = 1.0, renderCaches)

    List(
      BenchmarkRunner.Benchmark(
        "markdown.preview.window_mapping",
        3,
        20,
        () => assert(markdownPreviewWindow.firstSourceLine >= 0 && markdownPreviewWindow.source.nonEmpty),
        () =>
          MarkdownDocumentPreview.previewWindow(
            markdownLines,
            activeLine = Some(1_200),
            fallbackTopLine = 1_000,
            cache = markdownPreviewCache,
            maxSourceLines = 80
          )
      ),
      BenchmarkRunner.Benchmark(
        "markdown.preview.html_fragment",
        2,
        8,
        () => assert(markdownHtmlFragment.contains("<h2>")),
        () => MarkdownDocumentPreview.renderHtmlFragment(markdownSource.take(60_000), "benchmark", markdownPreviewCache)
      ),
      BenchmarkRunner.Benchmark(
        "render.markdown.inline_lens",
        2,
        BenchmarkIterationCounts.RenderMarkdown,
        () => assert(renderedFrameHasPixels(markdownLensFrame)),
        () => renderedFrame(markdownState, deviceScale = 1.0, renderCaches)
      )
    )

  /** 2,000 synthetic LSP diagnostics attached to a single benchmark document, for the diagnostics-rendering fixture. */
  private def benchmarkDiagnosticsState: DiagnosticsState =
    val diagnostics = (0 until 2_000).toList.map { line =>
      com.serenity.lsp.model.Diagnostic(
        com.serenity.lsp.model.LspRange(
          com.serenity.lsp.model.LspPosition(line, 0),
          com.serenity.lsp.model.LspPosition(line, 8)
        ),
        Some(com.serenity.lsp.model.DiagnosticSeverity.Warning),
        s"benchmark diagnostic $line",
        Some("benchmark")
      )
    }
    DiagnosticsState(diagnostics = Map(DocumentUri("file:///benchmark.scala") -> diagnostics))

  private def renderedFrame(state: AppState, deviceScale: Double, caches: RenderCaches): BufferedImage =
    val image = reusableFramePools(deviceScale).acquire(
      math.ceil(frameWidthPx * deviceScale).toInt,
      math.ceil(frameHeightPx * deviceScale).toInt,
      BufferedImage.TYPE_INT_ARGB
    )
    val surface = new Java2DRenderSurface(
      image,
      cellMetrics,
      monoFont,
      _ => (),
      logicalWidthPx = frameWidthPx,
      logicalHeightPx = frameHeightPx,
      deviceScaleX = deviceScale,
      deviceScaleY = deviceScale
    )
    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      viewportSize,
      FontSpec.fromAwt(monoFont),
      FontSpec.fromAwt(textFont),
      cellMetrics,
      None,
      caches
    )
    reusableFramePools(deviceScale).publish(image)
    image

  private def renderedFrameHasPixels(image: BufferedImage): Boolean =
    image.getWidth > 0 && image.getHeight > 0 && ((image.getRGB(0, 0) >>> 24) & 0xff) > 0

  private def renderedLongMeasuredLine(line: com.serenity.state.models.TextVisualLine): BufferedImage =
    val image = new BufferedImage(frameWidthPx, frameHeightPx, BufferedImage.TYPE_INT_ARGB)
    val surface = new Java2DRenderSurface(
      image,
      cellMetrics,
      textFont,
      _ => (),
      logicalWidthPx = frameWidthPx,
      logicalHeightPx = frameHeightPx
    )
    surface.setFont(FontSpec.fromAwt(textFont))
    surface.clearViewport(Theme.light.background)
    CharacterRenderer.renderMeasuredLine(
      surface,
      xOriginPx = 0.0f,
      yPx = 0,
      lineHeightPx = cellMetrics.lineHeight,
      ascentPx = cellMetrics.ascent,
      line,
      Theme.light,
      clipRightXPx = Some(frameWidthPx.toFloat)
    )
    image

  private def prepareCursorBaseFrame(state: AppState, window: SwingWindow, caches: RenderCaches): Unit =
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      window,
      FontSpec.fromAwt(monoFont),
      FontSpec.fromAwt(textFont),
      FontSpec.fromAwt(uiFont),
      uiMetrics,
      cursorColor = None,
      repaintOnFlush = false,
      caches = caches
    )

  private def renderedCursorOverlay(state: AppState, window: SwingWindow, caches: RenderCaches): Boolean =
    RendererCursorOverlay.renderCursorOnly(
      state,
      cursorVisible = true,
      window,
      FontSpec.fromAwt(monoFont),
      FontSpec.fromAwt(textFont),
      FontSpec.fromAwt(uiFont),
      uiMetrics,
      None,
      caches = caches
    )

end RenderBenchmarks
