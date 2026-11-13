package com.serenity.perf

import java.awt.Font

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
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot, WrappedLineCache}
import com.serenity.ui.renderer.{CharacterRenderer, FontSpec, RendererCursorOverlay, RendererEntryPoints}
import com.serenity.ui.skia.SkiaRasterSurfaceFactory
import com.serenity.ui.terminal.SwingWindow
import com.serenity.ui.theme.Theme
import com.serenity.{FramePixels, SkiaFrames}

/** Layout, Skia raster frame and markdown benchmarks: everything that needs the shared fonts, frame pools and render
  * caches.
  */
private[perf] object RenderBenchmarks:

  given Balance = Balance.default

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

  private lazy val longMeasuredLine =
    TextLayoutSnapshot.visualLineForText(
      "Wi" * 8_000,
      bufferLine = 0,
      textFont,
      renderCaches.shapedText,
      renderCaches.wrappedLines.layoutMemos
    )

  def frameBenchmarks(cursorWindow: => SwingWindow): List[BenchmarkRunner.Benchmark] =
    lazy val richState        = editorStateForRichDocument(largeRichTextDocument(lines = 6_000))
    lazy val multilineState   = editorState(largeMultilineDocument(lines = 15_000), None)
    lazy val commentsState    = withDocumentComments(multilineState)
    lazy val diagnosticsState = withBenchmarkDiagnostics(commentsState)
    lazy val plainScrollState = scrolledToDeepViewport(multilineState)
    lazy val layoutSnapshot   = visibleViewportLayout(plainScrollState)
    lazy val unremembered = WrappedLineCache.measuring(renderCaches.shapedText, renderCaches.wrappedLines.layoutMemos)
    lazy val cursorBaseFrame        = prepareCursorBaseFrame(plainScrollState, cursorWindow, renderCaches)
    lazy val fullFrame              = renderedFrame(richState, deviceScale = 1.0, renderCaches)
    lazy val diagnosticsAndComments = renderedFrame(diagnosticsState, deviceScale = 1.0, renderCaches)
    lazy val hidpiFrame             = renderedFrame(commentsState, deviceScale = 2.0, renderCaches)
    lazy val longMeasuredLineFrame  = renderedLongMeasuredLine(longMeasuredLine)

    BenchmarkRunner.Benchmark.sharing(
      (
        richState,
        multilineState,
        commentsState,
        diagnosticsState,
        plainScrollState,
        layoutSnapshot,
        unremembered,
        cursorBaseFrame,
        fullFrame,
        diagnosticsAndComments,
        hidpiFrame,
        longMeasuredLineFrame
      )
    )(
      List(
        layoutRow(plainScrollState, layoutSnapshot, unremembered),
        BenchmarkRunner.Benchmark(
          "render.full_frame.skia_raster",
          2,
          8,
          () => assert(renderedFrameHasPixels(fullFrame)),
          () => renderedFrame(richState, deviceScale = 1.0, renderCaches)
        ),
        BenchmarkRunner.Benchmark(
          "render.long_measured_line.skia_raster",
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
          "render.diagnostics_and_comments.skia_raster",
          2,
          8,
          () => assert(renderedFrameHasPixels(diagnosticsAndComments)),
          () => renderedFrame(diagnosticsState, deviceScale = 1.0, renderCaches)
        ),
        hidpiRow(hidpiFrame, commentsState)
      )
    )

  private def layoutRow(
    plainScrollState: => AppState,
    layoutSnapshot: => Option[TextLayoutSnapshot],
    unremembered: => WrappedLineCache.Measuring
  ): BenchmarkRunner.Benchmark =
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
            wordWrapEnabled = false,
            wrapCache = unremembered
          )
        }
    )

  private def hidpiRow(hidpiFrame: => FramePixels, commentsState: => AppState): BenchmarkRunner.Benchmark =
    BenchmarkRunner.Benchmark(
      "render.hidpi_frame.skia_raster",
      2,
      8,
      () =>
        assert(
          hidpiFrame.widthPx == frameWidthPx * 2 &&
            hidpiFrame.heightPx == frameHeightPx * 2 &&
            renderedFrameHasPixels(hidpiFrame)
        ),
      () => renderedFrame(commentsState, deviceScale = 2.0, renderCaches)
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
    lazy val markdownLines  = largeMarkdownDocument(sections = 800)
    lazy val markdownSource = markdownLines.mkString("\n")
    // Shared across every markdown benchmark below, matching a real render's cache reuse across calls (#1677).
    lazy val markdownPreviewCache = MarkdownPreviewCache()
    lazy val markdownStateBase    = editorState(markdownSource, Some(LanguageId.Markdown))
    lazy val markdownState = markdownStateBase.copy(persisted =
      markdownStateBase.persisted.copy(config =
        AppConfig.conventional
          .withLineNumbers(false)
          .withoutStatusLine
          .withWordWrap(false)
          .withMarkdownViewMode(MarkdownViewMode.LivePreview)
      )
    )
    lazy val markdownHtmlFragment =
      MarkdownDocumentPreview.renderHtmlFragment(markdownSource.take(60_000), "benchmark", markdownPreviewCache)
    lazy val markdownLiveFrame = renderedFrame(markdownState, deviceScale = 1.0, renderCaches)

    BenchmarkRunner.Benchmark.sharing(
      (
        markdownLines,
        markdownSource,
        markdownPreviewCache,
        markdownStateBase,
        markdownState,
        markdownHtmlFragment,
        markdownLiveFrame
      )
    )(
      List(
        BenchmarkRunner.Benchmark(
          "markdown.preview.html_fragment",
          2,
          8,
          () => assert(markdownHtmlFragment.contains("<h2>")),
          () =>
            MarkdownDocumentPreview.renderHtmlFragment(markdownSource.take(60_000), "benchmark", markdownPreviewCache)
        ),
        BenchmarkRunner.Benchmark(
          "render.markdown.live_preview.skia_raster",
          2,
          BenchmarkIterationCounts.RenderMarkdown,
          () => assert(renderedFrameHasPixels(markdownLiveFrame)),
          () => renderedFrame(markdownState, deviceScale = 1.0, renderCaches)
        )
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

  private lazy val reusableFrames = Map(
    1.0 -> SkiaFrames.newFrame(frameWidthPx, frameHeightPx),
    2.0 -> SkiaFrames.newFrame(frameWidthPx * 2, frameHeightPx * 2)
  )

  private def renderedFrame(state: AppState, deviceScale: Double, caches: RenderCaches): FramePixels =
    val frame = reusableFrames(deviceScale)
    val surface = SkiaFrames.surfaceOver(
      frame,
      cellMetrics,
      monoFont,
      frameWidthPx,
      frameHeightPx,
      deviceScale,
      persists = false,
      caches
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
    frame

  private def renderedFrameHasPixels(frame: FramePixels): Boolean =
    frame.widthPx > 0 && frame.heightPx > 0 && ((frame.rgbAt(0, 0) >>> 24) & 0xff) > 0

  private def renderedLongMeasuredLine(line: com.serenity.state.models.TextVisualLine): FramePixels =
    val frame = SkiaFrames.newFrame(frameWidthPx, frameHeightPx)
    val surface =
      SkiaFrames.surfaceOver(
        frame,
        cellMetrics,
        textFont,
        frameWidthPx,
        frameHeightPx,
        1.0,
        persists = false,
        renderCaches
      )
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
    frame

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
      caches = caches,
      surfaces = SkiaRasterSurfaceFactory
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
