package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}

import cats.effect.{IO, Resource}
import com.serenity.animation.*
import com.serenity.config.{AppConfig, MarkdownViewMode}
import com.serenity.keystroke.events.{
  DeleteBackward,
  DeleteWordBackward,
  ExtendSelectionRight,
  InsertChar,
  MoveDown,
  MoveRight,
  ScrollDown
}
import com.serenity.lsp.client.LspFramer
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.perf.BenchmarkFixtures.{
  deepViewport,
  editorState,
  editorStateForRichDocument,
  largeFindDocument,
  largeMarkdownDocument,
  largeMultilineDocument,
  largeRichTextDocument,
  largeSingleLineJson,
  withCursorsOnConsecutiveLines
}
import com.serenity.project.{ProjectTaskDetector, ProjectTaskKind, ProjectTaskTerminal}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.{EditorEventReducer, ModalEventReducer}
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import com.serenity.ui.renderer.{CharacterRenderer, Java2DRenderSurface, Renderer}
import com.serenity.ui.terminal.SwingWindow
import com.serenity.ui.theme.Theme
import io.circe.Json

object PerformanceBenchmarks:

  private val reusableFramePools = Map(
    1.0 -> new SwingWindow.ReusableImagePool,
    2.0 -> new SwingWindow.ReusableImagePool
  )

  given Balance = Balance.default

  private val monoFont      = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont      = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont        = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val cellMetrics   = CellMetrics.fromFont(monoFont)
  private val uiMetrics     = CellMetrics.fromFont(uiFont)
  private val viewportSize  = BenchmarkFixtures.viewportSize
  private val frameWidthPx  = viewportSize.width * cellMetrics.charWidth
  private val frameHeightPx = viewportSize.height * cellMetrics.lineHeight

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global

    SwingWindow
      .resource(metrics = cellMetrics, chromeMetrics = uiMetrics)
      .flatMap(window => projectTaskFixtureResource.map(projectRoot => window -> projectRoot))
      .use {
        case (window, projectRoot) =>
          IO {
            val results = benchmarks(window, projectRoot).map(BenchmarkRunner.runBenchmark)
            BenchmarkRunner.printResults(results)
          }
      }
      .unsafeRunSync()

  /** Reducer benchmarks, extracted so `benchmarks` is not a single monolith and so the per-family coverage #993 depends
    * on is visible in one place.
    */
  private def reducerBenchmarks(
    editingState: AppState,
    plainScrollState: AppState,
    richScrollState: AppState,
    deepViewport: Viewport
  ): List[BenchmarkRunner.Benchmark] =
    val normalEditingResult    = EditorEventReducer.reduce(InsertChar('x'), PaneId(0), editingState)
    val backspaceResult        = EditorEventReducer.reduce(DeleteBackward, PaneId(0), editingState)
    val wordDeleteResult       = EditorEventReducer.reduce(DeleteWordBackward, PaneId(0), editingState)
    val moveRightResult        = EditorEventReducer.reduce(MoveRight, PaneId(0), editingState)
    val extendRightResult      = EditorEventReducer.reduce(ExtendSelectionRight, PaneId(0), editingState)
    val multiCursorState       = withCursorsOnConsecutiveLines(editingState, 50, fromLine = 5_000, column = 4)
    val multiCursorWrapState   = multiCursorState.copy(config = multiCursorState.config.withWordWrap(true))
    val multiInsertResult      = EditorEventReducer.reduce(InsertChar('x'), PaneId(0), multiCursorState)
    val multiMoveResult        = EditorEventReducer.reduce(MoveRight, PaneId(0), multiCursorState)
    val multiMoveDownResult    = com.serenity.VerticalNavSupport.dispatch(MoveDown, PaneId(0), multiCursorWrapState)
    val plainScrollResult      = EditorEventReducer.reduce(ScrollDown(40), PaneId(0), plainScrollState)
    val richScrollResult       = EditorEventReducer.reduce(ScrollDown(40), PaneId(0), richScrollState)
    val originalLine           = editingState.buffers.get(BufferId(1)).flatMap(_.document.content.getLine(6_000))
    val expectedEditedLine     = originalLine.map(_.patch(12, "x", 0))
    val expectedBackspacedLine = originalLine.map(_.patch(11, "", 1))

    def editedLine(result: com.serenity.state.reducers.ReducerResult): Option[String] =
      result.state.buffers.get(BufferId(1)).flatMap(_.document.content.getLine(6_000))

    def reducedBuffer(result: com.serenity.state.reducers.ReducerResult): Option[Buffer] =
      result.state.buffers.get(BufferId(1))

    def reducedCursor(result: com.serenity.state.reducers.ReducerResult): Option[CursorPosition] =
      reducedBuffer(result).flatMap(_.editing.cursors.headOption)

    def reducedSelection(result: com.serenity.state.reducers.ReducerResult): Option[Selection] =
      reducedBuffer(result).flatMap(_.primarySelection)

    List(
      BenchmarkRunner.Benchmark(
        "reducer.normal_editing",
        3,
        20,
        () =>
          assert(
            expectedEditedLine.exists(line => editedLine(normalEditingResult).contains(line))
          ),
        () => EditorEventReducer.reduce(InsertChar('x'), PaneId(0), editingState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.backspace",
        3,
        20,
        () =>
          assert(
            expectedBackspacedLine.exists(line => editedLine(backspaceResult).contains(line))
          ),
        () => EditorEventReducer.reduce(DeleteBackward, PaneId(0), editingState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.delete_word_backward",
        3,
        20,
        () =>
          assert(
            editedLine(wordDeleteResult).exists(_.length < expectedBackspacedLine.fold(0)(_.length))
          ),
        () => EditorEventReducer.reduce(DeleteWordBackward, PaneId(0), editingState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.arrow_navigation",
        3,
        20,
        () => assert(reducedCursor(moveRightResult).exists(_.column == 13)),
        () => EditorEventReducer.reduce(MoveRight, PaneId(0), editingState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.extend_selection",
        3,
        20,
        () => assert(reducedSelection(extendRightResult).exists(_.focus.column == 13)),
        () => EditorEventReducer.reduce(ExtendSelectionRight, PaneId(0), editingState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.multi_cursor_insert",
        3,
        20,
        () => assert(reducedBuffer(multiInsertResult).exists(_.editing.cursors.sizeIs == 50)),
        () => EditorEventReducer.reduce(InsertChar('x'), PaneId(0), multiCursorState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.multi_cursor_move",
        3,
        20,
        () => assert(reducedBuffer(multiMoveResult).exists(_.editing.cursors.forall(_.column == 5))),
        () => EditorEventReducer.reduce(MoveRight, PaneId(0), multiCursorState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.multi_cursor_move_down",
        3,
        20,
        () => assert(reducedBuffer(multiMoveDownResult).exists(_.editing.cursors.sizeIs == 50)),
        () => com.serenity.VerticalNavSupport.dispatch(MoveDown, PaneId(0), multiCursorWrapState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.deep_scroll.plain",
        3,
        20,
        () => assert(reducedTopLine(plainScrollResult) == Some(deepViewport.topLine + 40)),
        () => EditorEventReducer.reduce(ScrollDown(40), PaneId(0), plainScrollState)
      ),
      BenchmarkRunner.Benchmark(
        "reducer.deep_scroll.rich_text",
        3,
        20,
        () => assert(reducedTopLine(richScrollResult) == Some(deepViewport.topLine + 40)),
        () => EditorEventReducer.reduce(ScrollDown(40), PaneId(0), richScrollState)
      )
    )

  private def benchmarks(cursorWindow: SwingWindow, projectRoot: Path): List[BenchmarkRunner.Benchmark] =
    val jsonText       = largeSingleLineJson(entries = 20_000)
    val multilineText  = largeMultilineDocument(lines = 15_000)
    val findText       = largeFindDocument(matches = 12_000)
    val markdownLines  = largeMarkdownDocument(sections = 800)
    val markdownSource = markdownLines.mkString("\n")
    val longMeasuredLine = TextLayoutSnapshot.visualLineForText(
      "Wi" * 8_000,
      bufferLine = 0,
      textFont
    )
    val richDocument   = largeRichTextDocument(lines = 6_000)
    val richState      = editorStateForRichDocument(richDocument)
    val multilineState = editorState(multilineText, None)
    val markdownState = editorState(markdownSource, Some(LanguageId.Markdown)).copy(
      config = AppConfig.default
        .withLineNumbers(false)
        .withGutter(false)
        .withWordWrap(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val commentsState = multilineState.copy(
      buffers = multilineState.buffers.view.mapValues { buffer =>
        buffer.copy(annotations =
          buffer.annotations.copy(documentComments =
            (10 until 3_000 by 3)
              .map(line => DocumentComment(CursorPosition(line, 0), CursorPosition(line, 20), "note"))
              .toList
          )
        )
      }.toMap
    )
    val diagnosticsState = commentsState.copy(
      diagnostics = Map(
        "file:///benchmark.scala" ->
          (0 until 2_000).toList.map { line =>
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
      )
    )
    val plainScrollState = multilineState.copy(
      buffers = multilineState.buffers.view.mapValues(_.copy(viewport = deepViewport)).toMap
    )
    val deepRichDocument = largeRichTextDocument(lines = 15_000)
    val deepRichState    = editorStateForRichDocument(deepRichDocument)
    val richScrollState = deepRichState.copy(
      buffers = deepRichState.buffers.view
        .mapValues(_.copy(viewport = deepViewport))
        .toMap
    )
    val findState = editorState(findText, None)
    val editingState = findState.copy(
      buffers = findState.buffers.view
        .mapValues(buffer => buffer.copy(editing = buffer.editing.copy(cursors = List(CursorPosition(6_000, 12)))))
        .toMap
    )
    val jsonSearchResults = Rope(jsonText).searchAll("\"k19999\"")
    val jsonCursorOffset  = Rope(jsonText).lineColumnToOffset(0, jsonText.length - 5)
    val layoutSnapshot = plainScrollState.buffers
      .get(BufferId(1))
      .map(buffer =>
        com.serenity.ui.layout.TextLayoutSnapshot.fromBuffer(
          buffer,
          panelWidthPx = frameWidthPx,
          monoFont,
          wordWrapEnabled = false
        )
      )
    val findResultSet = FindResultSet.normalized(
      "needle",
      (0 until 12_000).toList.map(line => FindResult(line, 10)),
      requestedIndex = 6_000
    )
    val findQuerySurfaceId = SurfaceId("benchmark-find")
    val findQueryState = findState.copy(
      uiSurfaces = List(
        UiSurface(
          findQuerySurfaceId,
          SurfaceContent.ModalWorkflow(Modal.Find("needle", Nil, 0)),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(findQuerySurfaceId)
    )
    val findQueryRequest = FindSearchRequest(
      findQuerySurfaceId,
      BufferId(1),
      "needle",
      findQueryState.buffers(BufferId(1)).document.content
    )
    val completeFindQuery = ModalEventReducer.applyFindSearchResults(
      findQueryState,
      findQueryRequest,
      FindSearch.results(findQueryRequest.content, findQueryRequest.query)
    )
    val findKeystrokeState = findQueryState.copy(
      uiSurfaces = List(
        UiSurface(
          findQuerySurfaceId,
          SurfaceContent.ModalWorkflow(Modal.Find("needl", Nil, 0)),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )
    val findKeystrokeResult = ModalEventReducer.reduce(ModalType.Find, InsertChar('e'), findKeystrokeState)
    val lspMessages = (1 to 250).toList.map { id =>
      Json.obj("jsonrpc" -> Json.fromString("2.0"), "id" -> Json.fromInt(id), "method" -> Json.fromString("benchmark"))
    }
    val framedLspMessages = lspMessages.flatMap(LspFramer.encode).toArray
    val projectTask       = ProjectTaskDetector.detect(projectRoot, ProjectTaskKind.Test)
    prepareCursorBaseFrame(plainScrollState, cursorWindow)
    val animationCells = multilineState.buffers
      .get(BufferId(1))
      .map(buffer =>
        com.serenity.state.manager.VisibleBufferAnimationCells.fromBuffer(
          buffer,
          wordWrapEnabled = false,
          startColor = Theme.light.muted,
          endColor = Theme.light.foreground
        )
      )
      .getOrElse(Map.empty)
    val animationState = AnimationState(
      FlowAnimationBuilder.build(animationCells, FlowDirection.ByColumn, SweepDirection.Forward, 12)
    )
    val fullFrame               = renderedFrame(richState, deviceScale = 1.0)
    val diagnosticsAndComments  = renderedFrame(diagnosticsState, deviceScale = 1.0)
    val hidpiFrame              = renderedFrame(commentsState, deviceScale = 2.0)
    val visibleFindResults      = findResultSet.visibleResults(maxResults = 80)
    val decodedLspMessages      = decodeLspMessages(framedLspMessages)
    val projectTaskPresentation = projectTask.map(ProjectTaskTerminal.started)
    val markdownPreviewWindow =
      MarkdownDocumentPreview.previewWindow(
        markdownLines,
        activeLine = Some(1_200),
        fallbackTopLine = 1_000,
        maxSourceLines = 80
      )
    val markdownHtmlFragment   = MarkdownDocumentPreview.renderHtmlFragment(markdownSource.take(60_000), "benchmark")
    val markdownLensFrame      = renderedFrame(markdownState, deviceScale = 1.0)
    val longMeasuredLineFrame  = renderedLongMeasuredLine(longMeasuredLine)
    val advancedAnimationState = animationState.advanceAllAnimations()

    List(
      BenchmarkRunner.Benchmark(
        "rope.large_json.search",
        3,
        12,
        () => assert(jsonSearchResults.nonEmpty),
        () => Rope(jsonText).searchAll("\"k19999\"")
      ),
      BenchmarkRunner.Benchmark(
        "rope.large_json.cursor_offset",
        3,
        20,
        () => assert(jsonCursorOffset == jsonText.length - 5),
        () => Rope(jsonText).lineColumnToOffset(0, jsonText.length - 5)
      ),
      BenchmarkRunner.Benchmark(
        "layout.large_multiline.visible_viewport",
        3,
        20,
        () => assert(layoutSnapshot.exists(_.visualLines.size == viewportSize.height)),
        () =>
          plainScrollState.buffers.get(BufferId(1)).foreach { buffer =>
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
        () => renderedFrame(richState, deviceScale = 1.0)
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
        () => assert(renderedCursorOverlay(plainScrollState, cursorWindow)),
        () =>
          val _ = renderedCursorOverlay(plainScrollState, cursorWindow)
          ()
      ),
      BenchmarkRunner.Benchmark(
        "render.diagnostics_and_comments.java2d",
        2,
        8,
        () => assert(renderedFrameHasPixels(diagnosticsAndComments)),
        () => renderedFrame(diagnosticsState, deviceScale = 1.0)
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
        () => renderedFrame(commentsState, deviceScale = 2.0)
      )
    ) ++ reducerBenchmarks(editingState, plainScrollState, richScrollState, deepViewport) ++ DamageBenchmarks
      .benchmarks() ++ List(
      BenchmarkRunner.Benchmark(
        "find_replace.large_result_set",
        3,
        20,
        () => assert(visibleFindResults.size == 80 && visibleFindResults.exists(_._1 == FindResult(6_000, 10))),
        () => findResultSet.visibleResults(maxResults = 80)
      ),
      BenchmarkRunner.Benchmark(
        "find_replace.large_query_update",
        3,
        20,
        () => assert(completeFindQuery.buffers(BufferId(1)).findState.exists(_.results.length == 12_000)),
        () =>
          ModalEventReducer.applyFindSearchResults(
            findQueryState,
            findQueryRequest,
            FindSearch.results(findQueryRequest.content, findQueryRequest.query)
          )
      ),
      BenchmarkRunner.Benchmark(
        "find_replace.large_query_keystroke",
        3,
        20,
        () => assert(findKeystrokeResult.effects.nonEmpty),
        () => ModalEventReducer.reduce(ModalType.Find, InsertChar('e'), findKeystrokeState)
      ),
      BenchmarkRunner.Benchmark(
        "lsp.framer.large_batch",
        3,
        12,
        () => assert(decodedLspMessages == lspMessages),
        () => decodeLspMessages(framedLspMessages)
      ),
      BenchmarkRunner.Benchmark(
        "project_task.responsiveness",
        3,
        20,
        () =>
          assert(
            projectTask.exists(command => command.workingDirectory == projectRoot && command.executable == "sbt") &&
              projectTaskPresentation.exists(_.contains("Running test task"))
          ),
        () => ProjectTaskDetector.detect(projectRoot, ProjectTaskKind.Test).map(ProjectTaskTerminal.started)
      ),
      BenchmarkRunner.Benchmark(
        "markdown.preview.window_mapping",
        3,
        20,
        () => assert(markdownPreviewWindow.firstSourceLine >= 0 && markdownPreviewWindow.source.nonEmpty),
        () =>
          MarkdownDocumentPreview
            .previewWindow(markdownLines, activeLine = Some(1_200), fallbackTopLine = 1_000, maxSourceLines = 80)
      ),
      BenchmarkRunner.Benchmark(
        "markdown.preview.html_fragment",
        2,
        8,
        () => assert(markdownHtmlFragment.contains("<h2>")),
        () => MarkdownDocumentPreview.renderHtmlFragment(markdownSource.take(60_000), "benchmark")
      ),
      BenchmarkRunner.Benchmark(
        "render.markdown.inline_lens",
        2,
        8,
        () => assert(renderedFrameHasPixels(markdownLensFrame)),
        () => renderedFrame(markdownState, deviceScale = 1.0)
      ),
      BenchmarkRunner.Benchmark(
        "animation.large_visible_tick",
        3,
        30,
        () => assert(animationCells.nonEmpty && advancedAnimationState != animationState),
        () => animationState.advanceAllAnimations()
      )
    )

  private def renderedFrame(state: AppState, deviceScale: Double): BufferedImage =
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
    Renderer.render(state, cursorVisible = true, surface, viewportSize, monoFont, textFont, cellMetrics, None)
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
    surface.setFont(textFont)
    surface.clearViewport(Theme.light.background)
    CharacterRenderer.renderMeasuredLineWithAnimation(
      surface,
      xOriginPx = 0.0f,
      yPx = 0,
      lineHeightPx = cellMetrics.lineHeight,
      ascentPx = cellMetrics.ascent,
      line,
      Theme.light,
      AnimationState.empty,
      clipRightXPx = Some(frameWidthPx.toFloat)
    )
    image

  private def prepareCursorBaseFrame(state: AppState, window: SwingWindow): Unit =
    Renderer.render(
      state,
      cursorVisible = false,
      window,
      monoFont,
      textFont,
      uiFont,
      uiMetrics,
      cursorColor = None,
      repaintOnFlush = false
    )

  private def renderedCursorOverlay(state: AppState, window: SwingWindow): Boolean =
    Renderer.renderCursorOnly(state, cursorVisible = true, window, monoFont, textFont, uiFont, uiMetrics, None)

  private def reducedTopLine(result: com.serenity.state.reducers.ReducerResult): Option[Int] =
    result.state.buffers.get(BufferId(1)).map(_.viewport.topLine)

  private def decodeLspMessages(bytes: Array[Byte]): List[Json] =
    import cats.effect.unsafe.implicits.global

    fs2.Stream
      .chunk(fs2.Chunk.array(bytes))
      .through(LspFramer.decode)
      .compile
      .toList
      .unsafeRunSync()

  private def projectTaskFixtureResource: Resource[IO, Path] =
    Resource.make(
      IO.blocking {
        val root = Files.createTempDirectory("serenity-performance-project-")
        val _    = Files.writeString(root.resolve("build.sbt"), "// deterministic benchmark fixture\n")
        root
      }
    )(root =>
      IO.blocking {
        val _ = Files.deleteIfExists(root.resolve("build.sbt"))
        val _ = Files.deleteIfExists(root)
        ()
      }
    )

end PerformanceBenchmarks
