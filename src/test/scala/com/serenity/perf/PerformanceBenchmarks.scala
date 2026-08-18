package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage
import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}

import cats.effect.{IO, Resource}
import com.serenity.animation.*
import com.serenity.config.{AppConfig, MarkdownViewMode}
import com.serenity.keystroke.events.{InsertChar, ScrollDown}
import com.serenity.lsp.client.LspFramer
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.project.{ProjectTaskDetector, ProjectTaskKind, ProjectTaskTerminal}
import com.serenity.richtext.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.{EditorEventReducer, ModalEventReducer}
import com.serenity.ui.layout.{CellMetrics, Layout, TextLayoutSnapshot, ViewportSize}
import com.serenity.ui.renderer.{CharacterRenderer, Java2DRenderSurface, Renderer}
import com.serenity.ui.terminal.SwingWindow
import com.serenity.ui.theme.Theme
import io.circe.Json

object PerformanceBenchmarks:

  private val reusableFramePools = Map(
    1.0 -> new SwingWindow.ReusableImagePool,
    2.0 -> new SwingWindow.ReusableImagePool
  )

  final private case class Benchmark(
      name: String,
      warmups: Int,
      iterations: Int,
      verify: () => Unit,
      run: () => Unit,
      measureAllocation: Boolean = false
  )

  final private case class BenchmarkResult(
      name: String,
      iterations: Int,
      minMs: Double,
      p50Ms: Double,
      p95Ms: Double,
      maxMs: Double,
      allocationP50Bytes: Option[Long],
      allocationP95Bytes: Option[Long]
  )

  private val allocationBean = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
      if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
      Some(bean)
    case _ => None

  given Balance = Balance.default

  private val monoFont      = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont      = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont        = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val cellMetrics   = CellMetrics.fromFont(monoFont)
  private val uiMetrics     = CellMetrics.fromFont(uiFont)
  private val viewportSize  = ViewportSize(120, 40)
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
            val results = benchmarks(window, projectRoot).map(runBenchmark)
            printResults(results)
          }
      }
      .unsafeRunSync()

  private def benchmarks(cursorWindow: SwingWindow, projectRoot: Path): List[Benchmark] =
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
        buffer.copy(documentComments =
          (10 until 3_000 by 3)
            .map(line => DocumentComment(CursorPosition(line, 0), CursorPosition(line, 20), "note"))
            .toList
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
      buffers = findState.buffers.view.mapValues(buffer => buffer.copy(cursors = List(CursorPosition(6_000, 12)))).toMap
    )
    val normalEditingResult = EditorEventReducer.reduce(InsertChar('x'), PaneId(0), editingState)
    val plainScrollResult   = EditorEventReducer.reduce(ScrollDown(40), PaneId(0), plainScrollState)
    val richScrollResult    = EditorEventReducer.reduce(ScrollDown(40), PaneId(0), richScrollState)
    val expectedEditedLine = editingState.buffers
      .get(BufferId(1))
      .flatMap(_.content.getLine(6_000))
      .map(_.patch(12, "x", 0))
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
      findQueryState.buffers(BufferId(1)).content
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
      Benchmark(
        "rope.large_json.search",
        3,
        12,
        () => assert(jsonSearchResults.nonEmpty),
        () => Rope(jsonText).searchAll("\"k19999\"")
      ),
      Benchmark(
        "rope.large_json.cursor_offset",
        3,
        20,
        () => assert(jsonCursorOffset == jsonText.length - 5),
        () => Rope(jsonText).lineColumnToOffset(0, jsonText.length - 5)
      ),
      Benchmark(
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
      Benchmark(
        "render.full_frame.java2d",
        2,
        8,
        () => assert(renderedFrameHasPixels(fullFrame)),
        () => renderedFrame(richState, deviceScale = 1.0)
      ),
      Benchmark(
        "render.long_measured_line.java2d",
        2,
        8,
        () => assert(renderedFrameHasPixels(longMeasuredLineFrame)),
        () =>
          val _ = renderedLongMeasuredLine(longMeasuredLine)
          ()
        ,
        measureAllocation = true
      ),
      Benchmark(
        "render.cursor_only.scene_reuse.java2d_overlay",
        2,
        8,
        () => assert(renderedCursorOverlay(plainScrollState, cursorWindow)),
        () =>
          val _ = renderedCursorOverlay(plainScrollState, cursorWindow)
          ()
      ),
      Benchmark(
        "render.diagnostics_and_comments.java2d",
        2,
        8,
        () => assert(renderedFrameHasPixels(diagnosticsAndComments)),
        () => renderedFrame(diagnosticsState, deviceScale = 1.0)
      ),
      Benchmark(
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
      ),
      Benchmark(
        "reducer.normal_editing",
        3,
        20,
        () =>
          assert(
            expectedEditedLine.exists(line =>
              normalEditingResult.state.buffers.get(BufferId(1)).flatMap(_.content.getLine(6_000)).contains(line)
            )
          ),
        () => EditorEventReducer.reduce(InsertChar('x'), PaneId(0), editingState)
      ),
      Benchmark(
        "reducer.deep_scroll.plain",
        3,
        20,
        () => assert(reducedTopLine(plainScrollResult) == Some(deepViewport.topLine + 40)),
        () => EditorEventReducer.reduce(ScrollDown(40), PaneId(0), plainScrollState)
      ),
      Benchmark(
        "reducer.deep_scroll.rich_text",
        3,
        20,
        () => assert(reducedTopLine(richScrollResult) == Some(deepViewport.topLine + 40)),
        () => EditorEventReducer.reduce(ScrollDown(40), PaneId(0), richScrollState)
      ),
      Benchmark(
        "find_replace.large_result_set",
        3,
        20,
        () => assert(visibleFindResults.size == 80 && visibleFindResults.exists(_._1 == FindResult(6_000, 10))),
        () => findResultSet.visibleResults(maxResults = 80)
      ),
      Benchmark(
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
      Benchmark(
        "find_replace.large_query_keystroke",
        3,
        20,
        () => assert(findKeystrokeResult.effects.nonEmpty),
        () => ModalEventReducer.reduce(ModalType.Find, InsertChar('e'), findKeystrokeState)
      ),
      Benchmark(
        "lsp.framer.large_batch",
        3,
        12,
        () => assert(decodedLspMessages == lspMessages),
        () => decodeLspMessages(framedLspMessages)
      ),
      Benchmark(
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
      Benchmark(
        "markdown.preview.window_mapping",
        3,
        20,
        () => assert(markdownPreviewWindow.firstSourceLine >= 0 && markdownPreviewWindow.source.nonEmpty),
        () =>
          MarkdownDocumentPreview
            .previewWindow(markdownLines, activeLine = Some(1_200), fallbackTopLine = 1_000, maxSourceLines = 80)
      ),
      Benchmark(
        "markdown.preview.html_fragment",
        2,
        8,
        () => assert(markdownHtmlFragment.contains("<h2>")),
        () => MarkdownDocumentPreview.renderHtmlFragment(markdownSource.take(60_000), "benchmark")
      ),
      Benchmark(
        "render.markdown.inline_lens",
        2,
        8,
        () => assert(renderedFrameHasPixels(markdownLensFrame)),
        () => renderedFrame(markdownState, deviceScale = 1.0)
      ),
      Benchmark(
        "animation.large_visible_tick",
        3,
        30,
        () => assert(animationCells.nonEmpty && advancedAnimationState != animationState),
        () => animationState.advanceAllAnimations()
      )
    )

  private def runBenchmark(benchmark: Benchmark): BenchmarkResult =
    benchmark.verify()
    (0 until benchmark.warmups).foreach(_ => benchmark.run())
    val samples = (0 until benchmark.iterations).map { _ =>
      val started = System.nanoTime()
      benchmark.run()
      (System.nanoTime() - started).toDouble / 1_000_000.0
    }.sorted
    val allocationSamples =
      if benchmark.measureAllocation then
        allocationBean
          .map { bean =>
            val threadId = Thread.currentThread().getId
            (0 until benchmark.iterations).map { _ =>
              val started = bean.getThreadAllocatedBytes(threadId)
              benchmark.run()
              (bean.getThreadAllocatedBytes(threadId) - started).max(0L)
            }
          }
          .getOrElse(Vector.empty[Long])
          .sorted
      else Vector.empty[Long]
    BenchmarkResult(
      name = benchmark.name,
      iterations = benchmark.iterations,
      minMs = samples.headOption.getOrElse(0.0),
      p50Ms = percentile(samples, 0.50),
      p95Ms = percentile(samples, 0.95),
      maxMs = samples.lastOption.getOrElse(0.0),
      allocationP50Bytes = allocationSamples.headOption.map(_ => percentileLong(allocationSamples, 0.50)),
      allocationP95Bytes = allocationSamples.headOption.map(_ => percentileLong(allocationSamples, 0.95))
    )

  private def percentile(samples: IndexedSeq[Double], percentile: Double): Double =
    if samples.isEmpty then 0.0
    else
      val index = math.ceil(percentile.max(0.0).min(1.0) * samples.length).toInt - 1
      samples(index.max(0).min(samples.length - 1))

  private def percentileLong(samples: IndexedSeq[Long], percentile: Double): Long =
    if samples.isEmpty then 0L
    else
      val index = math.ceil(percentile.max(0.0).min(1.0) * samples.length).toInt - 1
      samples(index.max(0).min(samples.length - 1))

  private def printResults(results: List[BenchmarkResult]): Unit =
    println("Serenity performance benchmarks")
    println(s"context,java_runtime,${System.getProperty("java.runtime.version", "unknown")}")
    println(s"context,java_vendor,${System.getProperty("java.vendor", "unknown")}")
    println(s"context,os,${System.getProperty("os.name", "unknown")} ${System.getProperty("os.version", "unknown")}")
    println(s"context,available_processors,${Runtime.getRuntime.availableProcessors()}")
    println("name,iterations,min_ms,p50_ms,p95_ms,max_ms,allocation_p50_bytes,allocation_p95_bytes")
    results.foreach { result =>
      val allocationP50 = result.allocationP50Bytes.fold("")(_.toString)
      val allocationP95 = result.allocationP95Bytes.fold("")(_.toString)
      println(
        f"${result.name},${result.iterations},${result.minMs}%.3f,${result.p50Ms}%.3f,${result.p95Ms}%.3f,${result.maxMs}%.3f,$allocationP50,$allocationP95"
      )
    }

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

  private def renderedLongMeasuredLine(line: com.serenity.ui.layout.TextVisualLine): BufferedImage =
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

  private def editorState(content: String, language: Option[LanguageId]): AppState =
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, content)
      .copy(
        language = language,
        viewport =
          Viewport(topLine = 0, leftColumn = 0, visibleColumns = viewportSize.width, visibleLines = viewportSize.height)
      )
    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout =
        Layout(editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)), activeEditorPaneId = Some(paneId)),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false).withWordWrap(false)
    )

  private def editorStateForRichDocument(document: RichTextDocument): AppState =
    val base = editorState(document.plainText, None)
    base.copy(buffers = base.buffers.view.mapValues(_.copy(richTextDocument = Some(document))).toMap)

  private def deepViewport: Viewport =
    Viewport(topLine = 10_000, leftColumn = 0, visibleColumns = viewportSize.width, visibleLines = viewportSize.height)

  private def largeSingleLineJson(entries: Int): String =
    (1 to entries).map(i => s""""k$i":$i""").mkString("{", ",", "}")

  private def largeMultilineDocument(lines: Int): String =
    (1 to lines)
      .map(i => s"Line $i with enough text to exercise wrapping, comments, and cursor movement.")
      .mkString("\n")

  private def largeFindDocument(matches: Int): String =
    (1 to matches).map(index => s"needle $index with replacement candidate").mkString("\n")

  private def largeMarkdownDocument(sections: Int): Vector[String] =
    (1 to sections).toVector.flatMap { section =>
      Vector(
        s"## Section $section",
        "",
        s"Paragraph with **bold** text, `code`, and [a link](https://example.com/$section).",
        "",
        "| Name | Value |",
        "| --- | ---: |",
        s"| item-$section | $section |",
        ""
      )
    }

  private def largeRichTextDocument(lines: Int): RichTextDocument =
    RichTextDocument(
      (1 to lines).map { line =>
        RichTextParagraph(
          List(
            RichTextRun(s"Rich paragraph $line ", RichTextStyle.empty.withMark(InlineMark.Bold)),
            RichTextRun("with styled content")
          )
        )
      }.toList
    )

end PerformanceBenchmarks
