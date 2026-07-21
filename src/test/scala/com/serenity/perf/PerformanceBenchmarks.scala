package com.serenity.perf

import java.awt.{Color, Font}
import java.awt.image.BufferedImage
import java.nio.file.Paths

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
import com.serenity.state.reducers.EditorEventReducer
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.{Java2DRenderSurface, Renderer}
import com.serenity.ui.terminal.SwingWindow
import com.serenity.ui.theme.Theme
import io.circe.Json

object PerformanceBenchmarks:

  private case class Benchmark(name: String, warmups: Int, iterations: Int, verify: () => Unit, run: () => Unit)
  private case class BenchmarkResult(name: String, iterations: Int, minMs: Double, p50Ms: Double, p95Ms: Double, maxMs: Double)

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont     = Font(Font.SERIF, Font.PLAIN, 14)
  private val cellMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(120, 40)
  private val frameWidthPx = viewportSize.width * cellMetrics.charWidth
  private val frameHeightPx = viewportSize.height * cellMetrics.lineHeight
  private val repaintTarget = new javax.swing.JPanel()

  def main(args: Array[String]): Unit =
    val results = benchmarks().map(runBenchmark)
    printResults(results)

  private def benchmarks(): List[Benchmark] =
    val jsonText       = largeSingleLineJson(entries = 20_000)
    val multilineText  = largeMultilineDocument(lines = 15_000)
    val findText       = largeFindDocument(matches = 12_000)
    val markdownLines  = largeMarkdownDocument(sections = 800)
    val markdownSource = markdownLines.mkString("\n")
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
      buffers = findState.buffers.view.mapValues { buffer =>
        buffer.copy(cursors = List(CursorPosition(6_000, 12)))
      }.toMap
    )
    val findResultSet = FindResultSet.normalized(
      "needle",
      (0 until 12_000).toList.map(line => FindResult(line, 10)),
      requestedIndex = 6_000
    )
    val lspMessages = (1 to 250).toList.map { id =>
      Json.obj("jsonrpc" -> Json.fromString("2.0"), "id" -> Json.fromInt(id), "method" -> Json.fromString("benchmark"))
    }
    val framedLspMessages = lspMessages.flatMap(LspFramer.encode).toArray
    val projectTask = ProjectTaskDetector.detect(Paths.get("."), ProjectTaskKind.Test)
    val cursorSnapshot = plainScrollState.buffers
      .get(BufferId(1))
      .map(buffer =>
        com.serenity.ui.layout.TextLayoutSnapshot.fromBuffer(
          buffer,
          frameWidthPx,
          monoFont,
          wordWrapEnabled = false
        )
      )
    val cursorBaseFrame = renderedFrame(plainScrollState, deviceScale = 1.0)
    val animationCells = multilineState.buffers.get(BufferId(1)).map(buffer =>
        com.serenity.state.manager.VisibleBufferAnimationCells.fromBuffer(
          buffer,
          wordWrapEnabled = false,
          startColor = Theme.light.muted,
          endColor = Theme.light.foreground
        )
      ).getOrElse(Map.empty)
    val animationState = AnimationState(
      FlowAnimationBuilder.build(animationCells, FlowDirection.ByColumn, SweepDirection.Forward, 12)
    )

    List(
      Benchmark(
        "rope.large_json.search",
        3,
        12,
        () => assert(Rope(jsonText).searchAll("\"k19999\"").nonEmpty),
        () => Rope(jsonText).searchAll("\"k19999\"")
      ),
      Benchmark(
        "rope.large_json.cursor_offset",
        3,
        20,
        () => assert(Rope(jsonText).lineColumnToOffset(0, jsonText.length - 5) == jsonText.length - 5),
        () => Rope(jsonText).lineColumnToOffset(0, jsonText.length - 5)
      ),
      Benchmark(
        "layout.large_multiline.visible_viewport",
        3,
        20,
        () => assert(plainScrollState.buffers.get(BufferId(1)).exists(_.content.lineCount == 15_000)),
        () => plainScrollState.buffers.get(BufferId(1)).foreach { buffer =>
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
        () => assert(richState.buffers.get(BufferId(1)).exists(_.richTextDocument.nonEmpty)),
        () => renderedFrame(richState, deviceScale = 1.0)
      ),
      Benchmark(
        "render.cursor_only.java2d_overlay",
        2,
        8,
        () => assert(cursorSnapshot.exists(_.visualLines.nonEmpty)),
        () => cursorSnapshot.foreach(snapshot => renderedCursorOverlay(cursorBaseFrame, snapshot))
      ),
      Benchmark(
        "render.diagnostics_and_comments.java2d",
        2,
        8,
        () => assert(diagnosticsState.diagnostics.values.flatten.size == 2_000),
        () => renderedFrame(diagnosticsState, deviceScale = 1.0)
      ),
      Benchmark(
        "render.hidpi_frame.java2d",
        2,
        8,
        () => assert(frameWidthPx > 0 && frameHeightPx > 0),
        () => renderedFrame(commentsState, deviceScale = 2.0)
      ),
      Benchmark(
        "reducer.normal_editing",
        3,
        20,
        () => assert(editingState.buffers.get(BufferId(1)).exists(_.cursors.headOption.contains(CursorPosition(6_000, 12)))),
        () => EditorEventReducer.reduce(InsertChar('x'), PaneId(0), editingState)
      ),
      Benchmark(
        "reducer.deep_scroll.plain",
        3,
        20,
        () => assert(plainScrollState.buffers.get(BufferId(1)).exists(_.viewport.topLine == deepViewport.topLine)),
        () => EditorEventReducer.reduce(ScrollDown(40), PaneId(0), plainScrollState)
      ),
      Benchmark(
        "reducer.deep_scroll.rich_text",
        3,
        20,
        () => assert(richScrollState.buffers.get(BufferId(1)).exists(_.richTextDocument.nonEmpty)),
        () => EditorEventReducer.reduce(ScrollDown(40), PaneId(0), richScrollState)
      ),
      Benchmark(
        "find_replace.large_result_set",
        3,
        20,
        () => assert(findResultSet.results.size == 12_000),
        () => findResultSet.visibleResults(maxResults = 80)
      ),
      Benchmark(
        "lsp.framer.large_batch",
        3,
        12,
        () => assert(decodeLspMessages(framedLspMessages).size == lspMessages.size),
        () => decodeLspMessages(framedLspMessages)
      ),
      Benchmark(
        "project_task.responsiveness",
        3,
        20,
        () => assert(projectTask.exists(_.executable == "sbt")),
        () => ProjectTaskDetector.detect(Paths.get("."), ProjectTaskKind.Test).map(ProjectTaskTerminal.started)
      ),
      Benchmark(
        "markdown.preview.window_mapping",
        3,
        20,
        () => assert(markdownLines.size == 6_400),
        () =>
          MarkdownDocumentPreview
            .previewWindow(markdownLines, activeLine = Some(1_200), fallbackTopLine = 1_000, maxSourceLines = 80)
      ),
      Benchmark(
        "markdown.preview.html_fragment",
        2,
        8,
        () => assert(markdownSource.nonEmpty),
        () => MarkdownDocumentPreview.renderHtmlFragment(markdownSource.take(60_000), "benchmark")
      ),
      Benchmark(
        "render.markdown.inline_lens",
        2,
        8,
        () => assert(markdownState.buffers.get(BufferId(1)).exists(_.language.contains(LanguageId.Markdown))),
        () => renderedFrame(markdownState, deviceScale = 1.0)
      ),
      Benchmark(
        "animation.large_visible_tick",
        3,
        30,
        () => assert(animationCells.nonEmpty),
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
    BenchmarkResult(
      name = benchmark.name,
      iterations = benchmark.iterations,
      minMs = samples.headOption.getOrElse(0.0),
      p50Ms = percentile(samples, 0.50),
      p95Ms = percentile(samples, 0.95),
      maxMs = samples.lastOption.getOrElse(0.0)
    )

  private def percentile(samples: IndexedSeq[Double], percentile: Double): Double =
    if samples.isEmpty then 0.0
    else
      val index = math.ceil(percentile.max(0.0).min(1.0) * samples.length).toInt - 1
      samples(index.max(0).min(samples.length - 1))

  private def printResults(results: List[BenchmarkResult]): Unit =
    println("Serenity performance benchmarks")
    println(s"context,java_runtime,${System.getProperty("java.runtime.version", "unknown")}")
    println(s"context,java_vendor,${System.getProperty("java.vendor", "unknown")}")
    println(s"context,os,${System.getProperty("os.name", "unknown")} ${System.getProperty("os.version", "unknown")}")
    println(s"context,available_processors,${Runtime.getRuntime.availableProcessors()}")
    println("name,iterations,min_ms,p50_ms,p95_ms,max_ms")
    results.foreach { result =>
      println(
        f"${result.name},${result.iterations},${result.minMs}%.3f,${result.p50Ms}%.3f,${result.p95Ms}%.3f,${result.maxMs}%.3f"
      )
    }

  private def renderedFrame(state: AppState, deviceScale: Double): BufferedImage =
    val image = new BufferedImage(
      math.ceil(frameWidthPx * deviceScale).toInt,
      math.ceil(frameHeightPx * deviceScale).toInt,
      BufferedImage.TYPE_INT_ARGB
    )
    val surface = new Java2DRenderSurface(
      image,
      cellMetrics,
      monoFont,
      _ => requestRepaint(),
      logicalWidthPx = frameWidthPx,
      logicalHeightPx = frameHeightPx,
      deviceScaleX = deviceScale,
      deviceScaleY = deviceScale
    )
    Renderer.render(state, cursorVisible = true, surface, viewportSize, monoFont, textFont, cellMetrics, None)
    image

  private def renderedCursorOverlay(
      baseFrame: BufferedImage,
      snapshot: com.serenity.ui.layout.TextLayoutSnapshot
  ): BufferedImage =
    val overlay = SwingWindow.copyImage(baseFrame)
    val surface = new Java2DRenderSurface(overlay, cellMetrics, monoFont, _ => requestRepaint())
    val cursorX = snapshot.visualLines.headOption.flatMap(_.xForColumn(0)).getOrElse(0.0f).toInt
    surface.fillPixelRect(cursorX, 0, math.max(1, cellMetrics.charWidth / 8), cellMetrics.lineHeight, Color.WHITE)
    surface.flush()
    overlay

  private def requestRepaint(): Unit =
    repaintTarget.repaint()

  private def decodeLspMessages(bytes: Array[Byte]): List[Json] =
    import cats.effect.unsafe.implicits.global

    fs2.Stream
      .chunk(fs2.Chunk.array(bytes))
      .through(LspFramer.decode)
      .compile
      .toList
      .unsafeRunSync()

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
