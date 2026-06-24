package com.serenity.perf

import java.awt.Font

import com.serenity.MockRenderSurface
import com.serenity.animation.*
import com.serenity.config.{AppConfig, MarkdownViewMode}
import com.serenity.lsp.config.LanguageId
import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.richtext.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme

object PerformanceBenchmarks:

  private case class Benchmark(name: String, warmups: Int, iterations: Int, run: () => Unit)
  private case class BenchmarkResult(name: String, iterations: Int, minMs: Double, medianMs: Double, maxMs: Double)

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont     = Font(Font.SERIF, Font.PLAIN, 14)
  private val cellMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(120, 40)

  def main(args: Array[String]): Unit =
    val results = benchmarks().map(runBenchmark)
    printResults(results)

  private def benchmarks(): List[Benchmark] =
    val jsonText       = largeSingleLineJson(entries = 20_000)
    val multilineText  = largeMultilineDocument(lines = 15_000)
    val markdownLines  = largeMarkdownDocument(sections = 800)
    val markdownSource = markdownLines.mkString("\n")
    val richDocument   = largeRichTextDocument(lines = 6_000)
    val richState      = editorStateForRichDocument(richDocument)
    val renderSurface  = MockRenderSurface(viewportSize.width, viewportSize.height)
    val markdownState = editorState(markdownSource, Some(LanguageId.Markdown)).copy(
      config = AppConfig.default
        .withLineNumbers(false)
        .withGutter(false)
        .withWordWrap(false)
        .withMarkdownViewMode(MarkdownViewMode.InlineLens)
    )
    val markdownSurface = MockRenderSurface(viewportSize.width, viewportSize.height)
    val commentsState = editorState(multilineText, None).copy(
      buffers = editorState(multilineText, None).buffers.view.mapValues { buffer =>
        buffer.copy(documentComments =
          (10 until 500 by 15)
            .map(line => DocumentComment(CursorPosition(line, 0), CursorPosition(line, 20), "note"))
            .toList
        )
      }.toMap
    )
    val commentsSurface = MockRenderSurface(viewportSize.width, viewportSize.height)
    val animationCells = com.serenity.state.manager.VisibleBufferAnimationCells.fromBuffer(
      editorState(multilineText, None).buffers.values.head,
      wordWrapEnabled = false,
      startColor = Theme.light.muted,
      endColor = Theme.light.foreground
    )
    val animationState = AnimationState(
      FlowAnimationBuilder.build(animationCells, FlowDirection.ByColumn, SweepDirection.Forward, 12)
    )

    List(
      Benchmark("rope.large_json.search", 3, 12, () => Rope(jsonText).searchAll("\"k19999\"")),
      Benchmark(
        "rope.large_json.cursor_offset",
        3,
        20,
        () => Rope(jsonText).lineColumnToOffset(0, jsonText.length - 5)
      ),
      Benchmark(
        "layout.large_multiline.visible_viewport",
        3,
        20,
        () =>
          val state  = editorState(multilineText, None)
          val buffer = state.buffers.values.head
          com.serenity.ui.layout.TextLayoutSnapshot.fromBuffer(
            buffer,
            panelWidthPx = viewportSize.width * cellMetrics.charWidth,
            monoFont,
            wordWrapEnabled = false
          )
      ),
      Benchmark(
        "render.rich_text.large_visible",
        2,
        8,
        () =>
          renderSurface.clear()
          Renderer
            .render(richState, cursorVisible = true, renderSurface, viewportSize, monoFont, textFont, cellMetrics, None)
      ),
      Benchmark(
        "render.comments.large_visible",
        2,
        8,
        () =>
          commentsSurface.clear()
          Renderer.render(
            commentsState,
            cursorVisible = true,
            commentsSurface,
            viewportSize,
            monoFont,
            textFont,
            cellMetrics,
            None
          )
      ),
      Benchmark(
        "markdown.preview.window_mapping",
        3,
        20,
        () =>
          MarkdownDocumentPreview
            .previewWindow(markdownLines, activeLine = Some(1_200), fallbackTopLine = 1_000, maxSourceLines = 80)
      ),
      Benchmark(
        "markdown.preview.html_fragment",
        2,
        8,
        () => MarkdownDocumentPreview.renderHtmlFragment(markdownSource.take(60_000), "benchmark")
      ),
      Benchmark(
        "render.markdown.inline_lens",
        2,
        8,
        () =>
          markdownSurface.clear()
          Renderer.render(
            markdownState,
            cursorVisible = true,
            markdownSurface,
            viewportSize,
            monoFont,
            textFont,
            cellMetrics,
            None
          )
      ),
      Benchmark("animation.large_visible_tick", 3, 30, () => animationState.advanceAllAnimations())
    )

  private def runBenchmark(benchmark: Benchmark): BenchmarkResult =
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
      medianMs = samples.lift(samples.length / 2).getOrElse(0.0),
      maxMs = samples.lastOption.getOrElse(0.0)
    )

  private def printResults(results: List[BenchmarkResult]): Unit =
    println("Serenity performance benchmarks")
    println("name,iterations,min_ms,median_ms,max_ms")
    results.foreach { result =>
      println(
        f"${result.name},${result.iterations},${result.minMs}%.3f,${result.medianMs}%.3f,${result.maxMs}%.3f"
      )
    }

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

  private def largeSingleLineJson(entries: Int): String =
    (1 to entries).map(i => s""""k$i":$i""").mkString("{", ",", "}")

  private def largeMultilineDocument(lines: Int): String =
    (1 to lines)
      .map(i => s"Line $i with enough text to exercise wrapping, comments, and cursor movement.")
      .mkString("\n")

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
