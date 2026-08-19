package com.serenity.perf

import com.serenity.config.AppConfig
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.{InlineMark, RichTextDocument, RichTextParagraph, RichTextRun, RichTextStyle}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, Buffer, BufferId, CursorPosition, EditorPane, PaneId, Viewport}
import com.serenity.ui.layout.{Layout, ViewportSize}
import com.serenity.ui.theme.Theme

/** Documents and editor states the benchmarks run against, kept apart from the measurements so that adding a benchmark
  * and adding a fixture stay separate edits.
  */
private[perf] object BenchmarkFixtures:

  given Balance = Balance.default

  val viewportSize: ViewportSize = ViewportSize(120, 40)

  def editorState(content: String, language: Option[LanguageId]): AppState =
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

  def editorStateForRichDocument(document: RichTextDocument): AppState =
    val base = editorState(document.plainText, None)
    base.copy(buffers = base.buffers.view.mapValues(_.copy(richTextDocument = Some(document))).toMap)

  def deepViewport: Viewport =
    Viewport(topLine = 10_000, leftColumn = 0, visibleColumns = viewportSize.width, visibleLines = viewportSize.height)

  def largeSingleLineJson(entries: Int): String =
    (1 to entries).map(i => s""""k$i":$i""").mkString("{", ",", "}")

  def largeMultilineDocument(lines: Int): String =
    (1 to lines)
      .map(i => s"Line $i with enough text to exercise wrapping, comments, and cursor movement.")
      .mkString("\n")

  def largeFindDocument(matches: Int): String =
    (1 to matches).map(index => s"needle $index with replacement candidate").mkString("\n")

  def largeMarkdownDocument(sections: Int): Vector[String] =
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

  def largeRichTextDocument(lines: Int): RichTextDocument =
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

  def withCursorsOnConsecutiveLines(state: AppState, count: Int, fromLine: Int, column: Int): AppState =
    val cursors = (0 until count).toList.map(row => CursorPosition(fromLine + row, column))
    state.copy(buffers = state.buffers.view.mapValues(_.copy(cursors = cursors)).toMap)

end BenchmarkFixtures
