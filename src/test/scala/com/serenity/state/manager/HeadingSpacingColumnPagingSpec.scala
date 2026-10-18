package com.serenity.state.manager

import com.serenity.config.{AppConfig, HeadingsConfig}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{TextLayoutSnapshot, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Space around headings is vertical room: a column holds the rows that fit its height with that room counted, so a
  * page holds no more rows than the same document has without space, and no column of a page starts with space above it
  * or overflows. The caret stays on the page it anchors.
  */
class HeadingSpacingColumnPagingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId       = PaneId(0)
  private val bufferId     = BufferId(1)
  private val viewportSize = ViewportSize(300, 40)
  private val fontConfig   = FontLoader.FontConfig()

  private val document = RichTextDocument(
    (1 to 14).toList.flatMap { chapter =>
      List(
        RichTextParagraph.plain(s"Chapter $chapter", role = ParagraphRole.Heading(1)),
        RichTextParagraph.plain(s"Chapter $chapter opens. " + "words that fill the column " * 9),
        RichTextParagraph.plain(s"Section $chapter.1", role = ParagraphRole.Heading(2)),
        RichTextParagraph.plain(s"Section text $chapter. " + "and some more words here " * 7)
      )
    }
  )

  private val buffer =
    Buffer
      .fromString(bufferId, document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 30, visibleLines = 12)
      )

  private val spacing = HeadingsConfig()
    .withLevel(1)(_.copy(spaceBefore = Some(1.5f), spaceAfter = Some(1.0f)))
    .withLevel(2)(_.copy(spaceBefore = Some(1.0f), spaceAfter = Some(0.5f)))

  private def stateWith(headings: HeadingsConfig, columnCount: Int): AppState =
    val config = AppConfig.default
      .withFontConfig(fontConfig)
      .withWordWrap(true)
      .withColumnMode(true)
      .withColumnCount(Some(columnCount))
      .withHeadingsConfig(headings)
    val base = AppState.initial
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = base.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = config
      ),
      runtime = base.runtime.copy(capabilities = FrontendCapabilities.gui, viewportSize = Some(viewportSize))
    )

  private def placed(state: AppState, from: Buffer, line: Int): Buffer =
    val cursor = CursorPosition(line, 0)
    from.copy(
      editing = EditingState(List(cursor)),
      viewport = CursorViewport.adjustForCursorColumnMode(from, state, cursor)
    )

  for columns <- List(1, 2, 3) do
    s"The pages of $columns column(s) with heading space" should "never be fewer than the pages without it" in {
      val plain  = stateWith(HeadingsConfig(), columns)
      val spaced = stateWith(spacing, columns)

      def pageCount(state: AppState): Int =
        (0 until buffer.document.content.lineCount).toList
          .map(line => placed(state, buffer, line).viewport)
          .map(viewport => viewport.topLine -> viewport.topVisualLine)
          .distinct
          .size

      pageCount(spaced) should be >= pageCount(plain)
    }

    it should s"keep the caret inside its page of $columns column(s) walking down and back up" in {
      val state = stateWith(spacing, columns)
      val lines = (0 until 56).toList ++ (55 to 0 by -1).toList

      val viewports = lines.scanLeft(buffer)((walk, line) => placed(state, walk, line)).drop(1)

      viewports.zip(lines).foreach { (walked, line) =>
        withClue(s"line $line with the page at ${walked.viewport.topLine}: ") {
          line should be >= walked.viewport.topLine
        }
      }
    }

  "A column page" should "start every column below the top of its column, with no heading space above its first row" in {
    val state = stateWith(spacing, 3)
    val font  = FontLoader.previewFontForRole(fontConfig, buffer.typographyRole)

    val columns = TextLayoutSnapshot.fromBufferColumns(
      buffer,
      columnWidthPx = 260,
      font = font,
      columnCount = 3,
      headings = state.persisted.config.headingSettings
    )

    columns.size shouldBe 3
    columns.foreach(column => column.visualLines.head.spaceBeforePx shouldBe 0)
    columns.foreach(_.visualLines.size should be <= buffer.viewport.visibleLines)
    columns.map(_.visualLines.size).min should be < buffer.viewport.visibleLines
    columns.flatMap(_.visualLines).count(_.spaceBeforePx > 0) should be > 0
  }
