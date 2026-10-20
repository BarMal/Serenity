package com.serenity.ui.layout

import java.awt.Font

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, CursorPosition, RichTextState, Viewport}
import com.serenity.ui.theme.{HeadingSettings, HeadingStyle}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Space around a heading is not a row: the caret steps from the row above straight to the heading and from the heading
  * to the row below, exactly as it does when the heading has no space.
  */
class HeadingSpacingNavigationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font = Font(Font.SERIF, Font.PLAIN, 14)

  private val document = RichTextDocument(
    List(
      RichTextParagraph.plain("Opening paragraph."),
      RichTextParagraph.plain("Chapter One", role = ParagraphRole.Heading(1)),
      RichTextParagraph.plain("Text under the chapter."),
      RichTextParagraph.plain("A section heading that wraps over several rows " * 3, role = ParagraphRole.Heading(2)),
      RichTextParagraph.plain("Closing paragraph.")
    )
  )

  private val buffer =
    Buffer
      .fromString(BufferId(1), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 30)
      )

  private val spacedSettings =
    HeadingSettings(styles = HeadingStyle.defaults.map(_.copy(spaceBeforeEm = 2.0f, spaceAfterEm = 1.5f)))

  private def snapshotOf(headings: HeadingSettings) =
    TextLayoutSnapshot.fromBuffer(buffer, 300, font, headings = headings)

  private def walk(snapshot: TextLayoutSnapshot, direction: Int, from: CursorPosition): List[CursorPosition] =
    LazyList
      .iterate(Option(from))(_.flatMap(snapshot.moveVertical(_, direction, 0.0f)))
      .takeWhile(_.isDefined)
      .flatten
      .toList

  "Moving down through a spaced heading" should "visit every row once, as it does without the space" in {
    val spaced = snapshotOf(spacedSettings)
    val plain  = snapshotOf(HeadingSettings.default)

    walk(spaced, 1, CursorPosition(0, 0)) shouldBe walk(plain, 1, CursorPosition(0, 0))
    walk(spaced, 1, CursorPosition(0, 0)).size shouldBe spaced.visualLines.size
  }

  "Moving up through a spaced heading" should "visit every row once, as it does without the space" in {
    val spaced = snapshotOf(spacedSettings)
    val plain  = snapshotOf(HeadingSettings.default)
    val last   = spaced.visualLines.last
    val bottom = CursorPosition(last.bufferLine, last.startColumn)

    walk(spaced, -1, bottom) shouldBe walk(plain, -1, bottom)
    walk(spaced, -1, bottom).size shouldBe spaced.visualLines.size
  }

  "The caret of each row" should "sit inside the row's text box, never in the space around it" in {
    val snapshot = snapshotOf(spacedSettings)
    val metrics  = TextRowMetrics.forSnapshot(LayoutRect(0, 0, 80, 40), CellMetrics(8, 1, 1), snapshot, measured = true)

    snapshot.visualLines.indices.foreach { row =>
      withClue(s"row $row: ") {
        metrics.cursorTopPx(row) should be >= metrics.bandTopPx(row) + snapshot.visualLines(row).spaceBeforePx
        metrics.cursorTopPx(row) + metrics.rowHeightPx(row) should be <=
          metrics.bandBottomPx(row) - snapshot.visualLines(row).spaceAfterPx
      }
    }
  }

  "A pixel row in the space" should "belong to the heading's nearest row, so a click there has a caret position" in {
    val snapshot = snapshotOf(spacedSettings)
    val metrics  = TextRowMetrics.forSnapshot(LayoutRect(0, 0, 80, 40), CellMetrics(8, 1, 1), snapshot, measured = true)

    snapshot.visualLines.zipWithIndex.filter(_._1.spaceBeforePx > 0).foreach { (line, row) =>
      withClue(s"row $row above: ") {
        metrics.visualRowAt(metrics.lineTopPx(row) - 1) shouldBe row
        metrics.visualRowAt(metrics.lineTopPx(row) - line.spaceBeforePx) shouldBe row
      }
    }
    snapshot.visualLines.zipWithIndex.filter(_._1.spaceAfterPx > 0).foreach { (line, row) =>
      withClue(s"row $row below: ") {
        val bottom = metrics.lineTopPx(row) + metrics.rowHeightPx(row)
        metrics.visualRowAt(bottom) shouldBe row
        metrics.visualRowAt(bottom + line.spaceAfterPx - 1) shouldBe row
      }
    }
    snapshot.visualLines.zipWithIndex.foreach { (_, row) =>
      snapshot.cursorForVisualRowAndXPx(metrics.visualRowAt(metrics.bandTopPx(row)), 0.0f) should not be empty
    }
  }
