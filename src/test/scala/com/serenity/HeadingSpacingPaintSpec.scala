package com.serenity

import java.awt.image.BufferedImage

import com.serenity.config.{AppConfig, HeadingsConfig}
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun}
import com.serenity.state.models.*
import com.serenity.ui.layout.TextRowMetrics
import com.serenity.ui.renderer.EditorPaneRenderPlan
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a frame paints around spaced headings: the space is blank (no selection, no caret, no line number inks into
  * it), and each row's text and line number sit in the row's own box, not in the space beside it.
  */
class HeadingSpacingPaintSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private val scale = 2.0

  private def paragraph(text: String, role: ParagraphRole = ParagraphRole.Body): RichTextParagraph =
    RichTextParagraph(List(RichTextRun(text)), role = role)

  private val document = RichTextDocument(
    List(
      paragraph("Opening paragraph of the book, short enough for one row."),
      paragraph("Chapter One", ParagraphRole.Heading(1)),
      paragraph("The first paragraph under the chapter, also short."),
      paragraph(
        "A section heading that is long enough that it must wrap onto a second row " * 2,
        ParagraphRole.Heading(2)
      ),
      paragraph("The last paragraph of the page."),
      paragraph("Another chapter", ParagraphRole.Heading(1)),
      paragraph("Closing words.")
    )
  )

  private val headings = HeadingsConfig()
    .withLevel(1)(_.copy(spaceBefore = Some(2.0f), spaceAfter = Some(1.0f)))
    .withLevel(2)(_.copy(spaceBefore = Some(1.5f), spaceAfter = Some(1.0f)))

  private def richState(
    config: AppConfig,
    cursor: CursorPosition = CursorPosition(0, 0),
    anchor: Option[CursorPosition] = None
  ): AppState =
    val base   = stateOf(document.plainText, cursor)
    val buffer = base.persisted.buffers(bufferId)
    val rich = buffer.copy(
      richText = RichTextState(richTextDocument = Some(document)),
      editing = EditingState.fromCursors(List(Cursor(cursor, selectionAnchor = anchor)))
    )
    base.copy(persisted =
      base.persisted.copy(
        buffers = Map(bufferId -> rich),
        config = withMargins(config.withoutStatusLine).withWordWrap(true)
      )
    )

  private def drawn(state: AppState): (BufferedImage, EditorPaneRenderPlan) =
    Session(scale).draw(state, Damage.Everything)

  final private case class Box(row: TextVisualLine, bandTop: Int, textTop: Int, textBottom: Int, bandBottom: Int)

  private def boxes(plan: EditorPaneRenderPlan): Vector[Box] =
    val snapshot = plan.snapshots(paneId)
    val content  = plan.paneLayouts(paneId).contentRect
    val metrics  = TextRowMetrics.forSnapshot(content, cellMetrics, snapshot, measured = snapshot.usesMeasuredLayout)
    snapshot.visualLines.zipWithIndex.map { (row, index) =>
      Box(
        row,
        metrics.bandTopPx(index),
        metrics.lineTopPx(index),
        metrics.lineTopPx(index) + metrics.rowHeightPx(index),
        metrics.bandBottomPx(index)
      )
    }

  private def deviceY(y: Int): Int = (y * scale).toInt

  private def pixels(image: BufferedImage, xFrom: Int, xTo: Int, yFrom: Int, yTo: Int): Seq[Int] =
    for
      y <- deviceY(yFrom) until deviceY(yTo).min(image.getHeight)
      x <- (xFrom * scale).toInt until (xTo * scale).toInt.min(image.getWidth)
    yield image.getRGB(x, y) & 0xffffff

  private def paneX(plan: EditorPaneRenderPlan): (Int, Int) =
    val content = plan.paneLayouts(paneId).contentRect
    (cellMetrics.toPixelX(content.x), cellMetrics.toPixelX(content.right))

  private val theme = Theme.light

  "The fixture" should "lay out spaced headings with a gap above and below them" in {
    val (_, plan) = drawn(richState(AppConfig.conventional.withHeadingsConfig(headings)))
    val rows      = boxes(plan)

    rows.exists(box => box.textTop - box.bandTop >= 8) shouldBe true
    rows.exists(box => box.bandBottom - box.textBottom >= 8) shouldBe true
    rows.map(_.bandBottom).init shouldBe rows.map(_.bandTop).tail
  }

  "The space around a heading" should "stay the pane's background when a selection runs through it" in {
    val state = richState(
      AppConfig.conventional.withHeadingsConfig(headings),
      cursor = CursorPosition(4, 5),
      anchor = Some(CursorPosition(0, 3))
    )
    val (image, plan) = drawn(state)
    val (left, right) = paneX(plan)
    val highlight     = theme.highlighted.background.toAwt.getRGB & 0xffffff
    val background    = theme.background.toAwt.getRGB & 0xffffff
    val rows          = boxes(plan)

    val gaps = rows
      .flatMap(box => List((box.bandTop, box.textTop), (box.textBottom, box.bandBottom)))
      .filter((a, b) => b - a >= 4)
    gaps should not be empty
    gaps.foreach { (from, to) =>
      withClue(s"gap $from-$to: ")(pixels(image, left, right, from + 1, to - 1).toSet should contain only background)
    }
    rows.filter(box => box.row.bufferLine >= 1 && box.row.bufferLine <= 3).foreach { box =>
      withClue(s"line ${box.row.bufferLine}: ")(
        pixels(image, left, right, box.textTop, box.textBottom).contains(highlight) shouldBe true
      )
    }
  }

  "A line number beside a spaced heading" should "sit in its row's own box, with the space beside it left empty" in {
    val state         = richState(AppConfig.conventional.withHeadingsConfig(headings).withLineNumbers(true))
    val (image, plan) = drawn(state)
    val numbers       = plan.layoutContract.lineNumberRect.getOrElse(fail("expected a line number column"))
    val from          = cellMetrics.toPixelX(numbers.x)
    val to            = cellMetrics.toPixelX(numbers.right) - cellMetrics.charWidth
    val rows          = boxes(plan)
    val gutterBg      = theme.panel.background.toAwt.getRGB & 0xffffff

    val numbered = rows.filter(box => box.row.startColumn == 0)
    numbered.size should be >= 6
    numbered.foreach { box =>
      withClue(s"line ${box.row.bufferLine}: ") {
        val ink = pixels(image, from, to, box.textTop, box.textBottom).exists(_ != gutterBg)
        ink shouldBe true
      }
    }
    rows
      .flatMap(box => List((box.bandTop, box.textTop), (box.textBottom, box.bandBottom)))
      .filter((a, b) => b - a >= 4)
      .foreach { (top, bottom) =>
        withClue(s"gap $top-$bottom: ")(pixels(image, from, to, top + 1, bottom - 1).toSet should contain only gutterBg)
      }
  }

  "A caret on a spaced heading" should "be as tall as the heading's text and stay inside its box" in {
    val state     = richState(AppConfig.conventional.withHeadingsConfig(headings), cursor = CursorPosition(1, 3))
    val (_, plan) = drawn(state)
    val snapshot  = plan.snapshots(paneId)
    val content   = plan.paneLayouts(paneId).contentRect
    val metrics   = TextRowMetrics.forSnapshot(content, cellMetrics, snapshot, measured = snapshot.usesMeasuredLayout)

    snapshot.visualLines.zipWithIndex.foreach { (_, index) =>
      withClue(s"row $index: ") {
        metrics.cursorTopPx(index) shouldBe metrics.lineTopPx(index)
        metrics.cursorTopPx(index) should be >= metrics.bandTopPx(index)
        metrics.cursorTopPx(index) + metrics.rowHeightPx(index) should be <= metrics.bandBottomPx(index)
      }
    }
    val heading = snapshot.visualLines.indexWhere(_.bufferLine == 1)
    metrics.cursorTopPx(heading) - metrics.bandTopPx(heading) shouldBe snapshot.visualLines(heading).spaceBeforePx
  }
