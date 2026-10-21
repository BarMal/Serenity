package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import com.serenity.richtext.{
  InlineMark,
  ParagraphRole,
  RichTextDocument,
  RichTextParagraph,
  RichTextRun,
  RichTextStyle
}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rows beside a drop cap must wrap at the panel width less the cap's indent, and later rows at the full width. */
class DropCapWrapWidthSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val serif = Font(Font.SERIF, Font.PLAIN, 14)

  private val prose =
    "Once upon a midnight dreary, while I pondered, weak and weary, over many a quaint and curious volume of " +
      "forgotten lore, while I nodded, nearly napping, suddenly there came a tapping, as of some one gently rapping, " +
      "rapping at my chamber door."

  private val frcs = List(
    "1x" -> TextLayoutSnapshot.defaultFontRenderContext(),
    "2x" -> FontRenderContext(
      AffineTransform.getScaleInstance(2.0, 2.0),
      TextLayoutSnapshot.defaultFontRenderContext().getAntiAliasingHint,
      TextLayoutSnapshot.defaultFontRenderContext().getFractionalMetricsHint
    )
  )

  private def snapshot(lines: Int, widthPx: Int, frc: FontRenderContext): TextLayoutSnapshot =
    val document = RichTextDocument(List(RichTextParagraph.plain(prose).copy(role = ParagraphRole.dropCap(lines))))
    val buffer = Buffer
      .fromString(BufferId(1), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 40)
      )
    TextLayoutSnapshot.fromBuffer(buffer, widthPx, serif, frc)

  for
    (scale, frc) <- frcs
    lines        <- List(2, 3)
    widthPx      <- List(230, 300, 333, 411)
  do
    s"a $lines-line drop cap at $scale, ${widthPx}px" should "fill the rows beside the cap and the rows below it within the panel" in {
      val rows = snapshot(lines, widthPx, frc).visualLines
      rows.length should be > lines
      rows.take(lines).foreach(_.xOffsetPx should be > 0.0f)
      rows.drop(lines).foreach(_.xOffsetPx shouldBe 0.0f)
      rows.zipWithIndex.foreach { (row, index) =>
        withClue(s"row $index (inset ${row.xOffsetPx}, width ${row.widthPx}): ") {
          (row.xOffsetPx + row.widthPx) should be <= widthPx.toFloat
        }
      }
    }

  private def bodySnapshot(text: String, widthPx: Int, frc: FontRenderContext): TextLayoutSnapshot =
    val document = RichTextDocument(List(RichTextParagraph.plain(text)))
    val buffer = Buffer
      .fromString(BufferId(2), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 40)
      )
    TextLayoutSnapshot.fromBuffer(buffer, widthPx, serif, frc)

  private val fixedWidths = List(230, 262, 300, 333, 377, 411, 480)

  /** Panel widths at and beside the trailing edge of every word and the space after it, where a pixel of room decides
    * whether the word fits: a fixed list of widths meets those edges only by luck of the platform's font metrics.
    */
  private def wordEdgeWidths(lines: Int, frc: FontRenderContext): List[Int] =
    val indent  = math.round(snapshot(lines, 300, frc).visualLines(1).xOffsetPx)
    val visible = bodySnapshot(prose.drop(1), 100000, frc).visualLines.head
    val edges =
      visible.text.indices.filter(index => visible.text(index) == ' ').flatMap(index => visible.xForColumn(index + 1))
    edges
      .flatMap(edge => (-1 to 1).map(beside => indent + math.ceil(edge.toDouble).toInt + beside))
      .filter(_ >= 200)
      .toList

  for
    (scale, frc) <- frcs
    lines        <- List(2, 3)
    widthPx      <- (fixedWidths ++ wordEdgeWidths(lines, frc)).distinct.sorted
  do
    s"the first line of a $lines-line drop cap at $scale, ${widthPx}px" should
      "break where its visible text breaks at the panel width less the indent" in {
        val rows   = snapshot(lines, widthPx, frc).visualLines
        val indent = math.round(rows(1).xOffsetPx)
        val plain  = bodySnapshot(prose.drop(1), widthPx - indent, frc).visualLines
        withClue(s"indent $indent: ") {
          (rows.head.endColumn - 1) shouldBe plain.head.endColumn
        }
      }

  private def richSnapshot(
    paragraph: RichTextParagraph,
    widthPx: Int,
    frc: FontRenderContext,
    cache: WrappedLineCache,
    visibleLines: Int = 40
  ): TextLayoutSnapshot =
    val document = RichTextDocument(List(paragraph))
    val buffer = Buffer
      .fromString(BufferId(1), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = visibleLines)
      )
    TextLayoutSnapshot.fromBuffer(buffer, widthPx, serif, frc, wrapCache = cache)

  private def capped(text: String, lines: Int, firstRunLength: Int): RichTextParagraph =
    RichTextParagraph(
      List(
        RichTextRun(text.take(firstRunLength)),
        RichTextRun(text.drop(firstRunLength), RichTextStyle.empty.withMark(InlineMark.Bold))
      ).filter(_.text.nonEmpty),
      role = ParagraphRole.dropCap(lines)
    )

  for
    (scale, frc) <- frcs
    lines        <- List(2, 3)
    firstRun     <- List(1, 9, prose.length)
  do
    s"typing into a $lines-line drop cap paragraph at $scale (first run $firstRun chars)" should
      "equal an uncached wrap after every keystroke" in {
        val cache = WrappedLineCache.bounded()
        (0 to 60).foldLeft(prose) { (text, step) =>
          val edited    = if step == 0 then text else text.patch(2 + step % 40, "x", 0)
          val paragraph = capped(edited, lines, firstRun)
          withClue(s"step $step: ") {
            richSnapshot(paragraph, 300, frc, cache).visualLines shouldBe
              richSnapshot(paragraph, 300, frc, WrappedLineCache.Uncached).visualLines
          }
          edited
        }
      }

    s"a $lines-line drop cap paragraph at $scale (first run $firstRun chars) scrolled to few visible lines" should
      "give the same rows as the full wrap" in {
        val paragraph = capped(prose, lines, firstRun)
        val full      = richSnapshot(paragraph, 300, frc, WrappedLineCache.Uncached).visualLines
        (1 to full.length).foreach { visible =>
          richSnapshot(paragraph, 300, frc, WrappedLineCache.bounded(), visible).visualLines shouldBe full.take(visible)
        }
      }
