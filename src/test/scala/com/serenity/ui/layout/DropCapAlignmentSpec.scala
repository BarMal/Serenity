package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import com.serenity.richtext.{ParagraphAlignment, ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Aligning a drop cap paragraph shifts each row by the room left between the row's right edge and the panel, after the
  * indent the glyph reserves: a right-aligned row ends at the panel and a centred row sits midway, never past it.
  */
class DropCapAlignmentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val serif = Font(Font.SERIF, Font.PLAIN, 14)

  private val prose =
    "Once upon a midnight dreary, while I pondered, weak and weary, over many a quaint and curious volume of " +
      "forgotten lore, while I nodded, nearly napping, suddenly there came a tapping, as of some one gently rapping, " +
      "rapping at my chamber door."

  private val base = TextLayoutSnapshot.defaultFontRenderContext()

  private val frcs = List(
    "1x" -> base,
    "2x" -> FontRenderContext(
      AffineTransform.getScaleInstance(2.0, 2.0),
      base.getAntiAliasingHint,
      base.getFractionalMetricsHint
    )
  )

  private def rows(alignment: ParagraphAlignment, lines: Int, widthPx: Int, frc: FontRenderContext) =
    val document = RichTextDocument(
      List(RichTextParagraph.plain(prose, alignment, ParagraphRole.dropCap(lines)))
    )
    val buffer = Buffer
      .fromString(BufferId(3), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 40)
      )
    TextLayoutSnapshot.fromBuffer(buffer, widthPx, serif, frc).visualLines

  for
    (scale, frc) <- frcs
    lines        <- List(2, 3)
    widthPx      <- List(230, 300, 333, 411)
  do
    s"a right-aligned $lines-line drop cap at $scale, ${widthPx}px" should "end every row at the panel's right edge" in
      rows(ParagraphAlignment.Right, lines, widthPx, frc).zipWithIndex.foreach { (row, index) =>
        withClue(s"row $index (offset ${row.xOffsetPx}, width ${row.widthPx}): ")(
          (row.xOffsetPx + row.widthPx) shouldBe widthPx.toFloat +- 0.01f
        )
      }

    s"a centred $lines-line drop cap at $scale, ${widthPx}px" should
      "leave equal room either side of every row, between its left edge and the panel" in {
        val left     = rows(ParagraphAlignment.Left, lines, widthPx, frc)
        val centered = rows(ParagraphAlignment.Center, lines, widthPx, frc)
        centered.length shouldBe left.length
        left.zip(centered).zipWithIndex.foreach {
          case ((unaligned, aligned), index) =>
            val room = widthPx - (unaligned.xOffsetPx + unaligned.widthPx)
            withClue(s"row $index (room $room): ") {
              (aligned.xOffsetPx - unaligned.xOffsetPx) shouldBe (room / 2.0f) +- 0.01f
              (aligned.xOffsetPx + aligned.widthPx) should be <= widthPx.toFloat
            }
        }
      }

    s"a justified $lines-line drop cap at $scale, ${widthPx}px" should "sit where the left-aligned rows sit" in {
      rows(ParagraphAlignment.Justify, lines, widthPx, frc).map(_.xOffsetPx) shouldBe
        rows(ParagraphAlignment.Left, lines, widthPx, frc).map(_.xOffsetPx)
    }
