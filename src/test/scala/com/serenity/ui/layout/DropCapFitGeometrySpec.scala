package com.serenity.ui.layout

import java.awt.Font

import com.serenity.MockRenderSurface
import com.serenity.config.AppConfig
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, Buffer, BufferId, RichTextState, TextVisualLine, Viewport}
import com.serenity.ui.renderer.DropCapRenderer
import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A drop cap spanning N lines is fitted the way a printed one is: the top of its letter is level with the top of the
  * capitals on its first line, and its baseline is the baseline of its last spanned line. Every line beside it starts
  * at one x, and the room it reserves covers the whole drawn glyph, descenders included.
  */
class DropCapFitGeometrySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val frc = TextLayoutSnapshot.defaultFontRenderContext()

  private val prose =
    "he quick brown fox jumps over the lazy dog while the afternoon sun settles over the hills and the river runs " +
      "slow beneath the old stone bridge, and nobody on the road says a word about the weather. "

  final private case class Sample(snapshot: TextLayoutSnapshot, font: Font, lines: Int)

  private def sample(
    letter: String,
    lines: Int,
    sizePx: Float,
    copies: Int = 3,
    gapEm: Float = 0.25f
  ): Sample =
    val font     = BundledInterFont.plain(sizePx)
    val text     = letter + (prose * copies)
    val document = RichTextDocument(List(RichTextParagraph.plain(text, role = ParagraphRole.dropCap(lines))))
    val buffer = Buffer
      .fromString(BufferId(40), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 20)
      )
    val snapshot = TextLayoutSnapshot.fromBuffer(
      buffer,
      panelWidthPx = (sizePx * 40).toInt,
      font,
      proseScale = sizePx / 14.0f,
      dropCapGapEm = gapEm
    )
    Sample(snapshot, font, lines)

  final private case class Drawn(
      baselinePx: Float,
      letterTopPx: Float,
      letterBottomPx: Float,
      inkRightPx: Float,
      line: TextVisualLine
  )

  /** What the renderer paints for the cap, read from the pixel run it asks the surface for. */
  private def drawn(sample: Sample, letter: String): Drawn =
    val home    = sample.snapshot.visualLines.head
    val theme   = AppState.empty.persisted.theme
    val surface = MockRenderSurface(2000, 600)
    val (drawLine, _) = DropCapRenderer.adjustHomeLineDraw(
      snapshot = sample.snapshot,
      visualLine = home,
      styledSegments = None,
      measured = true,
      surface = surface,
      baseFont = sample.font,
      frc = Some(frc),
      theme = theme,
      glyphColour = theme.highlighted.foreground,
      xOriginPx = 0.0f,
      lineTopPx = 0
    )
    val call       = surface.drawRunPxCalls.head
    val glyphStyle = surface.styleCalls.collectFirst { case surface.StyleCall("enable", style) => style }.get
    val glyphFont  = TextStyle.styledFont(sample.font, glyphStyle)
    val ink        = glyphFont.createGlyphVector(frc, letter).getVisualBounds
    val baseline   = (call.yPx + call.ascentPx).toFloat
    Drawn(
      baseline,
      baseline + ink.getMinY.toFloat,
      baseline + ink.getMaxY.toFloat,
      math.ceil(ink.getMaxX).toFloat,
      drawLine
    )

  private def capHeightOf(font: Font): Float =
    -font.createGlyphVector(frc, "H").getVisualBounds.getMinY.toFloat

  private def topOfRow(snapshot: TextLayoutSnapshot, row: Int): Int = snapshot.rowHeightsPx.take(row).sum

  private val tolerancePx = 1.0f

  for
    (label, sizePx) <- List("1x" -> 14.0f, "2x" -> 28.0f)
    lines           <- List(2, 3)
  do
    s"A $lines-line drop cap at $label" should
      "put the top of its letter level with the top of the capitals on the first line" in {
        val s            = sample("T", lines, sizePx)
        val home         = s.snapshot.visualLines.head
        val firstCapsTop = home.ascentPx - capHeightOf(s.font)

        drawn(s, "T").letterTopPx shouldBe firstCapsTop +- tolerancePx
      }

    it should s"put the baseline of its letter on the baseline of line $lines" in {
      val s            = sample("T", lines, sizePx)
      val lastBaseline = topOfRow(s.snapshot, lines - 1) + s.snapshot.visualLines(lines - 1).ascentPx

      drawn(s, "T").baselinePx shouldBe lastBaseline.toFloat +- tolerancePx
    }

    it should "start every spanned line at one x, the first line included" in {
      val s     = sample("T", lines, sizePx)
      val shown = drawn(s, "T")
      val starts = shown.line.xForColumn(shown.line.startColumn).get +: s.snapshot.visualLines
        .slice(1, lines)
        .map(line => line.xForColumn(line.startColumn).get)

      starts should have size lines.toLong
      starts.foreach(_ shouldBe starts.head +- 0.5f)
    }

    it should "map the caret after the cap to where the first line's text starts" in {
      val s      = sample("T", lines, sizePx)
      val shown  = drawn(s, "T")
      val home   = s.snapshot.visualLines.head
      val second = s.snapshot.visualLines(1)

      home.xForColumn(1).get shouldBe second.xForColumn(second.startColumn).get +- 0.5f
      home.nearestColumnForXPx(second.xForColumn(second.startColumn).get) shouldBe 1
      shown.line.xForColumn(shown.line.startColumn).get shouldBe home.xForColumn(1).get +- 0.5f
    }

  for
    (label, sizePx) <- List("1x" -> 14.0f, "2x" -> 28.0f)
    gapEm           <- List(0.0f, 0.25f, 0.5f, 1.0f, 2.0f)
  do
    s"The gap beside a drop cap at $label" should s"be $gapEm em of the body font between the glyph's ink and the text" in {
      val s     = sample("T", 2, sizePx, gapEm = gapEm)
      val shown = drawn(s, "T")
      val start = s.snapshot.visualLines(1).xForColumn(s.snapshot.visualLines(1).startColumn).get

      start shouldBe shown.inkRightPx + gapEm * sizePx +- tolerancePx
    }

  for
    (label, sizePx) <- List("1x" -> 14.0f, "2x" -> 28.0f)
    letter          <- List("I", "W", "T", "Q", "J")
  do
    s"The indent beside a drop cap $letter at $label" should
      "be the ink's right edge rounded up plus the gap, on every spanned line" in {
        val s        = sample(letter, 3, sizePx)
        val shown    = drawn(s, letter)
        val expected = shown.inkRightPx + AppConfig.default.documentConfig.dropCapGapEm * sizePx
        val starts = shown.line.xForColumn(shown.line.startColumn).get +: s.snapshot.visualLines
          .slice(1, 3)
          .map(line => line.xForColumn(line.startColumn).get)

        starts should have size 3L
        starts.foreach(_ shouldBe expected +- tolerancePx)
      }

  "The default gap beside a drop cap" should "be a quarter em of the body font" in {
    AppConfig.default.documentConfig.dropCapGapEm shouldBe 0.25f
  }

  for letter <- List("Q", "J") do
    s"A drop cap $letter" should "be covered by the room its paragraph reserves when the paragraph is short" in {
      val s     = sample(letter, 2, 14.0f, copies = 0)
      val shown = drawn(s, letter)
      val reservedBottom =
        s.snapshot.rowHeightsPx.sum

      s.snapshot.visualLines should have size 1
      shown.letterBottomPx should be <= reservedBottom.toFloat + tolerancePx
    }

    it should "be covered by the rows its glyph span reaches when the paragraph is long" in {
      val s    = sample(letter, 2, 14.0f)
      val span = DropCapLayout.glyphRowSpan(s.snapshot)(0)

      drawn(s, letter).letterBottomPx should be <= topOfRow(s.snapshot, span.end).toFloat + tolerancePx
    }
