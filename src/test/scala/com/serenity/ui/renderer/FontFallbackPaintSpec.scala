package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Font, GraphicsEnvironment}
import java.nio.file.Paths

import com.serenity.state.models.TextVisualLine
import com.serenity.ui.color.RenderColor
import com.serenity.ui.fonts.FontFallback
import com.serenity.ui.layout.{CellMetrics, RichTextContext, TextLayoutSnapshot}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866: the glyphs of a fallback run are drawn in the fallback font, at the x the row measured them at. The fallback
  * is a test subset font whose 中 is a box 0.9 em wide and 0.8 em tall starting 0.05 em in, so its ink says where and in
  * which font a glyph was drawn.
  */
class FontFallbackPaintSpec extends AnyFlatSpec with Matchers:

  private val frc     = TextLayoutSnapshot.defaultFontRenderContext()
  private val sizePx  = 14.0f
  private val originX = 4.0f
  private val white   = RenderColor.fromRgba(255, 255, 255)
  private val black   = RenderColor.fromRgba(0, 0, 0)
  private val whiteArgb = 0xffffffff

  private def registered(resource: String): Font =
    val font = Font.createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource(resource).toURI).toFile)
    val _    = GraphicsEnvironment.getLocalGraphicsEnvironment.registerFont(font)
    font

  private val fallback  = FontFallback(Vector(registered("/fonts/SerenityFallbackTest.ttf").getFamily))
  private val proseFont = registered("/fonts/SerenityProseTest.ttf").deriveFont(Font.PLAIN, sizePx)
  private val codeFont =
    Font
      .createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile)
      .deriveFont(Font.PLAIN, sizePx)

  private def rowOf(text: String, font: Font): TextVisualLine =
    TextLayoutSnapshot
      .boundedVisualLinesForText(
        text,
        0,
        10_000,
        font,
        frc,
        richText = RichTextContext(None, 1.0f, dropCapsEnabled = true, fallback = fallback)
      )
      .head

  private def paint(line: TextVisualLine, font: Font): BufferedImage =
    val image   = new BufferedImage(200, 60, BufferedImage.TYPE_INT_ARGB)
    val metrics = CellMetrics(charWidth = 8, lineHeight = line.heightPx, ascent = line.ascentPx)
    val surface = new Java2DRenderSurface(image, metrics, font, _ => (), 200, 60, 1.0, 1.0)
    surface.setForegroundColor(black)
    surface.setBackgroundColor(white)
    surface.clearViewport(white)
    CharacterRenderer.renderMeasuredLine(
      surface,
      originX,
      4,
      line.heightPx,
      line.ascentPx,
      line,
      Theme.light
    )
    image

  /** The columns between `fromX` and `untilX` that hold any ink. */
  private def inkColumns(image: BufferedImage, fromX: Int, untilX: Int): Vector[Int] =
    (fromX until untilX).filter(x => (0 until image.getHeight).exists(y => image.getRGB(x, y) != whiteArgb)).toVector

  "A fallback glyph in a prose row" should "be drawn in the fallback font where its caret stops say" in {
    val line  = rowOf("a中b", proseFont)
    val image = paint(line, proseFont)
    val from  = (originX + line.row.caretXs(1)).toInt
    val until = math.ceil((originX + line.row.caretXs(2)).toDouble).toInt
    val ink   = inkColumns(image, from, until)
    ink should not be empty
    // The subset 中 is a box from 0.05 em to 0.95 em of its one-em cell.
    (ink.head - (originX + line.row.caretXs(1))) shouldBe (0.05f * sizePx) +- 1.0f
    (ink.last + 1 - (originX + line.row.caretXs(1))) shouldBe (0.95f * sizePx) +- 1.0f
  }

  "A wide character in a code row" should "be drawn at its snapped caret, leaving the rest of its two cells empty" in {
    val line  = rowOf("中a", codeFont)
    val image = paint(line, codeFont)
    val cell  = line.row.caretXs(1) / 2
    val gap   = inkColumns(image, (originX + 0.96f * sizePx).toInt + 1, (originX + line.row.caretXs(1) - 1).toInt)
    gap shouldBe empty
    val letter = inkColumns(image, (originX + line.row.caretXs(1)).toInt, (originX + line.row.caretXs(2)).toInt + 1)
    letter should not be empty
    letter.head.toFloat should be >= (originX + 2 * cell - 1)
  }
