package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Font, GraphicsEnvironment}
import java.nio.file.Paths

import com.serenity.ReferenceFrames
import com.serenity.state.models.TextVisualLine
import com.serenity.ui.color.RenderColor
import com.serenity.ui.fonts.{FontFallback, FontLoader}
import com.serenity.ui.layout.{CellMetrics, RichTextContext, TextLayoutSnapshot}
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866 PR B: bold text takes the bundled Bold emoji face, in the measured layout and in paint. Java2D cannot select a
  * variable font's weight axis, so a bold glyph from a variable file would be synthetic emboldening, which also widens
  * the advance; the emoji advance must not change with weight, and the glyph must get heavier.
  */
class EmojiWeightSpec extends AnyFlatSpec with Matchers:

  private val frc       = TextLayoutSnapshot.defaultFontRenderContext()
  private val sizePx    = 28.0f
  private val white     = RenderColor.fromRgba(255, 255, 255)
  private val black     = RenderColor.fromRgba(0, 0, 0)
  private val whiteArgb = 0xffffffff
  private val grin      = Character.toString(0x1f600)

  private val fallback = FontFallback(Vector.empty, FontLoader.emojiFontFamily)

  private val proseFont =
    val font = Font.createFont(
      Font.TRUETYPE_FONT,
      Paths.get(getClass.getResource("/fonts/SerenityProseTest.ttf").toURI).toFile
    )
    val _ = GraphicsEnvironment.getLocalGraphicsEnvironment.registerFont(font)
    font

  private def fontOf(style: Int): Font = proseFont.deriveFont(style, sizePx)

  private def rowOf(text: String, style: Int): TextVisualLine =
    TextLayoutSnapshot
      .boundedVisualLinesForText(
        text,
        0,
        10_000,
        fontOf(style),
        frc,
        richText = RichTextContext(None, 1.0f, dropCapsEnabled = true, fallback = fallback)
      )
      .head

  private def emojiAdvance(style: Int): Float =
    rowOf(s"a${grin}b", style).widthPx - rowOf("ab", style).widthPx

  private def textStyle(style: Int): TextStyle = if style == Font.BOLD then TextStyle.bold else TextStyle.normal

  private def paint(style: Int): BufferedImage =
    val line      = rowOf(grin, style)
    val metrics   = CellMetrics(charWidth = 8, lineHeight = line.heightPx, ascent = line.ascentPx)
    val reference = ReferenceFrames.over(120, 80, metrics, fontOf(Font.PLAIN), 120, 80)
    val surface   = reference.surface
    surface.setForegroundColor(black)
    surface.setBackgroundColor(white)
    surface.clearViewport(white)
    CharacterRenderer.renderMeasuredLine(
      surface,
      4.0f,
      4,
      line.heightPx,
      line.ascentPx,
      line,
      Theme.light,
      styledSegments = Some(List(StyledText(line.text, textStyle(style), black, white)))
    )
    reference.image

  private def ink(image: BufferedImage): Int =
    (for
      y <- 0 until image.getHeight
      x <- 0 until image.getWidth
      if image.getRGB(x, y) != whiteArgb
    yield 1).size

  "Emoji in a measured row" should "measure the same advance in bold as in regular" in {
    emojiAdvance(Font.BOLD) shouldBe emojiAdvance(Font.PLAIN) +- 0.01f
  }

  it should "measure the advance of the bundled emoji face, not zero" in {
    emojiAdvance(Font.PLAIN) should be > (sizePx / 2)
  }

  "A painted emoji" should "get heavier when the text is bold" in {
    ink(paint(Font.BOLD)) should be > (ink(paint(Font.PLAIN)) * 1.05).toInt
  }

  it should "draw something at all in regular" in {
    ink(paint(Font.PLAIN)) should be > 50
  }
