package com.serenity.ui.layout

import java.awt.{Font, GraphicsEnvironment}
import java.nio.file.Paths

import com.serenity.state.models.TextVisualLine
import com.serenity.ui.fonts.FontFallback
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866: text the primary font has no glyph for is measured in a fallback font, so carets sit where the fallback
  * glyphs are drawn. The fallback is a test subset font (中 and 文 one em wide, → 0.8 em, ascent 1.4 em and descent
  * 0.4 em), so nothing depends on what the machine's `Dialog` font reaches.
  */
class FontFallbackLayoutSpec extends AnyFlatSpec with Matchers:

  private val frc = TextLayoutSnapshot.defaultFontRenderContext()

  private val subset: Font =
    val font = Font.createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource("/fonts/SerenityFallbackTest.ttf").toURI).toFile)
    val _    = GraphicsEnvironment.getLocalGraphicsEnvironment.registerFont(font)
    font

  private val fallback = FontFallback(Vector(subset.getFamily))

  private val sizePx = 14.0f

  private def bundled(resource: String): Font =
    Font
      .createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource(resource).toURI).toFile)
      .deriveFont(Font.PLAIN, sizePx)

  private val codeFont  = bundled("/fonts/MonaspaceNeon-Regular.otf")
  private val proseFont = Font(Font.SERIF, Font.PLAIN, 14)

  private def context(chain: FontFallback = fallback) =
    RichTextContext(None, 1.0f, dropCapsEnabled = true, fallback = chain)

  private def rowOf(text: String, font: Font, chain: FontFallback = fallback): TextVisualLine =
    val rows = TextLayoutSnapshot.boundedVisualLinesForText(
      text,
      0,
      10_000,
      font,
      frc,
      forceCellLayout = false,
      richText = context(chain)
    )
    rows should have size 1
    rows.head

  private def advanceOf(text: String, font: Font): Float =
    rowOf(text, font).widthPx

  "A prose line with a codepoint its font lacks" should "put the next caret one fallback advance later" in {
    val row = rowOf("a中b", codeFont)
    row.row.caretXs(2) - row.row.caretXs(1) shouldBe sizePx +- 0.01f
  }

  it should "end the row where the fallback glyphs end" in {
    val row = rowOf("a中文b", codeFont)
    row.widthPx shouldBe (advanceOf("a", codeFont) + 2 * sizePx + advanceOf("b", codeFont)) +- 0.01f
  }

  it should "measure each fallback in its own advance" in {
    val row = rowOf("→中", codeFont)
    row.row.caretXs(1) shouldBe 0.8f * sizePx +- 0.01f
  }

  it should "grow the row to fit the taller fallback glyphs in a proportional font" in {
    val plain  = rowOf("ab", proseFont)
    val mixed  = rowOf("a中b", proseFont)
    val tallPx = math.ceil(1.8 * sizePx).toInt
    mixed.heightPx should be >= tallPx
    mixed.heightPx should be > plain.heightPx
    mixed.ascentPx should be >= math.ceil(1.4 * sizePx).toInt
  }

  "A line the primary font covers" should "keep a resolver with no fallback runs" in {
    val inputs = LineLayout.lineLayoutInputs(codeFont, frc, measuredLayout = true, context(), 0, 5, "hello")
    inputs.resolver.runs shouldBe empty
  }

  it should "measure exactly as it did without a fallback chain" in {
    val plain = rowOf("hello world", proseFont, FontFallback.Default)
    plain.row shouldBe TextLayoutSnapshot
      .boundedVisualLinesForText("hello world", 0, 10_000, proseFont, frc, richText = RichTextContext.plain)
      .head
      .row
  }
