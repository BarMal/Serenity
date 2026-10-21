package com.serenity.ui.theme

import java.awt.Font
import java.awt.font.{FontRenderContext, TextAttribute}
import java.nio.file.Paths

import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2360: bold text in a bundled code font is drawn with the font's own Bold face, whose advances are the regular's,
  * not with the JDK's synthetic bold (a `Font.createFont` font keeps the one file it was made from), which is about 5%
  * wider and so drifts off a cell grid. A font that is not bundled keeps `deriveFont`, which finds a named family's
  * real bold when it has one.
  */
class TextStyleBoldFaceSpec extends AnyFlatSpec with Matchers:

  private val frc   = FontRenderContext(null, true, true)
  private val sizes = Vector(8.0f, 12.0f, 14.0f, 20.0f, 22.0f)

  private def advances(font: Font): Vector[Double] =
    (0x20 to 0x7e)
      .map(char => font.createGlyphVector(frc, char.toChar.toString).getGlyphMetrics(0).getAdvance.toDouble)
      .toVector

  private def resource(name: String): Font =
    Font.createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource(s"/fonts/$name").toURI).toFile)

  private def bundledCode(size: Float, ligatures: Boolean = false): Font =
    FontLoader.previewCodeFont(FontConfig(fontSize = size, enableLigatures = ligatures))

  "TextStyle.styledFont on the bundled code font" should "draw bold with the Bold face" in
    sizes.foreach(size =>
      TextStyle.styledFont(bundledCode(size), TextStyle.bold).getFontName shouldBe "Monaspace Neon Bold"
    )

  it should "report the font as bold, so the fallback chain draws bold too" in {
    TextStyle.styledFont(bundledCode(14.0f), TextStyle.bold).isBold shouldBe true
    TextStyle.styledFont(bundledCode(14.0f), TextStyle.boldItalic).isBold shouldBe true
  }

  it should "give bold every advance the regular face has" in
    sizes.foreach { size =>
      val base = bundledCode(size)
      withClue(s"$size px: ") {
        advances(TextStyle.styledFont(base, TextStyle.bold)) shouldBe advances(
          TextStyle.styledFont(base, TextStyle.normal)
        )
      }
    }

  it should "draw bold italic with the Bold face too, at the regular advances" in
    sizes.foreach { size =>
      val base = bundledCode(size)
      val font = TextStyle.styledFont(base, TextStyle.boldItalic)
      withClue(s"$size px: ") {
        font.getFontName shouldBe "Monaspace Neon Bold"
        font.isItalic shouldBe true
        advances(font) shouldBe advances(TextStyle.styledFont(base, TextStyle.normal))
      }
    }

  it should "keep italic, regular and underlined text as they were" in {
    val base = bundledCode(14.0f)
    TextStyle.styledFont(base, TextStyle.normal).getFontName shouldBe base.getFontName
    TextStyle.styledFont(base, TextStyle.italic).isItalic shouldBe true
    TextStyle.styledFont(base, TextStyle.underlined).getFontName shouldBe base.getFontName
  }

  it should "keep the size a style asks for, the ligature setting and strikethrough" in {
    val withLigatures = bundledCode(14.0f, ligatures = true)
    val sized         = TextStyle.styledFont(withLigatures, TextStyle(isBold = true, fontSize = Some(18.0f)))
    sized.getSize2D shouldBe 18.0f
    FontLoader.ligaturesEnabled(sized) shouldBe true
    FontLoader.ligaturesEnabled(TextStyle.styledFont(bundledCode(14.0f), TextStyle.bold)) shouldBe false
    TextStyle
      .styledFont(withLigatures, TextStyle(isBold = true, isStrikethrough = true))
      .getAttributes
      .get(
        TextAttribute.STRIKETHROUGH
      ) shouldBe TextAttribute.STRIKETHROUGH_ON
  }

  "TextStyle.styledFont on Courier Prime, bundled for manuscripts and used as a test font" should "draw bold with its Bold face" in {
    val base = resource("CourierPrime-Regular.ttf").deriveFont(Font.PLAIN, 22.0f)
    val bold = TextStyle.styledFont(base, TextStyle.bold)
    bold.getFontName shouldBe "Courier Prime Bold"
    advances(bold) shouldBe advances(base)
  }

  "TextStyle.styledFont on a font that is not bundled" should "derive bold as it always did" in {
    val base = resource("SerenityProseTest.ttf").deriveFont(Font.PLAIN, 14.0f)
    TextStyle.styledFont(base, TextStyle.bold) shouldBe base.deriveFont(Font.BOLD, 14.0f)
    val named = Font(Font.SERIF, Font.PLAIN, 14)
    TextStyle.styledFont(named, TextStyle.bold) shouldBe named.deriveFont(Font.BOLD, 14.0f)
  }
