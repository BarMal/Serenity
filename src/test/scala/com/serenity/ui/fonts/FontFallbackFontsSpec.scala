package com.serenity.ui.fonts

import java.awt.Font
import java.awt.font.TextAttribute
import java.nio.file.Paths

import com.serenity.text.fallback.FontSlot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The fonts a fallback chain draws each slot with, derived from the primary so a fallback glyph does not change size,
  * weight or ligature setting mid-line.
  */
class FontFallbackFontsSpec extends AnyFlatSpec with Matchers:

  private val codeFont =
    Font
      .createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile)
      .deriveFont(Font.BOLD, 14.0f)

  private val chain = FontFallback(Vector("Material Icons Round"))

  "A chain" should "end in the Dialog logical composite at the primary's style and size" in {
    chain.slotCount shouldBe 3
    val system = chain.fontFor(FontSlot(2), codeFont)
    system.getName shouldBe Font.DIALOG
    system.getStyle shouldBe Font.BOLD
    system.getSize2D shouldBe 14.0f
  }

  it should "derive each fallback to the primary's style and size" in {
    chain.fontFor(FontSlot(1), codeFont).getStyle shouldBe Font.BOLD
    chain.fontFor(FontSlot(1), codeFont).getSize2D shouldBe 14.0f
  }

  it should "carry the primary's ligature setting onto the fallbacks" in {
    val ligatures = java.util.Map.of[TextAttribute, AnyRef](TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON)
    chain.fontFor(FontSlot(1), codeFont.deriveFont(ligatures)).getAttributes.get(TextAttribute.LIGATURES) shouldBe
      TextAttribute.LIGATURES_ON
  }

  it should "draw the primary slot with the primary itself" in {
    chain.fontFor(FontSlot.Primary, codeFont) should be theSameInstanceAs codeFont
  }
