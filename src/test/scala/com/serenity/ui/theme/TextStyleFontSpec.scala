package com.serenity.ui.theme

import java.awt.Font

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The single font deriver shared by the draw path (`Java2DRenderSurface.enableStyle`) and the measured-layout caret
  * measurement, so glyph advances and drawn glyphs can never disagree.
  */
class TextStyleFontSpec extends AnyFlatSpec with Matchers:

  private val base = Font("SansSerif", Font.PLAIN, 12)

  "TextStyle.styledFont" should "keep the base size when the style specifies none" in {
    TextStyle.styledFont(base, TextStyle.normal).getSize2D shouldBe 12.0f
  }

  it should "apply an explicit font size" in {
    TextStyle.styledFont(base, TextStyle(fontSize = Some(24.0f))).getSize2D shouldBe 24.0f
  }

  it should "never derive a font smaller than 1pt" in {
    TextStyle.styledFont(base, TextStyle(fontSize = Some(0.0f))).getSize2D shouldBe 1.0f
  }

  it should "apply bold and italic" in {
    val font = TextStyle.styledFont(base, TextStyle(isBold = true, isItalic = true))
    font.isBold shouldBe true
    font.isItalic shouldBe true
  }

  it should "switch font family when one is given" in {
    TextStyle.styledFont(base, TextStyle(fontFamily = Some("Serif"), fontSize = Some(18.0f))).getSize2D shouldBe 18.0f
  }
