package com.serenity.ui.fonts

import java.awt.Font
import java.awt.font.FontRenderContext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866 PR B: the bundled monochrome emoji family has a real Regular and a real Bold face. Java2D cannot select a
  * variable font's weight axis, so a bold request has to find a face of its own; synthetic emboldening would also widen
  * the advance.
  */
class NotoEmojiFontSpec extends AnyFlatSpec with Matchers:

  private val frc    = FontRenderContext(null, true, true)
  private val grin   = Character.toString(0x1f600)
  private val sizePx = 40.0f

  private def family: String =
    FontLoader.emojiFontFamily.getOrElse(fail("the bundled emoji font did not register"))

  private def face(style: Int): Font = Font(family, style, 1).deriveFont(style, sizePx)

  private def advanceOf(font: Font): Double =
    font.createGlyphVector(frc, grin).getGlyphMetrics(0).getAdvance.toDouble

  "The bundled emoji font" should "register as one family with a Regular and a Bold face" in {
    face(Font.PLAIN).getFontName should include("Regular")
    face(Font.BOLD).getFontName should include("Bold")
    face(Font.BOLD).getFamily shouldBe face(Font.PLAIN).getFamily
  }

  it should "cover emoji and the dingbat sparkle" in {
    face(Font.PLAIN).canDisplay(0x1f600) shouldBe true
    face(Font.PLAIN).canDisplay(0x2728) shouldBe true
  }

  it should "keep the advance when it is bold, because bold is its own face and not synthetic emboldening" in {
    advanceOf(face(Font.BOLD)) shouldBe advanceOf(face(Font.PLAIN)) +- 0.001
  }

  it should "draw a heavier glyph when it is bold" in {
    def inkArea(font: Font): Double =
      val bounds = font.createGlyphVector(frc, grin).getOutline.getBounds2D
      bounds.getWidth * bounds.getHeight
    inkArea(face(Font.BOLD)) should be > inkArea(face(Font.PLAIN))
  }
