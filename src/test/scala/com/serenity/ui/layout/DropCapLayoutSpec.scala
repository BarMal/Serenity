package com.serenity.ui.layout

import java.awt.Font

import com.serenity.richtext.ParagraphRole
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DropCapLayoutSpec extends AnyFlatSpec with Matchers:

  "DropCapLayout.spansLine" should "hold for every line within the drop cap's span" in {
    val role = ParagraphRole.DropCap(3)
    DropCapLayout.spansLine(role, 0) shouldBe true
    DropCapLayout.spansLine(role, 1) shouldBe true
    DropCapLayout.spansLine(role, 2) shouldBe true
    DropCapLayout.spansLine(role, 3) shouldBe false
  }

  it should "never hold for a non-drop-cap role" in {
    DropCapLayout.spansLine(ParagraphRole.Body, 0) shouldBe false
    DropCapLayout.spansLine(ParagraphRole.Heading(1), 0) shouldBe false
  }

  it should "never hold for a negative line index" in {
    DropCapLayout.spansLine(ParagraphRole.DropCap(3), -1) shouldBe false
  }

  "DropCapLayout.leftInsetPx" should "reserve the glyph's width on every spanned line" in {
    val role = ParagraphRole.DropCap(2)
    DropCapLayout.leftInsetPx(role, 0, glyphWidthPx = 40.0f) shouldBe 40.0f
    DropCapLayout.leftInsetPx(role, 1, glyphWidthPx = 40.0f) shouldBe 40.0f
  }

  it should "reserve no inset past the drop cap's span" in {
    DropCapLayout.leftInsetPx(ParagraphRole.DropCap(2), 2, glyphWidthPx = 40.0f) shouldBe 0.0f
  }

  it should "reserve no inset for a non-drop-cap role" in {
    DropCapLayout.leftInsetPx(ParagraphRole.Body, 0, glyphWidthPx = 40.0f) shouldBe 0.0f
  }

  "DropCapLayout.glyphWidthPx" should "measure a font's advance width for the glyph text" in {
    val font  = Font(Font.SERIF, Font.BOLD, 36)
    val frc   = TextLayoutSnapshot.defaultFontRenderContext()
    val width = DropCapLayout.glyphWidthPx(font, frc, "C")

    width should be > 0.0f
  }

  it should "measure zero width for empty glyph text" in {
    val font = Font(Font.SERIF, Font.BOLD, 36)
    val frc  = TextLayoutSnapshot.defaultFontRenderContext()

    DropCapLayout.glyphWidthPx(font, frc, "") shouldBe 0.0f
  }

  "DropCapLayout.glyphHeightPx" should "span the drop cap's line count at the paragraph's normal line height" in {
    DropCapLayout.glyphHeightPx(ParagraphRole.DropCap(3), normalLineHeightPx = 16) shouldBe 48
  }

  it should "be zero for a non-drop-cap role" in {
    DropCapLayout.glyphHeightPx(ParagraphRole.Body, normalLineHeightPx = 16) shouldBe 0
  }
