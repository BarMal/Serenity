package com.serenity.ui.theme

import com.serenity.richtext.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RichTextStylingSpec extends AnyFlatSpec with Matchers:

  "RichTextStyling.dropCapGlyphFontSize" should "scale the base font size by the drop cap's line span" in {
    RichTextStyling.dropCapGlyphFontSize(12.0f, 3) shouldBe 36.0f
    RichTextStyling.dropCapGlyphFontSize(12.0f, 1) shouldBe 12.0f
  }

  it should "clamp a non-positive line span to one line" in {
    RichTextStyling.dropCapGlyphFontSize(12.0f, 0) shouldBe 12.0f
    RichTextStyling.dropCapGlyphFontSize(12.0f, -5) shouldBe 12.0f
  }

  "RichTextStyling.dropCapGlyphStyle" should "return a bold glyph style sized to the drop cap's span, scaled by zoom" in {
    val style = RichTextStyling.dropCapGlyphStyle(
      RichTextStyle.empty,
      ParagraphRole.DropCap(3),
      baseFontSizePx = 12.0f,
      scale = 2.0f
    )

    style shouldBe Some(TextStyle(isBold = true, fontSize = Some(72.0f)))
  }

  it should "layer the first character's own inline family/marks over the glyph style" in {
    val inline = RichTextStyle(
      marks = Set(InlineMark.Italic, InlineMark.Underline),
      fontFamily = Some("Serif")
    )

    val style = RichTextStyling.dropCapGlyphStyle(inline, ParagraphRole.DropCap(2), baseFontSizePx = 12.0f)

    style shouldBe Some(
      TextStyle(isBold = true, isItalic = true, isUnderlined = true, fontFamily = Some("Serif"), fontSize = Some(24.0f))
    )
  }

  it should "return None for a non-drop-cap role" in {
    RichTextStyling.dropCapGlyphStyle(RichTextStyle.empty, ParagraphRole.Body, baseFontSizePx = 12.0f) shouldBe None
    RichTextStyling.dropCapGlyphStyle(RichTextStyle.empty, ParagraphRole.Heading(1), baseFontSizePx = 12.0f) shouldBe None
  }

  "RichTextStyling.dropCapSplitFontSpans" should "split the first character of a drop cap paragraph's home line into its own glyph span" in {
    val document = RichTextDocument(List(RichTextParagraph.plain("Chapter One", role = ParagraphRole.DropCap(3))))

    val (glyph, rest) = RichTextStyling.dropCapSplitFontSpans(document, 0, 0, 11, baseFontSizePx = 12.0f)

    glyph.map(_.text) shouldBe Some("C")
    glyph.map(_.style.fontSize) shouldBe Some(Some(36.0f))
    rest.map(_.text).mkString shouldBe "hapter One"
    rest.foreach(_.style.fontSize shouldBe None)
  }

  it should "not split a drop cap paragraph's continuation lines (non-zero start column)" in {
    val document = RichTextDocument(List(RichTextParagraph.plain("Chapter One", role = ParagraphRole.DropCap(3))))

    val (glyph, rest) = RichTextStyling.dropCapSplitFontSpans(document, 0, 4, 11, baseFontSizePx = 12.0f)

    glyph shouldBe None
    rest.map(_.text).mkString shouldBe "ter One"
  }

  it should "leave a non-drop-cap paragraph's spans unsplit" in {
    val document = RichTextDocument(List(RichTextParagraph.plain("Chapter One")))

    val (glyph, rest) = RichTextStyling.dropCapSplitFontSpans(document, 0, 0, 11, baseFontSizePx = 12.0f)

    glyph shouldBe None
    rest.map(_.text).mkString shouldBe "Chapter One"
  }

  "RichTextStyling.effectiveRole" should "degrade a drop cap to Body when the toggle is disabled" in {
    RichTextStyling.effectiveRole(ParagraphRole.DropCap(3), dropCapsEnabled = false) shouldBe ParagraphRole.Body
  }

  it should "keep the drop cap role when the toggle is enabled" in {
    RichTextStyling.effectiveRole(ParagraphRole.DropCap(3), dropCapsEnabled = true) shouldBe ParagraphRole.DropCap(3)
  }

  it should "leave non-drop-cap roles unchanged regardless of the toggle" in {
    RichTextStyling.effectiveRole(ParagraphRole.Body, dropCapsEnabled = false) shouldBe ParagraphRole.Body
    RichTextStyling.effectiveRole(ParagraphRole.Heading(2), dropCapsEnabled = false) shouldBe ParagraphRole.Heading(2)
  }

  it should "leave an empty line with no first character unsplit" in {
    val document = RichTextDocument(List(RichTextParagraph.plain("", role = ParagraphRole.DropCap(3))))

    val (glyph, rest) = RichTextStyling.dropCapSplitFontSpans(document, 0, 0, 0, baseFontSizePx = 12.0f)

    glyph shouldBe None
    rest shouldBe Nil
  }
