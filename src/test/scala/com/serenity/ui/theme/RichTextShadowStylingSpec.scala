package com.serenity.ui.theme

import java.awt.Font

import com.serenity.richtext.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Shadow mark reaches the text style the surfaces paint, and changes nothing about how the text is measured. */
class RichTextShadowStylingSpec extends AnyFlatSpec with Matchers:

  private def documentOf(runs: RichTextRun*): RichTextDocument =
    RichTextDocument(List(RichTextParagraph(runs.toList)))

  private val shadowed = RichTextStyle(marks = Set(InlineMark.Shadow))

  "RichTextStyling" should "style a run carrying the Shadow mark as shadowed, and the others as not" in {
    val document = documentOf(RichTextRun("ab", shadowed), RichTextRun("cd"))

    RichTextStyling.styledFontSpans(document, 0, 0, 4).map(span => span.text -> span.style.isShadowed) shouldBe
      List("ab" -> true, "cd" -> false)
  }

  it should "carry the shadow into the lines the draw path paints" in {
    val document = documentOf(RichTextRun("ab", shadowed))

    RichTextStyling.styledLine(document, 0, 0, 2, Theme.default).map(_.style.isShadowed) shouldBe List(true)
  }

  it should "give a drop cap glyph the shadow of its first character" in {
    val style = RichTextStyling.dropCapGlyphStyle(shadowed, ParagraphRole.DropCap(2), glyphFontSizePx = 24.0f)

    style.map(_.isShadowed) shouldBe Some(true)
    RichTextStyling
      .dropCapGlyphStyle(RichTextStyle.empty, ParagraphRole.DropCap(2), glyphFontSizePx = 24.0f)
      .map(_.isShadowed) shouldBe Some(false)
  }

  "TextStyle" should "be shadowed when either side of a combine is" in {
    TextStyle(isShadowed = true).combine(TextStyle.normal).isShadowed shouldBe true
    TextStyle.normal.combine(TextStyle(isShadowed = true)).isShadowed shouldBe true
    TextStyle.normal.combine(TextStyle.normal).isShadowed shouldBe false
  }

  it should "measure with the same font whether or not it is shadowed" in {
    val base = Font(Font.MONOSPACED, Font.PLAIN, 12)

    TextStyle.styledFont(base, TextStyle(isShadowed = true, isBold = true)) shouldBe
      TextStyle.styledFont(base, TextStyle(isBold = true))
  }
end RichTextShadowStylingSpec
