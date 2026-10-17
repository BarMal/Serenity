package com.serenity.ui.theme

import java.awt.Font

import com.serenity.config.HeadingsConfig
import com.serenity.markdown.MarkdownBlockSpans
import com.serenity.markdown.MarkdownInlineSpans.Style
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.ui.color.RenderColor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A Markdown heading in the live preview is styled from the same [[HeadingSettings]] a rich-text heading is. */
class MarkdownHeadingStylingSpec extends AnyFlatSpec with Matchers:

  private val base      = 14.0f
  private val sharedPts = Map(1 -> 22.0f, 2 -> 18.0f, 3 -> 16.0f, 4 -> 14.0f, 5 -> 14.0f, 6 -> 14.0f)

  private def headingRun(level: Int): Style = Style(bold = false, italic = false, code = false, headingLevel = level)

  private def previewStyle(level: Int, headings: HeadingSettings = HeadingSettings.default): TextStyle =
    MarkdownInlineStyling.textStyle(headingRun(level), baseIsMonospaced = false, baseFontSize = base, headings)

  private def richStyle(level: Int, headings: HeadingSettings = HeadingSettings.default): TextStyle =
    val document = RichTextDocument(List(RichTextParagraph.plain("Title", role = ParagraphRole.Heading(level))))
    RichTextStyling
      .styledFontSpans(document, 0, 0, 5, base / RichTextStyling.ProseZoomBaselinePx, headings)
      .headOption
      .map(_.style)
      .getOrElse(fail("expected a heading span"))

  private def sizeOf(style: TextStyle): Float = style.fontSize.getOrElse(fail("expected a font size"))

  "A preview heading" should "be the rich-text multiple of the base size, H1 to H6" in
    sharedPts.foreach {
      case (level, pts) =>
        withClue(s"H$level")(sizeOf(previewStyle(level)) shouldBe (base * pts / 12.0f +- 0.001f))
    }

  it should "be styled exactly as a rich-text heading of the same level" in
    (1 to 6).foreach { level =>
      val preview = previewStyle(level)
      val rich    = richStyle(level)
      withClue(s"H$level") {
        sizeOf(preview) shouldBe (sizeOf(rich) +- 0.001f)
        preview.copy(fontSize = None) shouldBe rich.copy(fontSize = None)
      }
    }

  it should "follow a configured size, weight, slant and family" in {
    val configured = HeadingsConfig()
      .withLevel(2)(_.copy(size = Some(3.0f), bold = Some(false), italic = Some(true), family = Some("Charter")))
      .settings(12.0f)
    val style = previewStyle(2, configured)

    sizeOf(style) shouldBe (3.0f * base +- 0.001f)
    style.isBold shouldBe false
    style.isItalic shouldBe true
    style.fontFamily shouldBe Some("Charter")
    previewStyle(1, configured) shouldBe previewStyle(1)
  }

  it should "be damped above the threshold as a rich-text heading is" in {
    val damped = HeadingsConfig().settings(48.0f)
    val h1     = HeadingScale.effective(22.0f / 12.0f, 48.0f, 24.0f, dampingOn = true)

    sizeOf(previewStyle(1, damped)) shouldBe (h1 * base +- 0.001f)
    sizeOf(previewStyle(1, damped)) should be < sizeOf(previewStyle(1))
  }

  it should "keep a code span inside it monospaced and as large as the heading" in {
    val code = headingRun(1).copy(code = true)
    val style =
      MarkdownInlineStyling.textStyle(code, baseIsMonospaced = false, baseFontSize = base, HeadingSettings.default)

    style.fontFamily shouldBe Some(Font.MONOSPACED)
    sizeOf(style) shouldBe (sizeOf(previewStyle(1)) +- 0.001f)
  }

  "Text that is not a heading" should "keep the base size" in {
    MarkdownInlineStyling.textStyle(
      Style(bold = false, italic = false, code = false),
      baseIsMonospaced = false,
      baseFontSize = base,
      HeadingSettings.default
    ) shouldBe TextStyle.normal
  }

  "The Markdown scanner" should "mark a heading's content with its level, ATX and setext alike" in {
    MarkdownBlockSpans.scan("### Third").map(_.style.headingLevel) shouldBe Vector(0, 3)
    MarkdownBlockSpans.scan("###### Sixth").map(_.style.headingLevel) shouldBe Vector(0, 6)
    MarkdownBlockSpans
      .scan("Title", setext = com.serenity.markdown.MarkdownBlockLens.SetextRole.Text(2))
      .map(_.style.headingLevel) shouldBe Vector(2)
  }

  private val theme      = Theme.dark
  private val headingRed = RenderColor.fromRgba(0xd0, 0x30, 0x30)

  private def restyled(headings: HeadingSettings, bold: Boolean = true) =
    val segments = List(
      StyledText(
        "Title",
        if bold then TextStyle.bold else TextStyle.normal,
        theme.colorFor(SyntaxElement.Heading).foreground,
        theme.background
      )
    )
    MarkdownInlineStyling
      .restyle(
        segments,
        MarkdownBlockSpans.scan("Title", setext = com.serenity.markdown.MarkdownBlockLens.SetextRole.Text(1)),
        startColumn = 0,
        hideMarkers = true,
        theme,
        baseIsMonospaced = false,
        baseFontSize = base,
        headings
      )
      .headOption
      .getOrElse(fail("expected a painted piece"))

  "A painted preview heading" should "use the style it was measured with, not the highlighter's bold" in {
    val unbold = HeadingsConfig().withLevel(1)(_.copy(bold = Some(false))).settings(12.0f)

    restyled(unbold).style.isBold shouldBe false
    restyled(HeadingSettings.default).style.isBold shouldBe true
    sizeOf(restyled(HeadingSettings.default).style) shouldBe (sizeOf(previewStyle(1)) +- 0.001f)
  }

  it should "take the theme's heading colour, or the level's own when configured" in {
    val configured = HeadingsConfig().withLevel(1)(_.copy(colour = Some(headingRed))).settings(12.0f)

    restyled(HeadingSettings.default).foregroundColor shouldBe theme.colorFor(SyntaxElement.Heading).foreground
    restyled(configured).foregroundColor shouldBe headingRed
  }
