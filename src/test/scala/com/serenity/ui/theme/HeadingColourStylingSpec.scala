package com.serenity.ui.theme

import com.serenity.config.HeadingsConfig
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun, RichTextStyle}
import com.serenity.ui.color.RenderColor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A rich-text heading is painted in the theme's heading colour unless its level or the run itself says otherwise. */
class HeadingColourStylingSpec extends AnyFlatSpec with Matchers:

  private val teal   = RenderColor.fromRgba(0x11, 0xaa, 0x99)
  private val orange = RenderColor.fromRgba(0xee, 0x88, 0x22)

  private val theme = Theme.dark.copy(syntaxColors =
    Theme.dark.syntaxColors.updated(
      SyntaxElement.Heading,
      ThemeColor(teal, Theme.dark.background, TextStyle.normal)
    )
  )

  private def document(role: ParagraphRole, style: RichTextStyle = RichTextStyle.empty) =
    RichTextDocument(List(RichTextParagraph(List(RichTextRun("Title", style)), role = role)))

  private def painted(
    document: RichTextDocument,
    headings: HeadingSettings = HeadingSettings.default
  ): List[RenderColor] =
    RichTextStyling.styledLine(document, 0, 0, 5, theme, headings = headings).map(_.foregroundColor)

  "A rich-text heading" should "be painted in the theme's heading colour, at every level" in
    (1 to 6).foreach(level => painted(document(ParagraphRole.Heading(level))) shouldBe List(teal))

  it should "be painted in its level's own colour once that is configured" in {
    val configured = HeadingsConfig().withLevel(2)(_.copy(colour = Some(orange))).settings(12.0f)

    painted(document(ParagraphRole.Heading(2)), configured) shouldBe List(orange)
    painted(document(ParagraphRole.Heading(1)), configured) shouldBe List(teal)
  }

  it should "keep a run's own colour over both" in {
    val configured = HeadingsConfig().withLevel(1)(_.copy(colour = Some(orange))).settings(12.0f)
    val explicit   = document(ParagraphRole.Heading(1), RichTextStyle(color = Some("#336699")))

    painted(explicit) shouldBe List(RenderColor.fromRgba(0x33, 0x66, 0x99))
    painted(explicit, configured) shouldBe List(RenderColor.fromRgba(0x33, 0x66, 0x99))
  }

  "Body text and drop caps" should "stay in the theme's foreground" in {
    painted(document(ParagraphRole.Body)) shouldBe List(theme.foreground)
    painted(document(ParagraphRole.dropCap(lines = 2))) shouldBe List(theme.foreground)
  }
