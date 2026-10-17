package com.serenity.ui.theme

import com.serenity.config.HeadingsConfig
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun, RichTextStyle}
import com.serenity.ui.color.RenderColor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What colour a rich-text heading is painted in: the text colour unless the global key, its level or the run says
  * otherwise.
  */
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

  private def globally(choice: HeadingColour) = HeadingsConfig(colour = Some(choice)).settings(12.0f)

  "A rich-text heading by default" should "be painted in the text colour, as it was before headings had a colour" in {
    (1 to 6).foreach(level => painted(document(ParagraphRole.Heading(level))) shouldBe List(theme.foreground))
    painted(document(ParagraphRole.Heading(1)), HeadingsConfig().settings(12.0f)) shouldBe List(theme.foreground)
    Theme.dark.colorFor(SyntaxElement.Keyword).foreground should not be Theme.dark.foreground
  }

  "The global heading colour" should "make every heading the text colour or the theme's heading colour" in {
    val heading = globally(HeadingColour.Heading)
    val text    = globally(HeadingColour.Text)

    (1 to 6).foreach { level =>
      painted(document(ParagraphRole.Heading(level)), heading) shouldBe List(teal)
      painted(document(ParagraphRole.Heading(level)), text) shouldBe List(theme.foreground)
    }
  }

  "A level's own colour" should "win over the global one, and auto should follow it" in {
    val configured = HeadingsConfig(colour = Some(HeadingColour.Heading))
      .withLevel(2)(_.copy(colour = Some(HeadingColour.Text)))
      .withLevel(3)(_.copy(colour = Some(HeadingColour.Fixed(orange))))
      .settings(12.0f)

    painted(document(ParagraphRole.Heading(1)), configured) shouldBe List(teal)
    painted(document(ParagraphRole.Heading(2)), configured) shouldBe List(theme.foreground)
    painted(document(ParagraphRole.Heading(3)), configured) shouldBe List(orange)
  }

  it should "say heading or text whatever the global key says" in {
    val textGlobal = HeadingsConfig(colour = Some(HeadingColour.Text))
      .withLevel(1)(_.copy(colour = Some(HeadingColour.Heading)))
      .settings(12.0f)
    val headingGlobal = HeadingsConfig(colour = Some(HeadingColour.Heading))
      .withLevel(1)(_.copy(colour = Some(HeadingColour.Text)))
      .settings(12.0f)

    painted(document(ParagraphRole.Heading(1)), textGlobal) shouldBe List(teal)
    painted(document(ParagraphRole.Heading(1)), headingGlobal) shouldBe List(theme.foreground)
  }

  "A run's own colour" should "win over every heading colour" in {
    val configured = HeadingsConfig(colour = Some(HeadingColour.Heading))
      .withLevel(1)(_.copy(colour = Some(HeadingColour.Fixed(orange))))
      .settings(12.0f)
    val explicit = document(ParagraphRole.Heading(1), RichTextStyle(color = Some("#336699")))

    painted(explicit) shouldBe List(RenderColor.fromRgba(0x33, 0x66, 0x99))
    painted(explicit, configured) shouldBe List(RenderColor.fromRgba(0x33, 0x66, 0x99))
  }

  "Body text and drop caps" should "stay in the text colour whatever the heading colour is" in {
    val heading = globally(HeadingColour.Heading)

    painted(document(ParagraphRole.Body), heading) shouldBe List(theme.foreground)
    painted(document(ParagraphRole.dropCap(lines = 2)), heading) shouldBe List(theme.foreground)
  }

  "A Markdown heading" should "default to the theme's heading colour, and follow the same keys" in {
    val settings = HeadingSettings.default
    settings.colourFor(1, markdown = true) shouldBe HeadingColour.Heading
    settings.colourFor(1, markdown = false) shouldBe HeadingColour.Text
    globally(HeadingColour.Text).colourFor(1, markdown = true) shouldBe HeadingColour.Text
    globally(HeadingColour.Heading).colourFor(1, markdown = false) shouldBe HeadingColour.Heading
    HeadingsConfig()
      .withLevel(4)(_.copy(colour = Some(HeadingColour.Fixed(orange))))
      .settings(12.0f)
      .colourFor(4, markdown = true) shouldBe HeadingColour.Fixed(orange)
  }

  "A heading colour" should "resolve against the theme" in {
    HeadingColour.Text.resolve(theme) shouldBe theme.foreground
    HeadingColour.Heading.resolve(theme) shouldBe teal
    HeadingColour.Fixed(orange).resolve(theme) shouldBe orange
  }
