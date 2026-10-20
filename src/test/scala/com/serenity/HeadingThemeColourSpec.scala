package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.lsp.config.LanguageId
import com.serenity.ui.color.RenderColor
import com.serenity.ui.theme.config.*
import com.serenity.ui.theme.{SyntaxElement, TextStyle, Theme, ThemeColor, ThemeHighlightCache}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A theme colours headings and their `#` markers; one that does not say how gets its keyword and delimiter colours,
  * which is what Markdown source highlighting has always used.
  */
class HeadingThemeColourSpec extends AnyFlatSpec with Matchers:

  private val teal   = RenderColor.fromRgba(0x11, 0xaa, 0x99)
  private val orange = RenderColor.fromRgba(0xee, 0x88, 0x22)

  private def colour(foreground: RenderColor, theme: Theme = Theme.dark) =
    ThemeColor(foreground, theme.background, TextStyle.normal)

  private val themed = Theme.dark.copy(syntaxColors =
    Theme.dark.syntaxColors
      .updated(SyntaxElement.Heading, colour(teal))
      .updated(SyntaxElement.HeadingMarker, colour(orange))
  )

  private def highlighted(theme: Theme) =
    ThemeHighlightCache().highlightLine("## Title", theme, Some(LanguageId.Markdown), None)

  "A theme without heading colours" should "colour headings as keywords and their markers as delimiters" in
    List(Theme.dark, Theme.light).foreach { theme =>
      theme.colorFor(SyntaxElement.Heading) shouldBe theme.colorFor(SyntaxElement.Keyword)
      theme.colorFor(SyntaxElement.HeadingMarker) shouldBe theme.colorFor(SyntaxElement.Delimiter)
    }

  "A theme with heading colours" should "use them" in {
    themed.colorFor(SyntaxElement.Heading).foreground shouldBe teal
    themed.colorFor(SyntaxElement.HeadingMarker).foreground shouldBe orange
    themed.colorFor(SyntaxElement.Keyword) shouldBe Theme.dark.colorFor(SyntaxElement.Keyword)
  }

  "Markdown source highlighting" should "look as it always has under a theme without heading colours" in {
    val theme = Theme.dark
    highlighted(theme) shouldBe List(
      com.serenity.ui.theme.StyledText(
        "## ",
        TextStyle.bold,
        theme.colorFor(SyntaxElement.Delimiter).foreground,
        theme.background
      ),
      com.serenity.ui.theme.StyledText(
        "Title",
        TextStyle.bold,
        theme.colorFor(SyntaxElement.Keyword).foreground,
        theme.background
      )
    )
  }

  it should "colour a heading's marker and text from the theme's heading colours" in {
    highlighted(themed).map(piece => (piece.content, piece.foregroundColor)) shouldBe
      List("## " -> orange, "Title" -> teal)
  }

  private def themeFile(headingBlock: String): String =
    val base = ThemeConfigWriter.render(ThemeConfig.defaultDark)
    base.replace("  syntax {\n", s"  syntax {\n$headingBlock")

  private def block(key: String, foreground: String): String =
    s"""    $key { foreground = "$foreground", style { bold = false, italic = false, underline = false } }\n"""

  private def load(text: String): Theme =
    val path = TestTemp.file("heading-theme", ".conf")
    try
      Files.writeString(path, text)
      ConfigurableThemeManager(ThemeConfigLoader()).loadThemeFromFile(path).unsafeRunSync()
    finally Files.deleteIfExists(path)

  "A theme file" should "fall back to keyword and delimiter when it has no heading keys" in {
    val theme = load(themeFile(""))

    theme.colorFor(SyntaxElement.Heading) shouldBe theme.colorFor(SyntaxElement.Keyword)
    theme.colorFor(SyntaxElement.HeadingMarker) shouldBe theme.colorFor(SyntaxElement.Delimiter)
  }

  it should "read heading and heading-marker" in {
    val theme = load(themeFile(block("heading", "#11AA99") + block("heading-marker", "#EE8822")))

    theme.colorFor(SyntaxElement.Heading).foreground shouldBe teal
    theme.colorFor(SyntaxElement.HeadingMarker).foreground shouldBe orange
  }

  it should "show a reloaded file's new heading colour" in {
    val path = TestTemp.file("heading-reload", ".conf")
    try
      val manager = ConfigurableThemeManager(ThemeConfigLoader())
      Files.writeString(path, themeFile(block("heading", "#11AA99")))
      manager.loadThemeFromFile(path).unsafeRunSync().colorFor(SyntaxElement.Heading).foreground shouldBe teal
      Files.writeString(path, themeFile(block("heading", "#EE8822")))
      manager.loadThemeFromFile(path).unsafeRunSync().colorFor(SyntaxElement.Heading).foreground shouldBe orange
    finally Files.deleteIfExists(path)
  }

  "The theme writer" should "keep heading colours a theme sets, and invent none for a theme that sets none" in {
    ThemeConfigWriter.themeToConfig(Theme.dark).syntax.heading shouldBe None
    ThemeConfigWriter.themeToConfig(Theme.dark).syntax.headingMarker shouldBe None

    val written = ThemeConfigWriter.themeToConfig(themed).syntax
    written.heading.map(_.foreground.toUpperCase) shouldBe Some("#11AA99")
    written.headingMarker.map(_.foreground.toUpperCase) shouldBe Some("#EE8822")

    val reread = load(ThemeConfigWriter.render(ThemeConfigWriter.themeToConfig(themed)))
    reread.colorFor(SyntaxElement.Heading).foreground shouldBe teal
    reread.colorFor(SyntaxElement.HeadingMarker).foreground shouldBe orange
  }
