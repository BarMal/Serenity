package com.serenity.ui.theme

import com.serenity.state.models.AppState
import com.serenity.ui.color.RenderColor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DropCapColourSpec extends AnyFlatSpec with Matchers:

  private val textColour    = RenderColor.fromRgba(0xf0, 0xf0, 0xf0)
  private val headingColour = RenderColor.fromRgba(0x20, 0xa0, 0x60)
  private val accentColour  = RenderColor.fromRgba(0xd0, 0x80, 0x20)
  private val custom        = RenderColor.fromRgba(0x12, 0x34, 0x56)

  /** A theme in which the three colours a drop cap can follow are told apart. */
  private val theme: Theme =
    val base = AppState.empty.persisted.theme
    base.copy(
      foreground = textColour,
      highlighted = ThemeColor(accentColour, base.background),
      syntaxColors = base.syntaxColors.updated(SyntaxElement.Heading, ThemeColor(headingColour, base.background))
    )

  private def headingsWithH1(colour: HeadingColour): HeadingSettings =
    HeadingSettings(styles = HeadingStyle.defaults.updated(0, HeadingStyle.default(1).copy(colour = Some(colour))))

  "DropCapColour.Text" should "resolve to the theme's foreground" in {
    DropCapColour.Text.resolve(theme, HeadingSettings.default) shouldBe textColour
  }

  "DropCapColour.Accent" should "resolve to the highlighted foreground the cell fallback already uses" in {
    DropCapColour.Accent.resolve(theme, HeadingSettings.default) shouldBe accentColour
  }

  "DropCapColour.Fixed" should "resolve to its own colour whatever the theme" in {
    DropCapColour.Fixed(custom).resolve(theme, HeadingSettings.default) shouldBe custom
  }

  "DropCapColour.Heading" should "resolve to what headings.colour makes of a level 1 heading in rich text" in {
    val global = HeadingSettings(globalColour = Some(HeadingColour.Heading))

    DropCapColour.Heading.resolve(theme, global) shouldBe headingColour
    DropCapColour.Heading.resolve(theme, HeadingSettings.default) shouldBe textColour
  }

  it should "follow a colour set for the level 1 heading alone" in {
    DropCapColour.Heading.resolve(theme, headingsWithH1(HeadingColour.Fixed(custom))) shouldBe custom
    DropCapColour.Heading.resolve(theme, headingsWithH1(HeadingColour.Heading)) shouldBe
      headingColour
  }

  it should "ignore a colour set for another level" in {
    val onlyH2 = HeadingSettings(styles =
      HeadingStyle.defaults.updated(1, HeadingStyle.default(2).copy(colour = Some(HeadingColour.Fixed(custom))))
    )

    DropCapColour.Heading.resolve(theme, onlyH2) shouldBe textColour
  }
