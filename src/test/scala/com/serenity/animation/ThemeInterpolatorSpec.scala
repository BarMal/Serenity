package com.serenity.animation

import com.googlecode.lanterna.TextColor
import com.serenity.ui.theme.{Theme, ThemeColor, TextStyle, SyntaxElement}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ThemeInterpolatorSpec extends AnyFlatSpec with Matchers:

  private val black = new TextColor.RGB(0, 0, 0)
  private val white = new TextColor.RGB(255, 255, 255)
  private val red   = new TextColor.RGB(255, 0, 0)
  private val blue  = new TextColor.RGB(0, 0, 255)

  private def makeTheme(fg: TextColor, bg: TextColor): Theme =
    Theme(
      name        = "test",
      foreground  = fg,
      background  = bg,
      cursor      = fg,
      highlighted = ThemeColor(fg, bg),
      menuItem    = ThemeColor(fg, bg),
      panel       = ThemeColor(fg, bg),
      error       = ThemeColor(fg, bg),
      border      = fg,
      muted       = bg,
      placeholder = bg,
      textStyle   = TextStyle.normal,
      syntaxColors = Map(SyntaxElement.Keyword -> ThemeColor(fg, bg))
    )

  private val fromTheme = makeTheme(black, white)
  private val toTheme   = makeTheme(white, black)

  "ThemeInterpolator.blend" should "return from-theme colors at t=0" in {
    val result = ThemeInterpolator.blend(fromTheme, toTheme, 0.0)
    result.foreground shouldEqual black
    result.background shouldEqual white
    result.panel.foreground shouldEqual black
    result.panel.background shouldEqual white
  }

  it should "return to-theme colors at t=1" in {
    val result = ThemeInterpolator.blend(fromTheme, toTheme, 1.0)
    result.foreground shouldEqual white
    result.background shouldEqual black
    result.panel.foreground shouldEqual white
    result.panel.background shouldEqual black
  }

  it should "return midpoint colors at t=0.5" in {
    val result = ThemeInterpolator.blend(fromTheme, toTheme, 0.5)
    val mid    = new TextColor.RGB(127, 127, 127)
    RgbInterpolator.toRgb(result.foreground).getRed  shouldEqual 127 +- 2
    RgbInterpolator.toRgb(result.background).getRed  shouldEqual 127 +- 2
    RgbInterpolator.toRgb(result.panel.foreground).getRed shouldEqual 127 +- 2
  }

  it should "interpolate syntax colors" in {
    val result = ThemeInterpolator.blend(fromTheme, toTheme, 0.0)
    result.syntaxColors(SyntaxElement.Keyword).foreground shouldEqual black

    val result2 = ThemeInterpolator.blend(fromTheme, toTheme, 1.0)
    result2.syntaxColors(SyntaxElement.Keyword).foreground shouldEqual white
  }

  it should "keep the to-theme name and textStyle" in {
    val result = ThemeInterpolator.blend(fromTheme, toTheme, 0.5)
    result.name shouldEqual toTheme.name
    result.textStyle shouldEqual toTheme.textStyle
  }
