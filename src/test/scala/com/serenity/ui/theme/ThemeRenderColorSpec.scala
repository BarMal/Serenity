package com.serenity.ui.theme

import com.serenity.ui.color.RenderColor
import com.serenity.ui.theme.config.{ColorParser, ConfigurableThemeManager, ThemeConfig, ThemeConfigWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The theme holds the neutral [[RenderColor]] (#1812 step 3), so render code paints theme colours without converting
  * from `java.awt.Color`, while theme files parse and write exactly the hex they did before.
  */
class ThemeRenderColorSpec extends AnyFlatSpec with Matchers:

  private val themes = List(DefaultThemes.defaultDark, DefaultThemes.defaultLight)

  "Theme" should "hand out RenderColor for its scalar, token, syntax and interaction colours" in {
    val theme                   = Theme.dark
    val foreground: RenderColor = theme.foreground
    val keyword: RenderColor    = theme.colorFor(SyntaxElement.Keyword).foreground
    val hover: RenderColor      = theme.interactionStates.hover.background

    foreground shouldBe theme.foregroundColor
    keyword shouldBe theme.syntaxColors(SyntaxElement.Keyword).foreground
    hover shouldBe theme.menuItem.background.blendToward(theme.menuItem.foreground, 0.12)
    StyledText("x").foregroundColor shouldBe RenderColor.White
    StyledText("x").backgroundColor shouldBe RenderColor.Black
  }

  it should "parse the bundled theme hex into the exact ARGB values" in {
    val config = ThemeConfig.defaultDark
    Theme.dark.foreground shouldBe ColorParser.parseColor(config.ui.foreground).toOption.get
    ColorParser.parseColor("#1a2B3c") shouldBe Right(RenderColor.fromArgb(0xff1a2b3c))
    ColorParser.parseColor("#abc") shouldBe Right(RenderColor.fromArgb(0xffaabbcc))
    ColorParser.parseColor("#11223344") shouldBe Right(RenderColor.fromArgb(0x44112233))
    ColorParser.parseColor("rgb(1, 2, 3)") shouldBe Right(RenderColor.fromArgb(0xff010203))
    ColorParser.parseColor("#-1-1-1") shouldBe a[Left[?, ?]]
  }

  it should "round-trip every bundled theme through its written hex config unchanged" in
    themes.foreach { theme =>
      ConfigurableThemeManager.configToTheme(ThemeConfigWriter.themeToConfig(theme)) shouldBe Right(theme)
    }

  it should "write theme colours as uppercase #RRGGBB, dropping alpha, as before" in {
    val theme  = Theme.dark.copy(foreground = RenderColor.fromArgb(0x80a1b2c3))
    val config = ThemeConfigWriter.themeToConfig(theme)
    config.ui.foreground shouldBe "#A1B2C3"
    ColorFormat.toHex(RenderColor.fromArgb(0x80a1b2c3), withAlpha = true) shouldBe "#A1B2C380"
    ColorFormat.toHex(RenderColor.fromArgb(0xffa1b2c3), withAlpha = true) shouldBe "#A1B2C3"
  }
