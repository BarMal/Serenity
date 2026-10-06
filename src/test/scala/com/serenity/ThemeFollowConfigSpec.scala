package com.serenity

import com.serenity.config.{AppConfig, ConfigFileFormat, ConfigRegistry, ThemeFollowConfig}
import com.serenity.ui.theme.appearance.OsAppearance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ThemeFollowConfigSpec extends AnyFlatSpec with Matchers:

  private val following = ThemeFollowConfig(
    followSystem = true,
    lightTheme = "paper",
    darkTheme = "ink",
    highContrastTheme = "contrast"
  )

  "following the system" should "be off by default, so existing users see no change" in {
    AppConfig.default.themeFollowConfig.followSystem shouldBe false
  }

  "theme selection" should "pick the configured theme for each known appearance" in {
    following.themeFor(OsAppearance.Light) shouldBe Some("paper")
    following.themeFor(OsAppearance.Dark) shouldBe Some("ink")
    following.themeFor(OsAppearance.HighContrast) shouldBe Some("contrast")
  }

  it should "pick nothing for an Unknown appearance, which keeps the current theme" in {
    following.themeFor(OsAppearance.Unknown) shouldBe None
  }

  it should "pick nothing at all while follow_system is off" in {
    val off = following.copy(followSystem = false)

    List(OsAppearance.Light, OsAppearance.Dark, OsAppearance.HighContrast).map(off.themeFor) shouldBe
      List(None, None, None)
  }

  "the config file" should "read the follow settings from their theme.* keys" in {
    val read = List(
      "theme.follow_system" -> "true",
      "theme.light"         -> "paper",
      "theme.dark"          -> "ink",
      "theme.high_contrast" -> "contrast"
    ).foldLeft(Option(AppConfig.default))((config, setting) =>
      config.flatMap(ConfigRegistry.read(_, setting._1, setting._2))
    )

    read.map(_.themeFollowConfig) shouldBe Some(following)
  }

  it should "write the follow settings back out so a save does not drop them" in {
    val written = ConfigFileFormat.settings(AppConfig.default.withThemeFollowConfig(following)).toMap

    written.view.mapValues(_.rendered).toMap.filter(_._1.startsWith("theme.")) shouldBe Map(
      "theme.follow_system" -> "true",
      "theme.light"         -> "paper",
      "theme.dark"          -> "ink",
      "theme.high_contrast" -> "contrast"
    )
  }

  it should "reject a blank theme name" in {
    ConfigRegistry.read(AppConfig.default, "theme.dark", "  ") shouldBe None
  }
