package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.state.reducers.SettingsPreviewReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `theme.follow_system` and the themes for the OS appearances have a place in the settings palette, under Look. */
class CommandRunnerFollowSystemSettingsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry   = CommandRegistry.default
  given CommandRegistry  = registry
  private val customised = ThemeFollowConfig(followSystem = true, lightTheme = "paper", darkTheme = "ink")
  private def runnerFor(config: AppConfig) = CommandRunner.empty.activate(registry, config).openSettings

  private def followGroup(config: AppConfig): CommandSurfaceItem.GroupItem =
    val look = runnerFor(config).settingsGroups
      .find(_.id == "settings-look")
      .getOrElse(fail("missing the Look group"))
    look.children
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == "settings-follow-system" => group }
      .getOrElse(fail("missing the OS appearance group under Look"))

  private def toggle(config: AppConfig): CommandSurfaceItem.OptionItem =
    followGroup(config).children
      .collectFirst { case item: CommandSurfaceItem.OptionItem if item.id == "follow-system-theme" => item }
      .getOrElse(fail("missing the follow toggle"))

  private def slot(config: AppConfig, id: String): CommandSurfaceItem.InputItem =
    followGroup(config).children
      .collectFirst { case item: CommandSurfaceItem.InputItem if item.id == id => item }
      .getOrElse(fail(s"missing the $id row"))

  "The Look group" should "hold an OS appearance group with the toggle and a theme for each appearance" in {
    followGroup(AppConfig.default).label shouldBe "OS Appearance"
    followGroup(AppConfig.default).children.map(_.id) shouldBe List(
      "follow-system-theme",
      "follow-system-light",
      "follow-system-dark",
      "follow-system-high-contrast"
    )
  }

  "The follow toggle" should "read Off by default and On when the config follows the OS" in {
    toggle(AppConfig.default).selectedOption shouldBe "Off"
    toggle(AppConfig.default.withThemeFollowConfig(customised)).selectedOption shouldBe "On"
  }

  it should "turn following on and off" in {
    toggle(AppConfig.default).options.map(_.intent) shouldBe List(
      CommandIntent.Theme(ThemeIntent.SetFollowSystem(true)),
      CommandIntent.Theme(ThemeIntent.SetFollowSystem(false))
    )
  }

  it should "say what it does in its hint" in {
    toggle(AppConfig.default).hint.getOrElse("") should include("OS")
  }

  "The theme rows" should "show the theme each OS appearance maps to" in {
    val config = AppConfig.default.withThemeFollowConfig(customised)

    slot(config, "follow-system-light").currentValue shouldBe "paper"
    slot(config, "follow-system-dark").currentValue shouldBe "ink"
    slot(config, "follow-system-high-contrast").currentValue shouldBe "high-contrast"
  }

  it should "set the theme for their appearance from what is typed" in {
    val config = AppConfig.default

    slot(config, "follow-system-light").parse(" paper ") shouldBe
      Some(CommandIntent.Theme(ThemeIntent.SetFollowSystemTheme(AppearanceSlot.Light, "paper")))
    slot(config, "follow-system-dark").parse("ink") shouldBe
      Some(CommandIntent.Theme(ThemeIntent.SetFollowSystemTheme(AppearanceSlot.Dark, "ink")))
    slot(config, "follow-system-high-contrast").parse("contrast") shouldBe
      Some(CommandIntent.Theme(ThemeIntent.SetFollowSystemTheme(AppearanceSlot.HighContrast, "contrast")))
  }

  it should "refuse an empty theme name" in {
    slot(AppConfig.default, "follow-system-dark").parse("   ") shouldBe None
  }

  "Searching the settings for the OS" should "find the OS appearance group" in {
    val runner = runnerFor(AppConfig.default).updateSearchTerm("appearance")

    runner.visibleItems.map(_.id) should contain("settings-follow-system")
  }

  "Settings preview" should "commit the follow toggle and the theme rows as soon as they are chosen" in {
    SettingsPreviewReducer.isPreviewable(CommandIntent.Theme(ThemeIntent.SetFollowSystem(true))) shouldBe false
    SettingsPreviewReducer.isPreviewable(CommandIntent.Theme(ThemeIntent.SetFollowSystem(false))) shouldBe false
    SettingsPreviewReducer.isPreviewable(
      CommandIntent.Theme(ThemeIntent.SetFollowSystemTheme(AppearanceSlot.Dark, "ink"))
    ) shouldBe false
  }
