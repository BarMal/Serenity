package com.serenity

import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.presets.{UiPreset, UiPresetDiff}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A preset is about how the workspace looks, not about whether the theme follows the OS: applying one keeps
  * `theme.follow_system` and the themes chosen for the OS appearances as they are.
  */
class UiPresetFollowSystemSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val following =
    ThemeFollowConfig(followSystem = true, lightTheme = "paper", darkTheme = "ink", highContrastTheme = "contrast")

  private val running = AppConfig.default.withThemeFollowConfig(following)

  private def preset(config: AppConfig): UiPreset =
    UiPreset(
      name = "Custom",
      config = config.withLineNumbers(!config.surfaceConfig.showLineNumbers),
      themeName = Some("dark")
    )

  private def stateWith(config: AppConfig): AppState =
    val state = AppState.empty(config)
    state.copy(persisted = state.persisted.copy(theme = Theme.dark))

  "Applying a preset whose config follows nothing" should "keep follow_system and its themes" in {
    val applied = UiPreset.applyToState(preset(AppConfig.default), stateWith(running), Some(Theme.dark))

    applied.persisted.config.themeFollowConfig shouldBe following
  }

  it should "still apply the rest of the preset" in {
    val chosen  = preset(AppConfig.default)
    val applied = UiPreset.applyToState(chosen, stateWith(running), Some(Theme.dark))

    applied.persisted.config.surfaceConfig.showLineNumbers shouldBe chosen.config.surfaceConfig.showLineNumbers
  }

  "Applying a preset that follows the OS" should "not turn following on for someone who has it off" in {
    val followingPreset = preset(running)
    val applied         = UiPreset.applyToState(followingPreset, stateWith(AppConfig.default), Some(Theme.dark))

    applied.persisted.config.themeFollowConfig shouldBe ThemeFollowConfig()
  }

  "UiPresetDiff.changes" should "not offer to change the follow_system settings" in {
    val changes = UiPresetDiff.changes(running, "dark", false, false, preset(AppConfig.default))

    changes.map(_.key).filter(_.startsWith("theme.")) shouldBe Nil
  }

  "UiPresetDiff.applySelected" should "keep the follow_system settings whatever is selected" in {
    val chosen   = preset(AppConfig.default)
    val allKeys  = UiPresetDiff.changes(running, "dark", false, false, chosen).map(_.key).toSet + "dockedPanels"
    val selected = UiPresetDiff.applySelected(stateWith(running), Some(Theme.dark), chosen, allKeys)

    selected.persisted.config.themeFollowConfig shouldBe following
  }

  /** While following, the OS picks the theme: a preset's own theme neither replaces it nor rewrites the slot that
    * produced it -- the slots are the person's mapping, kept like the follow flag itself.
    */
  private def followingOn(current: Theme): AppState =
    val state = stateWith(running)
    state.copy(persisted = state.persisted.copy(theme = current))

  "Applying a preset that names a theme while following the OS" should "keep the OS-chosen theme" in {
    val applied = UiPreset.applyToState(preset(AppConfig.default), followingOn(Theme.light), Some(Theme.dark))

    applied.persisted.theme shouldBe Theme.light
  }

  it should "leave every light, dark and high-contrast slot as it was" in {
    val applied = UiPreset.applyToState(preset(AppConfig.default), followingOn(Theme.light), Some(Theme.dark))

    applied.persisted.config.themeFollowConfig shouldBe following
  }

  "Applying a preset that names a theme while not following the OS" should "apply the preset's theme" in {
    val notFollowing = stateWith(AppConfig.default)
    val applied      = UiPreset.applyToState(preset(AppConfig.default), notFollowing, Some(Theme.light))

    applied.persisted.theme shouldBe Theme.light
  }

  "UiPresetDiff.changes" should "not offer a theme change while following the OS" in {
    val changes = UiPresetDiff.changes(running, Theme.light.name, false, false, preset(AppConfig.default))

    changes.map(_.key) should not contain "theme"
  }

  it should "offer the theme change when not following the OS" in {
    val changes = UiPresetDiff.changes(AppConfig.default, Theme.light.name, false, false, preset(AppConfig.default))

    changes.map(_.key) should contain("theme")
  }

  "UiPresetDiff.applySelected with the theme selected while following the OS" should "keep the OS-chosen theme" in {
    val selected =
      UiPresetDiff.applySelected(followingOn(Theme.light), Some(Theme.dark), preset(AppConfig.default), Set("theme"))

    selected.persisted.theme shouldBe Theme.light
  }
