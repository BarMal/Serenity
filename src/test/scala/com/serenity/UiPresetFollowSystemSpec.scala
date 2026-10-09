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
      themeName = "dark"
    )

  private def stateWith(config: AppConfig): AppState =
    val state = AppState.empty(config)
    state.copy(persisted = state.persisted.copy(theme = Theme.dark))

  "Applying a preset whose config follows nothing" should "keep follow_system and its themes" in {
    val applied = UiPreset.applyToState(preset(AppConfig.default), stateWith(running), Theme.dark)

    applied.persisted.config.themeFollowConfig shouldBe following
  }

  it should "still apply the rest of the preset" in {
    val chosen  = preset(AppConfig.default)
    val applied = UiPreset.applyToState(chosen, stateWith(running), Theme.dark)

    applied.persisted.config.surfaceConfig.showLineNumbers shouldBe chosen.config.surfaceConfig.showLineNumbers
  }

  "Applying a preset that follows the OS" should "not turn following on for someone who has it off" in {
    val followingPreset = preset(running)
    val applied         = UiPreset.applyToState(followingPreset, stateWith(AppConfig.default), Theme.dark)

    applied.persisted.config.themeFollowConfig shouldBe ThemeFollowConfig()
  }

  "UiPresetDiff.changes" should "not offer to change the follow_system settings" in {
    val changes = UiPresetDiff.changes(running, "dark", false, false, preset(AppConfig.default))

    changes.map(_.key).filter(_.startsWith("theme.")) shouldBe Nil
  }

  "UiPresetDiff.applySelected" should "keep the follow_system settings whatever is selected" in {
    val chosen   = preset(AppConfig.default)
    val allKeys  = UiPresetDiff.changes(running, "dark", false, false, chosen).map(_.key).toSet + "dockedPanels"
    val selected = UiPresetDiff.applySelected(stateWith(running), Theme.dark, chosen, allKeys)

    selected.persisted.config.themeFollowConfig shouldBe following
  }
