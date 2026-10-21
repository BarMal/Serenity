package com.serenity

import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.presets.{UiPreset, UiPresetDiff}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Heading styles belong to the person's config, not to a workspace setup: a preset neither carries them in nor
  * overwrites them.
  */
class UiPresetHeadingsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val mine = HeadingsConfig().withLevel(1)(_.copy(size = Some(3.0f)))

  private def preset(config: AppConfig): UiPreset =
    UiPreset(
      name = "Custom",
      config = config.withLineNumbers(!config.surfaceConfig.showLineNumbers),
      themeName = Some("dark")
    )

  private def stateWith(config: AppConfig): AppState =
    val state = AppState.empty(config)
    state.copy(persisted = state.persisted.copy(theme = Theme.dark))

  "Applying a preset" should "keep the running heading styles whatever the preset holds" in {
    val running = AppConfig.default.withHeadingsConfig(mine)

    val fromPlain = UiPreset.applyToState(preset(AppConfig.default), stateWith(running), Some(Theme.dark))
    val fromOther = UiPreset.applyToState(
      preset(AppConfig.default.withHeadingsConfig(HeadingsConfig(damping = false))),
      stateWith(running),
      Some(Theme.dark)
    )

    fromPlain.persisted.config.headingsConfig shouldBe mine
    fromOther.persisted.config.headingsConfig shouldBe mine
  }

  it should "not take heading styles from a preset into a config that has none" in {
    val applied = UiPreset.applyToState(
      preset(AppConfig.default.withHeadingsConfig(mine)),
      stateWith(AppConfig.default),
      Some(Theme.dark)
    )

    applied.persisted.config.headingsConfig shouldBe HeadingsConfig()
  }

  "UiPresetDiff.changes" should "not offer to change a heading setting" in {
    val changes = UiPresetDiff.changes(
      AppConfig.default.withHeadingsConfig(mine),
      "dark",
      false,
      false,
      preset(AppConfig.default)
    )

    changes.map(_.key).filter(_.startsWith("headings.")) shouldBe Nil
  }
