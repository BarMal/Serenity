package com.serenity.command

import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Auto-save is reachable from the settings surface (#1992): a mode picker and a delay input. */
class AutoSaveSettingsSpec extends AnyFlatSpec with Matchers:

  private def modeIntent(mode: AutoSaveMode): CommandIntent =
    CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAutoSaveMode(mode)))

  private def delayInput(config: AppConfig): CommandSurfaceItem.InputItem =
    CommandRunnerSettingsInputItems
      .build(config)
      .find(_.id == "auto-save-delay")
      .getOrElse(fail("no auto-save-delay input"))

  "the auto-save option" should "offer every mode, with the current one selected" in {
    val selections =
      CommandRunnerOptionSelections.default(AppConfig.default.withAutoSaveMode(AutoSaveMode.OnFocusChange))
    val item = CommandRunnerSettingsItems.autoSaveModeOptionItem(selections)

    item.options.map(_.intent) shouldBe AutoSaveMode.values.toList.map(modeIntent)
    item.options(item.selectedIndex).intent shouldBe modeIntent(AutoSaveMode.OnFocusChange)
  }

  it should "start on Off for the default config" in {
    val selections = CommandRunnerOptionSelections.default(AppConfig.default)

    selections("auto-save-mode") shouldBe 0
  }

  "the auto-save delay input" should "show the configured delay and its default" in {
    val item = delayInput(AppConfig.default.withAutoSaveDelayMillis(2500L))

    item.currentValue shouldBe "2500"
    item.defaultValue shouldBe Some("1000")
  }

  it should "turn a delay of at least the minimum into a setting change" in {
    val item = delayInput(AppConfig.default)

    item.parse("750") shouldBe Some(
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAutoSaveDelayMillis(750L)))
    )
  }

  it should "refuse a delay that is too short or not a number" in {
    val item = delayInput(AppConfig.default)

    item.parse("20") shouldBe None
    item.parse("soon") shouldBe None
  }
