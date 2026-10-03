package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent, MotionIntent, SettingsIntent}
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, MotionPreset}
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MotionUiScenarioSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "Motion UI scenario" should "apply family overrides and make reduced motion settle without geometry drift" in {
    val driver = UiScenarioDriver
      .create("motion-overrides", initialConfig = AppConfig.default.withMotionPreset(MotionPreset.Smooth))
      .unsafeRunSync()
    execute(driver, CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetEditorTextTransitionSpeedScale(0.5))))
    execute(
      driver,
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerTransitionSpeedScale(1.5)))
    )
    execute(driver, CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetUiTransitionSpeedScale(2.0))))
    execute(driver, CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCursorTransitionSpeedScale(0.75))))
    val overridden = driver.state.unsafeRunSync().persisted.config

    overridden.surfaceConfig.motionPreset shouldBe MotionPreset.Custom
    overridden.effectiveEditorTextTransitionSpeedScale shouldBe 0.5
    overridden.effectiveCommandRunnerTransitionSpeedScale shouldBe 1.5
    overridden.effectiveUiTransitionSpeedScale shouldBe 2.0
    overridden.effectiveCursorTransitionSpeedScale shouldBe 0.75

    execute(driver, CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionPreset(MotionPreset.Reduced))))
    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    val before = driver.renderFrame("reduced-open").unsafeRunSync().evidence.surfaceRects
    val after  = driver.renderFrame("reduced-settled").unsafeRunSync()
    after.evidence.surfaceRects shouldBe before
  }

  private def execute(driver: UiScenarioDriver, intent: CommandIntent): Unit =
    driver.stateManager
      .executeCommand(Command.typed("scenario-motion", "Scenario motion", intent, CommandCategory.Settings))
      .unsafeRunSync()
