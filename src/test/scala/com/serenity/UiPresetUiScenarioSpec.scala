package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.BackgroundStyle
import com.serenity.rope.Balance
import com.serenity.ui.presets.UiPresetStore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UiPresetUiScenarioSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "UI preset scenario" should "save a visible draft and render its applied state" in {
    val store  = UiPresetStore(Files.createTempDirectory("ui-scenario-preset").resolve("presets.json"))
    val driver = UiScenarioDriver.create("ui-preset", uiPresetStore = Some(store)).unsafeRunSync()
    driver
      .updateState(state => state.copy(config = state.config.withBackgroundStyle(BackgroundStyle.Solid)))
      .unsafeRunSync()
    driver.stateManager
      .executeCommand(
        Command.typed(
          "save-scenario-preset",
          "Save scenario preset",
          CommandIntent.SaveUiPreset("Scenario"),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    val saved = store.find("Scenario").unsafeRunSync()
    val frame = driver.renderFrame("saved").unsafeRunSync()

    saved.map(_.config.backgroundStyle) shouldBe Some(BackgroundStyle.Solid)
    frame.evidence.layoutViolations shouldBe empty
  }
