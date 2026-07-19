package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.{BackgroundStyle, MotionPreset}
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

  it should "preview edits, discard explicitly, and restore the saved preset after restart" in {
    val path   = Files.createTempDirectory("ui-scenario-preset-restart").resolve("presets.json")
    val store  = UiPresetStore(path)
    val driver = UiScenarioDriver.create("ui-preset-transactions", uiPresetStore = Some(store)).unsafeRunSync()

    driver
      .updateState(state => state.copy(config = state.config.withBackgroundStyle(BackgroundStyle.Solid)))
      .unsafeRunSync()
    val savedMotion = driver.state.unsafeRunSync().config.motionPreset
    execute(driver, CommandIntent.SaveUiPreset("Scenario"))
    execute(driver, CommandIntent.SetMotionPreset(MotionPreset.Subtle))
    val preview = driver.renderFrame("preview").unsafeRunSync()
    driver.state.unsafeRunSync().config.motionPreset shouldBe MotionPreset.Subtle
    preview.evidence.layoutViolations shouldBe empty
    store.find("Scenario").unsafeRunSync().map(_.config.motionPreset) shouldBe Some(savedMotion)

    execute(driver, CommandIntent.DiscardUiPresetDraft)
    val discarded = driver.state.unsafeRunSync()
    discarded.config.backgroundStyle shouldBe BackgroundStyle.Solid
    discarded.config.motionPreset shouldBe savedMotion

    val restarted = UiScenarioDriver.create("ui-preset-restarted", uiPresetStore = Some(store)).unsafeRunSync()
    execute(restarted, CommandIntent.ApplyUiPreset("Scenario"))
    restarted.state.unsafeRunSync().config.backgroundStyle shouldBe BackgroundStyle.Solid
    restarted.renderFrame("restarted").unsafeRunSync().evidence.layoutViolations shouldBe empty
  }

  it should "recover after a preset persistence failure" in {
    val parentFile = Files.createTempFile("ui-scenario-preset-failure", ".tmp")
    val broken     = UiPresetStore(parentFile.resolve("presets.json"))
    val driver     = UiScenarioDriver.create("ui-preset-failure", uiPresetStore = Some(broken)).unsafeRunSync()

    execute(driver, CommandIntent.SaveUiPreset("Broken"))
    broken.find("Broken").unsafeRunSync() shouldBe empty
    driver.renderFrame("failure").unsafeRunSync().evidence.layoutViolations shouldBe empty

    val healthy   = UiPresetStore(Files.createTempDirectory("ui-scenario-preset-recovery").resolve("presets.json"))
    val recovered = UiScenarioDriver.create("ui-preset-recovery", uiPresetStore = Some(healthy)).unsafeRunSync()
    execute(recovered, CommandIntent.SaveUiPreset("Recovered"))
    healthy.find("Recovered").unsafeRunSync() should not be empty
  }

  private def execute(driver: UiScenarioDriver, intent: CommandIntent): Unit =
    driver.stateManager
      .executeCommand(Command.typed("scenario-preset", "Scenario preset", intent, CommandCategory.Settings))
      .unsafeRunSync()
