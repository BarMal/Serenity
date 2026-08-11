package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.{BackgroundStyle, MaterialPreset, MotionPreset}
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.rope.Balance
import com.serenity.state.models.SurfaceContent
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
          CommandIntent.SaveUiPresetAsNew("Scenario"),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    val saved = store.find("Scenario").unsafeRunSync()
    val frame = driver.renderFrame("saved").unsafeRunSync()

    saved.map(_.config.backgroundStyle) shouldBe Some(BackgroundStyle.Solid)
    frame.evidence.layoutViolations shouldBe empty
  }

  it should "leave a saved preset untouched while later settings change, and reapply it after restart" in {
    val path   = Files.createTempDirectory("ui-scenario-preset-restart").resolve("presets.json")
    val store  = UiPresetStore(path)
    val driver = UiScenarioDriver.create("ui-preset-transactions", uiPresetStore = Some(store)).unsafeRunSync()

    driver
      .updateState(state => state.copy(config = state.config.withBackgroundStyle(BackgroundStyle.Solid)))
      .unsafeRunSync()
    val savedMotion = driver.state.unsafeRunSync().config.motionPreset
    execute(driver, CommandIntent.SaveUiPresetAsNew("Scenario"))
    val beforeChange = driver.renderFrame("before-settings-change").unsafeRunSync()
    execute(driver, CommandIntent.SetMotionPreset(MotionPreset.Subtle))
    val changed = driver.renderFrame("changed").unsafeRunSync()
    driver.state.unsafeRunSync().config.motionPreset shouldBe MotionPreset.Subtle
    changed.evidence.layoutViolations shouldBe empty
    store.find("Scenario").unsafeRunSync().map(_.config.motionPreset) shouldBe Some(savedMotion)

    execute(driver, CommandIntent.ApplyUiPreset("Scenario"))
    val reapplied = driver.state.unsafeRunSync()
    reapplied.config.backgroundStyle shouldBe BackgroundStyle.Solid
    reapplied.config.motionPreset shouldBe savedMotion
    beforeChange.evidence.layoutViolations shouldBe empty
    driver.renderFrame("after-reapply").unsafeRunSync().evidence.layoutViolations shouldBe empty

    val restarted = UiScenarioDriver.create("ui-preset-restarted", uiPresetStore = Some(store)).unsafeRunSync()
    execute(restarted, CommandIntent.ApplyUiPreset("Scenario"))
    restarted.state.unsafeRunSync().config.backgroundStyle shouldBe BackgroundStyle.Solid
    restarted.renderFrame("restarted").unsafeRunSync().evidence.layoutViolations shouldBe empty
  }

  it should "retain frame evidence when overwriting a saved preset with current settings" in {
    val store  = UiPresetStore(Files.createTempDirectory("ui-scenario-preset-save").resolve("presets.json"))
    val driver = UiScenarioDriver.create("ui-preset-preview-save", uiPresetStore = Some(store)).unsafeRunSync()

    execute(driver, CommandIntent.SaveUiPresetAsNew("Scenario"))
    val beforeChange = driver.renderFrame("before-change-save").unsafeRunSync()
    val savedMotion  = store.find("Scenario").unsafeRunSync().map(_.config.motionPreset)
    execute(driver, CommandIntent.SetMotionPreset(MotionPreset.Subtle))
    val changed = driver.renderFrame("changed-save").unsafeRunSync()
    store.find("Scenario").unsafeRunSync().map(_.config.motionPreset) shouldBe savedMotion

    execute(driver, CommandIntent.OverwriteUiPreset("Scenario"))
    val saved = driver.renderFrame("after-save").unsafeRunSync()
    store.find("Scenario").unsafeRunSync().map(_.config.motionPreset) shouldBe Some(MotionPreset.Subtle)

    val restarted =
      UiScenarioDriver.create("ui-preset-preview-save-restarted", uiPresetStore = Some(store)).unsafeRunSync()
    execute(restarted, CommandIntent.ApplyUiPreset("Scenario"))
    val appliedAfterRestart = restarted.renderFrame("applied-after-restart").unsafeRunSync()

    restarted.state.unsafeRunSync().config.motionPreset shouldBe MotionPreset.Subtle
    beforeChange.evidence.layoutViolations shouldBe empty
    changed.evidence.layoutViolations shouldBe empty
    saved.evidence.layoutViolations shouldBe empty
    appliedAfterRestart.evidence.layoutViolations shouldBe empty
  }

  it should "keep changed settings across a session restart and offer preset actions in the runner" in {
    val sessionRoot = Files.createTempDirectory("ui-scenario-dirty-restart")
    val store       = UiPresetStore(sessionRoot.resolve("presets.json"))
    val driver = UiScenarioDriver
      .create("ui-preset-dirty-restart", uiPresetStore = Some(store), sessionRoot = Some(sessionRoot))
      .unsafeRunSync()

    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    execute(driver, CommandIntent.SetMaterialPreset(MaterialPreset.Solid))
    val changed = driver.renderFrame("changed-before-restart").unsafeRunSync()
    driver.stateManager.saveSession().unsafeRunSync()

    val restarted = UiScenarioDriver
      .create("ui-preset-fresh-runtime", uiPresetStore = Some(store), sessionRoot = Some(sessionRoot))
      .unsafeRunSync()
    execute(restarted, CommandIntent.StartupRestoreSession)
    restarted.dispatch(ToggleCommandRunner).unsafeRunSync()
    val reopened      = restarted.state.unsafeRunSync()
    val reopenedFrame = restarted.renderFrame("changed-after-restart").unsafeRunSync()
    val runner = reopened.commandRunnerSurface
      .flatMap(_.content match
        case SurfaceContent.CommandPalette(current) => Some(current)
        case _                                      => None)
      .getOrElse(fail("command runner should reopen after session restore"))

    reopened.config.materialPreset shouldBe MaterialPreset.Solid
    store.find("Restart Draft").unsafeRunSync() shouldBe None
    inputIds(runner.settingsGroups) should contain allOf ("ui-preset-save-as-new", "ui-preset-overwrite")
    changed.evidence.layoutViolations shouldBe empty
    reopenedFrame.evidence.layoutViolations shouldBe empty
  }

  it should "recover after a preset persistence failure" in {
    val parentFile = Files.createTempFile("ui-scenario-preset-failure", ".tmp")
    val broken     = UiPresetStore(parentFile.resolve("presets.json"))
    val driver     = UiScenarioDriver.create("ui-preset-failure", uiPresetStore = Some(broken)).unsafeRunSync()

    execute(driver, CommandIntent.SaveUiPresetAsNew("Broken"))
    broken.find("Broken").unsafeRunSync() shouldBe empty
    driver.renderFrame("failure").unsafeRunSync().evidence.layoutViolations shouldBe empty

    val healthy   = UiPresetStore(Files.createTempDirectory("ui-scenario-preset-recovery").resolve("presets.json"))
    val recovered = UiScenarioDriver.create("ui-preset-recovery", uiPresetStore = Some(healthy)).unsafeRunSync()
    execute(recovered, CommandIntent.SaveUiPresetAsNew("Recovered"))
    healthy.find("Recovered").unsafeRunSync() should not be empty
  }

  private def execute(driver: UiScenarioDriver, intent: CommandIntent): Unit =
    driver.stateManager
      .executeCommand(Command.typed("scenario-preset", "Scenario preset", intent, CommandCategory.Settings))
      .unsafeRunSync()

  private def inputIds(groups: List[com.serenity.command.CommandSurfaceItem.GroupItem]): List[String] =
    groups.flatMap { group =>
      group.children.collect { case input: com.serenity.command.CommandSurfaceItem.InputItem => input.id } ++
        inputIds(group.children.collect { case child: com.serenity.command.CommandSurfaceItem.GroupItem => child })
    }
