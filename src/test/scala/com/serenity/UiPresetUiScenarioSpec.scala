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
    val beforePreview = driver.renderFrame("before-preview-discard").unsafeRunSync()
    execute(driver, CommandIntent.SetMotionPreset(MotionPreset.Subtle))
    val preview = driver.renderFrame("preview").unsafeRunSync()
    driver.state.unsafeRunSync().config.motionPreset shouldBe MotionPreset.Subtle
    preview.evidence.layoutViolations shouldBe empty
    store.find("Scenario").unsafeRunSync().map(_.config.motionPreset) shouldBe Some(savedMotion)

    execute(driver, CommandIntent.DiscardUiPresetDraft)
    val discarded = driver.state.unsafeRunSync()
    discarded.config.backgroundStyle shouldBe BackgroundStyle.Solid
    discarded.config.motionPreset shouldBe savedMotion
    beforePreview.evidence.layoutViolations shouldBe empty
    driver.renderFrame("after-discard").unsafeRunSync().evidence.layoutViolations shouldBe empty

    val restarted = UiScenarioDriver.create("ui-preset-restarted", uiPresetStore = Some(store)).unsafeRunSync()
    execute(restarted, CommandIntent.ApplyUiPreset("Scenario"))
    restarted.state.unsafeRunSync().config.backgroundStyle shouldBe BackgroundStyle.Solid
    restarted.renderFrame("restarted").unsafeRunSync().evidence.layoutViolations shouldBe empty
  }

  it should "retain before preview and saved frame evidence when committing a preset draft" in {
    val store  = UiPresetStore(Files.createTempDirectory("ui-scenario-preset-save").resolve("presets.json"))
    val driver = UiScenarioDriver.create("ui-preset-preview-save", uiPresetStore = Some(store)).unsafeRunSync()

    execute(driver, CommandIntent.SaveUiPreset("Scenario"))
    val beforePreview = driver.renderFrame("before-preview-save").unsafeRunSync()
    val savedMotion   = store.find("Scenario").unsafeRunSync().map(_.config.motionPreset)
    execute(driver, CommandIntent.SetMotionPreset(MotionPreset.Subtle))
    val preview = driver.renderFrame("preview-save").unsafeRunSync()
    store.find("Scenario").unsafeRunSync().map(_.config.motionPreset) shouldBe savedMotion

    execute(driver, CommandIntent.SaveUiPreset("Scenario"))
    val saved = driver.renderFrame("after-save").unsafeRunSync()
    store.find("Scenario").unsafeRunSync().map(_.config.motionPreset) shouldBe Some(MotionPreset.Subtle)
    beforePreview.evidence.layoutViolations shouldBe empty
    preview.evidence.layoutViolations shouldBe empty
    saved.evidence.layoutViolations shouldBe empty
  }

  it should "restore a dirty draft through the session path and discard it after reopening the runner" in {
    val sessionRoot = Files.createTempDirectory("ui-scenario-dirty-restart")
    val store       = UiPresetStore(sessionRoot.resolve("presets.json"))
    val driver = UiScenarioDriver
      .create("ui-preset-dirty-restart", uiPresetStore = Some(store), sessionRoot = Some(sessionRoot))
      .unsafeRunSync()
    val baselineMaterial = driver.state.unsafeRunSync().config.materialPreset

    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    execute(driver, CommandIntent.StartUiPresetDraft("Restart Draft"))
    execute(driver, CommandIntent.SetMaterialPreset(MaterialPreset.Solid))
    val preview = driver.renderFrame("dirty-preview-before-restart").unsafeRunSync()
    driver.stateManager.saveSession().unsafeRunSync()

    val restarted = UiScenarioDriver
      .create("ui-preset-fresh-runtime", uiPresetStore = Some(store), sessionRoot = Some(sessionRoot))
      .unsafeRunSync()
    execute(restarted, CommandIntent.StartupRestoreSession)
    restarted.dispatch(ToggleCommandRunner).unsafeRunSync()
    val reopened      = restarted.state.unsafeRunSync()
    val reopenedFrame = restarted.renderFrame("dirty-preview-after-restart").unsafeRunSync()
    val runner = reopened.commandRunnerSurface
      .flatMap(_.content match
        case SurfaceContent.CommandPalette(current) => Some(current)
        case _                                      => None)
      .getOrElse(fail("command runner should reopen after session restore"))

    reopened.config.materialPreset shouldBe MaterialPreset.Solid
    reopened.uiPresetEditSession.map(_.draftName) shouldBe Some("Restart Draft")
    reopened.uiPresetEditSession.map(_.dirty) shouldBe Some(true)
    store.find("Restart Draft").unsafeRunSync() shouldBe None
    inputIds(runner.settingsGroups) should contain allOf ("ui-preset-save", "ui-preset-discard")
    preview.evidence.layoutViolations shouldBe empty
    reopenedFrame.evidence.layoutViolations shouldBe empty

    execute(restarted, CommandIntent.DiscardUiPresetDraft)
    val discarded = restarted.state.unsafeRunSync()

    discarded.config.materialPreset shouldBe baselineMaterial
    discarded.uiPresetEditSession shouldBe None
    store.find("Restart Draft").unsafeRunSync() shouldBe None
    restarted.renderFrame("dirty-preview-after-discard").unsafeRunSync().evidence.layoutViolations shouldBe empty
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

  private def inputIds(groups: List[com.serenity.command.CommandSurfaceItem.GroupItem]): List[String] =
    groups.flatMap { group =>
      group.children.collect { case input: com.serenity.command.CommandSurfaceItem.InputItem => input.id } ++
        inputIds(group.children.collect { case child: com.serenity.command.CommandSurfaceItem.GroupItem => child })
    }
