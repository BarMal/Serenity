package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.command.*
import com.serenity.config.{AppConfig, AppMode, ConfigManager}
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.PanelPosition
import com.serenity.{DockedPanelFixtures, StateManagerTestSupport}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The panels that belong to code go with it: leaving code mode puts them away whichever way the mode changes -- the
  * command, a workflow preset, the config file edited from outside -- and entering code mode brings none of them back.
  */
class ModeSwitchPanelsSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  final private class Editor(val manager: StateManager, val directory: Path):

    def state: AppState = manager.getCurrentState.unsafeRunSync()

    def docked: List[PanelId] =
      state.pinnedSurfaces.flatMap(surface => PanelId.forContent(surface.content))

    def dockPanels(): Unit =
      manager
        .updateStateValidated(
          DockedPanelFixtures.dockAllContent(
            _,
            List(
              (PanelId.Outline.surfaceId, SurfaceContent.Outline(Nil), PanelPosition.Right, 30),
              (PanelId.Diagnostics.surfaceId, SurfaceContent.Diagnostics(Nil), PanelPosition.Bottom, 10),
              (
                PanelId.ProjectOutput.surfaceId,
                SurfaceContent.Terminal("Project task stopped", 20),
                PanelPosition.Bottom,
                14
              )
            )
          )
        )
        .unsafeRunSync()

    def run(command: Command): Unit =
      (manager.executeCommand(command) >> manager.runtimeLifecycle.awaitEffects).unsafeRunSync()

    def switchTo(mode: AppMode): Unit =
      run(
        Command.typed(
          s"app-mode-${mode.configKey}",
          s"Switch to ${mode.configKey} mode",
          CommandIntent.View(ViewIntent.SetAppMode(mode)),
          CommandCategory.Settings
        )
      )

    def applyWritingPreset(): Unit =
      run(
        Command.typed(
          "apply-writing-preset",
          "Apply the Writing workflow",
          CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Writing")),
          CommandCategory.View
        )
      )

    def reloadConfig(mode: AppMode): Unit =
      Files.writeString(
        directory.resolve("config.conf"),
        ConfigManager.configToString(AppConfig.default.withAppMode(mode))
      )
      (manager.fileService.configWatch.traverse_(_.reload) >> manager.runtimeLifecycle.awaitEffects).unsafeRunSync()

    def resetSettings(): Unit =
      run(
        Command.typed(
          "reset-settings",
          "Reset Settings",
          CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.ResetSettings)),
          CommandCategory.Settings
        )
      )

  private def editor(mode: AppMode): Editor =
    val directory = Files.createTempDirectory("mode-switch-panels")
    val config    = AppConfig.default.withAppMode(mode)
    val manager = StateManager
      .apply(
        testLogger("ModeSwitchPanelsSpec"),
        sessionRootOverride = Some(directory.resolve("session")),
        initialConfig = config,
        configPersistencePath = Some(directory.resolve("config.conf")),
        dictionaryCache = SharedDictionary.cacheFor(config)
      )
      .unsafeRunSync()
    Editor(manager, directory)

  private val codePanelsAway = List(PanelId.Outline)

  "The mode command switching to prose" should "put away the code panels and keep the others" in {
    val editor = this.editor(AppMode.Code)
    editor.dockPanels()
    editor.docked.toSet shouldBe Set(PanelId.Outline, PanelId.Diagnostics, PanelId.ProjectOutput)

    editor.switchTo(AppMode.Prose)

    editor.docked shouldBe codePanelsAway
  }

  "The config file switching to prose" should "put away the code panels, as the command does" in {
    val editor = this.editor(AppMode.Code)
    editor.dockPanels()

    editor.reloadConfig(AppMode.Prose)

    editor.state.persisted.config.appMode shouldBe AppMode.Prose
    editor.docked shouldBe codePanelsAway
  }

  "A workflow preset switching to prose" should "put away the code panels, as the command does" in {
    val editor = this.editor(AppMode.Code)
    editor.dockPanels()

    editor.applyWritingPreset()

    editor.state.persisted.config.appMode shouldBe AppMode.Prose
    editor.docked should not contain PanelId.ProjectOutput
    editor.docked should not contain PanelId.Diagnostics
  }

  "Going back to code mode" should "bring back no panel, by any route" in {
    val byCommand = this.editor(AppMode.Code)
    byCommand.dockPanels()
    byCommand.switchTo(AppMode.Prose)
    byCommand.switchTo(AppMode.Code)

    val byConfig = this.editor(AppMode.Code)
    byConfig.dockPanels()
    byConfig.reloadConfig(AppMode.Prose)
    byConfig.reloadConfig(AppMode.Code)

    byCommand.state.persisted.config.appMode shouldBe AppMode.Code
    byConfig.state.persisted.config.appMode shouldBe AppMode.Code
    byCommand.docked shouldBe codePanelsAway
    byConfig.docked shouldBe byCommand.docked
  }

  "Reset Settings entering code mode" should "leave the panels as they are" in {
    val editor = this.editor(AppMode.Prose)
    editor.manager
      .updateStateValidated(
        DockedPanelFixtures.dock(
          _,
          PanelId.Outline.surfaceId,
          SurfaceContent.Outline(Nil),
          PanelPosition.Right,
          30
        )
      )
      .unsafeRunSync()

    editor.resetSettings()

    editor.state.persisted.config.appMode shouldBe AppMode.Code
    editor.docked shouldBe List(PanelId.Outline)
  }
