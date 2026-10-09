package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.command.{Command, CommandCategory, CommandIntent, FontIntent, SettingsIntent}
import com.serenity.config.AppConfigOps.*
import com.serenity.config.{AppConfig, ConfigManager}
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.testkit.SharedDictionary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The config file as a running editor sees it (#1934): edits made outside apply without a restart, and the editor's
  * own writes never come back as edits that undo what was just set.
  */
class StateManagerConfigReloadSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def editorOn(configPath: Path, initial: AppConfig): StateManager =
    StateManager
      .apply(
        testLogger("StateManagerConfigReloadSpec"),
        sessionRootOverride = Some(Files.createTempDirectory("config-reload-session")),
        initialConfig = initial,
        configPersistencePath = Some(configPath),
        dictionaryCache = SharedDictionary.cacheFor(initial)
      )
      .unsafeRunSync()

  private def configFile: Path = Files.createTempDirectory("config-reload").resolve("config.conf")

  private def reloadFromWatcher(stateManager: StateManager): Unit =
    (stateManager.fileService.configWatch.traverse_(_.reload) >> stateManager.runtimeLifecycle.awaitEffects)
      .unsafeRunSync()

  private def liveConfig(stateManager: StateManager): AppConfig =
    stateManager.getCurrentState.unsafeRunSync().persisted.config

  private def increaseFontSize(stateManager: StateManager): Unit =
    stateManager
      .executeCommand(
        Command.typed(
          "increase-font-size",
          "Increase font size",
          CommandIntent.Settings(SettingsIntent.Font(FontIntent.IncreaseFontSize)),
          CommandCategory.View
        )
      )
      .unsafeRunSync()

  "A running editor" should "apply an edit made to the config file from outside, without a restart" in {
    val path         = configFile
    val stateManager = editorOn(path, AppConfig.default)

    Files.writeString(path, ConfigManager.configToString(AppConfig.default.withWheelScrollLines(7)))
    reloadFromWatcher(stateManager)

    liveConfig(stateManager).inputConfig.wheelScrollLines shouldBe 7
  }

  it should "keep a burst of settings changes when the watcher reports the writes they caused" in {
    val path         = configFile
    val stateManager = editorOn(path, AppConfig.default)
    val before       = liveConfig(stateManager).editorConfig.fontConfig.fontSize

    (1 to 5).foreach(_ => increaseFontSize(stateManager))
    stateManager.runtimeLifecycle.awaitEffects.unsafeRunSync()
    val afterBurst = liveConfig(stateManager)
    reloadFromWatcher(stateManager)

    afterBurst.editorConfig.fontConfig.fontSize shouldBe before + 5.0f
    liveConfig(stateManager) shouldBe afterBurst
    ConfigManager
      .loadConfigResultIO(Some(path.toString))
      .unsafeRunSync()
      .map(_.config.editorConfig.fontConfig.fontSize) shouldBe
      Right(before + 5.0f)
  }

  it should "have nothing to watch when it keeps no config file" in {
    createStateManager("StateManagerConfigReloadSpec").fileService.configWatch shouldBe None
  }
