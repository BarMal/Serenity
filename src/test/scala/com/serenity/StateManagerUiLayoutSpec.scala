package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent, InterfaceChromeIntent, SettingsIntent}
import com.serenity.config.{
  AppConfig,
  ConfigManager,
  ConfigManagerTestSupport,
  ConfigVersion,
  StatusLinePlacement,
  UiLayout
}
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.testkit.SharedDictionary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerUiLayoutSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def editorOn(configPath: Path): StateManager =
    val initial = ConfigManager.loadConfigIO(Some(configPath.toString)).unsafeRunSync()
    StateManager
      .apply(
        testLogger("StateManagerUiLayoutSpec"),
        sessionRootOverride = Some(TestTemp.directory("ui-layout-session")),
        initialConfig = initial,
        configPersistencePath = Some(configPath),
        dictionaryCache = SharedDictionary.cacheFor(initial)
      )
      .unsafeRunSync()

  private def configFile(text: String): Path =
    val path = TestTemp.directory("ui-layout-config").resolve("config.conf")
    Files.writeString(path, s"config.version = ${ConfigVersion.Current.value}\n$text")
    path

  private def toggleLayout(stateManager: StateManager): Unit =
    stateManager
      .executeCommand(
        Command.typed(
          "toggle-layout",
          "Switch between the minimal and conventional layouts.",
          CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.ToggleUiLayout)),
          CommandCategory.View
        )
      )
      .unsafeRunSync()
    stateManager.runtimeLifecycle.awaitEffects.unsafeRunSync()

  private def assigns(text: String, key: String): Boolean =
    text.linesIterator.map(_.trim).exists(_.startsWith(s"$key ="))

  private def live(stateManager: StateManager): AppConfig =
    stateManager.getCurrentState.unsafeRunSync().persisted.config

  "toggle-layout" should "be a registered command" in {
    com.serenity.command.CommandRegistry.withToggleUI.getAllCommands.map(_.name) should contain("toggle-layout")
  }

  it should "switch a minimal layout to the conventional one and write only the layout key" in {
    val path         = configFile("editor.tab_size = 3\n")
    val stateManager = editorOn(path)

    toggleLayout(stateManager)

    live(stateManager) shouldBe ConfigManagerTestSupport.loadConfig(Some(path.toString))
    live(stateManager).uiLayout shouldBe UiLayout.Conventional
    live(stateManager).surfaceConfig.showLineNumbers shouldBe true
    live(stateManager).statusLine.placement shouldBe StatusLinePlacement.Pinned
    val text = Files.readString(path)
    text should include("ui.layout = conventional")
    assigns(text, "editor.line_numbers") shouldBe false
    assigns(text, "status.placement") shouldBe false
  }

  it should "switch back again" in {
    val path         = configFile("ui.layout = conventional\n")
    val stateManager = editorOn(path)

    toggleLayout(stateManager)

    live(stateManager).uiLayout shouldBe UiLayout.Minimal
    live(stateManager).surfaceConfig.showLineNumbers shouldBe false
    Files.readString(path) should include("ui.layout = minimal")
  }

  it should "leave a setting the file writes where the file put it" in {
    val path         = configFile("editor.line_numbers = false\nstatus.placement = floating\n")
    val stateManager = editorOn(path)

    toggleLayout(stateManager)

    live(stateManager).uiLayout shouldBe UiLayout.Conventional
    live(stateManager).surfaceConfig.showLineNumbers shouldBe false
    live(stateManager).statusLine.placement shouldBe StatusLinePlacement.Floating
    live(stateManager).surfaceConfig.showPaneHeaders shouldBe true
    val text = Files.readString(path)
    text should include("editor.line_numbers = false")
    text should include("status.placement = floating")
    assigns(text, "editor.pane_headers") shouldBe false
  }

  it should "apply the layout the settings row picks" in {
    val path         = configFile("")
    val stateManager = editorOn(path)

    stateManager
      .executeCommand(
        Command.typed(
          "set-layout",
          "Set the layout",
          CommandIntent.Settings(
            SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetUiLayout(UiLayout.Conventional))
          ),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager.runtimeLifecycle.awaitEffects.unsafeRunSync()

    live(stateManager).uiLayout shouldBe UiLayout.Conventional
    live(stateManager).surfaceConfig.scrollbarEnabled shouldBe true
  }
