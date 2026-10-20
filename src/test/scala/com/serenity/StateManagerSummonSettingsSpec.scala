package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent, InterfaceChromeIntent, SettingsIntent}
import com.serenity.config.{AppConfig, ChromeElement, ConfigManager, ConfigVersion, HotkeyAction, SummonConfig}
import com.serenity.keystroke.Modifier
import com.serenity.keystroke.events.ChromeSummonToggled
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.testkit.SharedDictionary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerSummonSettingsSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def configFile(text: String): Path =
    val path = TestTemp.directory("summon-config").resolve("config.conf")
    Files.writeString(path, s"config.version = ${ConfigVersion.Current.value}\n$text")
    path

  private def editorOn(configPath: Path): StateManager =
    val initial = ConfigManager.loadConfigIO(Some(configPath.toString)).unsafeRunSync()
    StateManager
      .apply(
        testLogger("StateManagerSummonSettingsSpec"),
        sessionRootOverride = Some(TestTemp.directory("summon-session")),
        initialConfig = initial,
        configPersistencePath = Some(configPath),
        dictionaryCache = SharedDictionary.cacheFor(initial)
      )
      .unsafeRunSync()

  private def choose(stateManager: StateManager, intent: InterfaceChromeIntent): Unit =
    stateManager
      .executeCommand(
        Command.typed(
          "set-summon",
          "Change a summon setting.",
          CommandIntent.Settings(SettingsIntent.InterfaceChrome(intent)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager.runtimeLifecycle.awaitEffects.unsafeRunSync()

  private def live(stateManager: StateManager): AppConfig =
    stateManager.getCurrentState.unsafeRunSync().persisted.config

  "Choosing a summon letter" should "set it and write it to the config file" in {
    val path         = configFile("")
    val stateManager = editorOn(path)

    choose(stateManager, InterfaceChromeIntent.SetSummonLetter(ChromeElement.LineNumbers, Some('x')))

    live(stateManager).summon.letterOf(ChromeElement.LineNumbers) shouldBe Some('x')
    Files.readString(path) should include("ui.summon.line_numbers = x")
  }

  it should "write off for an element given no chord" in {
    val path         = configFile("")
    val stateManager = editorOn(path)

    choose(stateManager, InterfaceChromeIntent.SetSummonLetter(ChromeElement.Scrollbar, None))

    live(stateManager).summon.letterOf(ChromeElement.Scrollbar) shouldBe None
    Files.readString(path) should include("ui.summon.scrollbar = off")
  }

  it should "refuse a letter another element already has, leaving the config as it was" in {
    val path         = configFile("")
    val stateManager = editorOn(path)

    choose(stateManager, InterfaceChromeIntent.SetSummonLetter(ChromeElement.Scrollbar, Some('l')))

    live(stateManager).summon shouldBe SummonConfig()
    Files.readString(path) should not include "ui.summon"
  }

  it should "refuse a letter whose chord a hotkey holds" in {
    val path         = configFile("hotkey.find = [\"alt+shift+q\"]\n")
    val stateManager = editorOn(path)
    live(stateManager).inputConfig.hotkeyConfig.bindingsFor(HotkeyAction.Find).map(_.render) shouldBe
      List("alt+shift+q")

    choose(stateManager, InterfaceChromeIntent.SetSummonLetter(ChromeElement.Scrollbar, Some('q')))

    live(stateManager).summon.letterOf(ChromeElement.Scrollbar) shouldBe Some('b')
  }

  "Choosing the base chord" should "set it and write it" in {
    val path         = configFile("")
    val stateManager = editorOn(path)

    choose(stateManager, InterfaceChromeIntent.SetSummonModifiers(Set(Modifier.Ctrl, Modifier.Alt)))

    live(stateManager).summon.modifiers shouldBe Set(Modifier.Ctrl, Modifier.Alt)
    Files.readString(path) should include("ui.summon.modifiers = \"ctrl+alt\"")
  }

  "Choosing the lock key" should "set it and write it" in {
    val path         = configFile("")
    val stateManager = editorOn(path)

    choose(stateManager, InterfaceChromeIntent.SetSummonLock(SummonConfig.parseLock("enter").getOrElse(fail("enter"))))

    Files.readString(path) should include("ui.summon.lock = enter")
  }

  it should "refuse a key that is one of the summon letters" in {
    val stateManager = editorOn(configFile(""))

    choose(stateManager, InterfaceChromeIntent.SetSummonLock(SummonConfig.parseLock("l").getOrElse(fail("l"))))

    live(stateManager).summon.lock shouldBe SummonConfig.DefaultLock
  }

  "Summoning chrome" should "never write the config file" in {
    val path         = configFile("editor.tab_size = 3\n")
    val before       = Files.readString(path)
    val stateManager = editorOn(path)

    stateManager.applyEvent(ChromeSummonToggled(ChromeElement.LineNumbers)).unsafeRunSync()
    stateManager.runtimeLifecycle.awaitEffects.unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().chrome.lineNumbers shouldBe true
    live(stateManager).surfaceConfig.showLineNumbers shouldBe false
    Files.readString(path) shouldBe before
  }
