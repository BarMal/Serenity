package com.serenity

import java.nio.file.Files
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.ConfigManager
import com.serenity.keystroke.events.*
import com.serenity.state.manager.StateManager
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.{FontConfig, TextScaleMode}
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerFontConfigSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private val CodeFontSettingsGroupId = "settings-code-font"
  private val UiFontSettingsGroupId   = "settings-ui-font"

  private def openRunner(stateManager: StateManager): Unit =
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

  private def openSettingsSubmenu(stateManager: StateManager, groupId: String): Unit =
    openRunner(stateManager)
    settingsGroupSearchTerm(groupId).foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

  private def settingsGroupSearchTerm(groupId: String): String =
    groupId match
      case CodeFontSettingsGroupId => "code font"
      case UiFontSettingsGroupId   => "ui font"
      case other                   => other.stripPrefix("settings-").replace("-", " ")

  "StateManager" should "invoke the runtime font callback when changing code font size from font settings" in {
    val observed = AtomicReference[List[FontConfig]](Nil)
    val stateManager =
      createStateManager("StateManagerFontConfigSpec", config => IO(observed.updateAndGet(_ :+ config)))

    openSettingsSubmenu(stateManager, CodeFontSettingsGroupId)
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    List('1', '3').foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    observed.get() should not be empty
    observed.get().last.codeFontSize shouldBe 13.0f
  }

  it should "invoke the runtime font callback when changing UI font size from UI font settings" in {
    val observed = AtomicReference[List[FontConfig]](Nil)
    val stateManager =
      createStateManager("StateManagerFontConfigSpec", config => IO(observed.updateAndGet(_ :+ config)))

    openSettingsSubmenu(stateManager, UiFontSettingsGroupId)
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    List('1', '5').foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    observed.get() should not be empty
    observed.get().last.uiFontSize shouldBe 15.0f
  }

  it should "invoke the runtime font callback when changing code ligature shaping from font settings" in {
    val observed = AtomicReference[List[FontConfig]](Nil)
    val stateManager =
      createStateManager("StateManagerFontConfigSpec", config => IO(observed.updateAndGet(_ :+ config)))

    openSettingsSubmenu(stateManager, CodeFontSettingsGroupId)
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveRight).unsafeRunSync()

    observed.get() should not be empty
    observed.get().last.codeLigatures shouldBe false
  }

  it should "invoke the runtime font callback when changing UI font family from UI font settings" in {
    val observed = AtomicReference[List[FontConfig]](Nil)
    val stateManager =
      createStateManager("StateManagerFontConfigSpec", config => IO(observed.updateAndGet(_ :+ config)))

    openSettingsSubmenu(stateManager, UiFontSettingsGroupId)
    stateManager.applyEvent(Enter).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    observed.get() should not be empty
    observed.get().last.uiFontFamily shouldBe FontLoader.availableUiFamilies.head
  }

  it should "resolve auto text scale from the live device scale when switching modes at runtime" in {
    val observed    = AtomicReference[List[FontConfig]](Nil)
    val sessionRoot = Files.createTempDirectory("font-scale-auto-runtime")
    val initialConfig = com.serenity.config.AppConfig.default.withFontConfig(
      FontConfig(textScaleMode = TextScaleMode.Manual, textScaleMultiplier = 1.0)
    )
    val stateManager =
      StateManager
        .apply(
          testLogger("StateManagerFontConfigSpec"),
          onFontConfigChanged = config => IO(observed.updateAndGet(_ :+ config)),
          deviceTextScaleProvider = IO.pure(2.0),
          sessionRootOverride = Some(sessionRoot),
          initialConfig = initialConfig
        )
        .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "text-scale-auto",
          "Set text scale to auto",
          CommandIntent.SetTextScaleMode(TextScaleMode.Auto),
          CommandCategory.View
        )
      )
      .unsafeRunSync()

    val fontConfig = stateManager.getCurrentState.unsafeRunSync().config.fontConfig
    fontConfig.textScaleMode shouldBe TextScaleMode.Auto
    fontConfig.textScaleMultiplier shouldBe 2.0
    observed.get() should not be empty
    observed.get().last.textScaleMultiplier shouldBe 2.0
  }

  it should "refresh auto text scale when the viewport moves to a scaled display" in {
    val deviceScale = AtomicReference(1.0)
    val observed    = AtomicReference[List[FontConfig]](Nil)
    val stateManager =
      createStateManager(
        "StateManagerFontConfigSpec",
        config => IO(observed.updateAndGet(_ :+ config)),
        IO(deviceScale.get())
      )

    deviceScale.set(2.0)
    stateManager.handleViewportResize(ViewportSize(120, 40)).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().config.fontConfig.textScaleMultiplier shouldBe 2.0
    observed.get().last.textScaleMultiplier shouldBe 2.0
  }

  it should "persist font family changes made through UI font settings" in {
    val sessionRoot  = Files.createTempDirectory("font-config-persistence")
    val expectedFont = FontLoader.availableUiFamilies.lift(1).getOrElse(FontLoader.availableUiFamilies.head)
    val stateManager =
      StateManager
        .apply(
          testLogger("StateManagerFontConfigSpec"),
          sessionRootOverride = Some(sessionRoot)
        )
        .unsafeRunSync()

    openSettingsSubmenu(stateManager, UiFontSettingsGroupId)
    stateManager.applyEvent(Enter).unsafeRunSync()
    if FontLoader.availableUiFamilies.size > 1 then stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val loaded = stateManager.loadSession().unsafeRunSync()

    loaded.map(_.config.fontConfig.uiFontFamily) shouldBe Some(expectedFont)
  }

  it should "persist font size changes made through code font settings to the config file" in {
    val configFile = Files.createTempDirectory("font-config-file").resolve("config.conf")
    val stateManager =
      StateManager
        .apply(
          testLogger("StateManagerFontConfigSpec"),
          configPersistencePath = Some(configFile)
        )
        .unsafeRunSync()

    openSettingsSubmenu(stateManager, CodeFontSettingsGroupId)
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    List('1', '6').foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val saved = ConfigManager.loadConfig(Some(configFile.toString))
    saved.fontConfig.codeFontSize shouldBe 16.0f
  }

  it should "persist font family changes made through UI font settings to the config file" in {
    val configFile   = Files.createTempDirectory("font-family-config-file").resolve("config.conf")
    val expectedFont = FontLoader.availableUiFamilies.lift(1).getOrElse(FontLoader.availableUiFamilies.head)
    val stateManager =
      StateManager
        .apply(
          testLogger("StateManagerFontConfigSpec"),
          configPersistencePath = Some(configFile)
        )
        .unsafeRunSync()

    openSettingsSubmenu(stateManager, UiFontSettingsGroupId)
    stateManager.applyEvent(Enter).unsafeRunSync()
    if FontLoader.availableUiFamilies.size > 1 then stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val saved = ConfigManager.loadConfig(Some(configFile.toString))
    saved.fontConfig.uiFontFamily shouldBe expectedFont
  }
