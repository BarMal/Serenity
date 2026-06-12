package com.serenity

import java.awt.Color
import java.nio.file.Files

import com.serenity.config.*
import com.serenity.keystroke.{InputKey, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigManagerSpec extends AnyFlatSpec with Matchers:

  "ConfigManager" should "load configured hotkey overrides from a config file" in {
    val configFile = Files.createTempFile("serenity-config", ".conf")
    Files.writeString(
      configFile,
      """hotkey.command_palette = ctrl+k
        |hotkey.file_search = ctrl+alt+f
        |hotkey.previous_tab = ctrl+shift+tab
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.hotkeyConfig
      .bindingsFor(HotkeyAction.ToggleCommandRunner)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Character,
      character = Some('k'),
      modifiers = Set(Modifier.Ctrl)
    )
    config.hotkeyConfig
      .bindingsFor(HotkeyAction.FileSearch)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Character,
      character = Some('f'),
      modifiers = Set(Modifier.Ctrl, Modifier.Alt)
    )
    config.hotkeyConfig
      .bindingsFor(HotkeyAction.PreviousTab)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Tab,
      character = None,
      modifiers = Set(Modifier.Ctrl, Modifier.Shift)
    )
  }

  it should "parse richer key trigger names for local keymap overrides" in {
    val configFile = Files.createTempFile("serenity-config", ".conf")
    Files.writeString(
      configFile,
      """keymap.editor.page_down = ctrl+pagedown
        |keymap.command_runner.submit = ctrl+enter
        |keymap.modal.dismiss = ctrl+escape
        |keymap.panel.activate = ctrl+enter
        |keymap.peek.accept = ctrl+enter
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.focusedKeymapConfig.editor
      .bindingsFor(EditorKeyAction.PageDown)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.PageDown,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
    config.focusedKeymapConfig.commandRunner
      .bindingsFor(CommandRunnerKeyAction.Submit)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Enter,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
    config.focusedKeymapConfig.modal
      .bindingsFor(ModalKeyAction.Dismiss)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Escape,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
    config.focusedKeymapConfig.panel
      .bindingsFor(PanelKeyAction.Activate)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Enter,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
    config.focusedKeymapConfig.peek
      .bindingsFor(PeekKeyAction.Accept)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Enter,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
  }

  it should "load and write font configuration including UI font family" in {
    val configFile = Files.createTempFile("serenity-font-config", ".conf")
    Files.writeString(
      configFile,
      """font.code.family = Monospaced
        |font.text.family = Serif
        |font.ui.family = Dialog
        |font.size = 15.0
        |font.ui.size = 13.0
        |font.ligatures = false
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.fontConfig.codeFontFamily shouldBe "Monospaced"
    config.fontConfig.textFontFamily shouldBe "Serif"
    config.fontConfig.uiFontFamily shouldBe "Dialog"
    config.fontConfig.fontSize shouldBe 15.0f
    config.fontConfig.uiFontSize shouldBe 13.0f
    config.fontConfig.enableLigatures shouldBe false

    ConfigManager.configToString(config) should include("font.ui.family = Dialog")
  }

  it should "load and write active and inactive cursor colour overrides" in {
    val configFile = Files.createTempFile("serenity-cursor-config", ".conf")
    Files.writeString(
      configFile,
      """cursor.active.color = #3366CC
        |cursor.inactive.color = #CC663380
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.cursorColors.active shouldBe Some(new Color(0x33, 0x66, 0xcc))
    config.cursorColors.inactive shouldBe Some(new Color(0xcc, 0x66, 0x33, 0x80))
    ConfigManager.configToString(config) should include("cursor.active.color = #3366CC")
    ConfigManager.configToString(config) should include("cursor.inactive.color = #CC663380")
  }

  it should "ignore invalid cursor colour overrides" in {
    val configFile = Files.createTempFile("serenity-cursor-config", ".conf")
    Files.writeString(
      configFile,
      """cursor.active.color = not-a-colour
        |cursor.inactive.color = #xyz
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.cursorColors.active shouldBe None
    config.cursorColors.inactive shouldBe None
  }
