package com.serenity

import java.awt.Color
import java.nio.charset.StandardCharsets
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
        |font.code.size = 15.0
        |font.text.size = 16.0
        |font.ui.size = 13.0
        |font.code.ligatures = false
        |font.text.ligatures = true
        |font.ui.ligatures = true
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.fontConfig.codeFontFamily shouldBe "Monospaced"
    config.fontConfig.textFontFamily shouldBe "Serif"
    config.fontConfig.uiFontFamily shouldBe "Dialog"
    config.fontConfig.codeFontSize shouldBe 15.0f
    config.fontConfig.textFontSize shouldBe 16.0f
    config.fontConfig.uiFontSize shouldBe 13.0f
    config.fontConfig.codeLigatures shouldBe false
    config.fontConfig.textLigatures shouldBe true
    config.fontConfig.uiLigatures shouldBe true

    ConfigManager.configToString(config) should include("font.ui.family = Dialog")
    ConfigManager.configToString(config) should include("font.code.size = 15.0")
    ConfigManager.configToString(config) should include("font.text.size = 16.0")
    ConfigManager.configToString(config) should include("font.ui.ligatures = true")
  }

  it should "load legacy shared font size and ligature keys for code and prose fonts" in {
    val configFile = Files.createTempFile("serenity-legacy-font-config", ".conf")
    Files.writeString(
      configFile,
      """font.size = 17.0
        |font.ligatures = false
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.fontConfig.codeFontSize shouldBe 17.0f
    config.fontConfig.textFontSize shouldBe 17.0f
    config.fontConfig.codeLigatures shouldBe false
    config.fontConfig.textLigatures shouldBe false
    config.fontConfig.uiLigatures shouldBe false
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

  it should "load and write preferred window size" in {
    val configFile = Files.createTempFile("serenity-window-size-config", ".conf")
    Files.writeString(
      configFile,
      """window.preferred.width = 1400
        |window.preferred.height = 900
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.preferredWindowSize shouldBe Some(PreferredWindowSize(1400, 900))
    ConfigManager.configToString(config) should include("window.preferred.width = 1400")
    ConfigManager.configToString(config) should include("window.preferred.height = 900")
  }

  it should "close loaded config files and save using UTF-8" in {
    val configFile = Files.createTempFile("serenity-config-utf8", ".conf")
    Files.writeString(
      configFile,
      """font.text.family = Sérif
        |""".stripMargin,
      StandardCharsets.UTF_8
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))
    config.fontConfig.textFontFamily shouldBe "Sérif"

    Files.delete(configFile)

    ConfigManager.saveConfig(config, configFile) shouldBe true
    Files.readString(configFile, StandardCharsets.UTF_8) should include("font.text.family = Sérif")
  }
