package com.serenity

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
