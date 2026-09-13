package com.serenity

import java.nio.file.Files

import com.serenity.animation.WindowSitterConfig
import com.serenity.config.*
import com.serenity.keystroke.events.EditorEvent
import com.serenity.keystroke.{InputKey, Modifier}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Hotkey overrides, keymap parsing/round-tripping, and the focused keymap JSON codec. */
class ConfigManagerHotkeyKeymapSpec extends AnyFlatSpec with Matchers with OptionValues:

  "ConfigManager" should "persist window sitter controls" in {
    val config = AppConfig.default.withWindowSitterConfig(
      WindowSitterConfig(
        enabled = false,
        action = com.serenity.animation.WindowSitterAction.Blink,
        frames = Vector(".", "x"),
        activeTicks = 4,
        fastActiveTicks = 9,
        fastTypingThresholdMs = 275
      )
    )
    val configFile = Files.createTempFile("serenity-sitter-config", ".conf")
    Files.writeString(configFile, ConfigManager.configToString(config))

    val reloaded = ConfigManagerTestSupport.loadConfig(Some(configFile.toString)).windowSitterConfig
    reloaded.enabled shouldBe false
    reloaded.action shouldBe com.serenity.animation.WindowSitterAction.Blink
    reloaded.frames shouldBe Vector(".", "x")
    reloaded.activeTicks shouldBe 4
    reloaded.fastActiveTicks shouldBe 9
    reloaded.fastTypingThresholdMs shouldBe 275
  }

  "ConfigManager" should "load configured hotkey overrides from a config file" in {
    val configFile = Files.createTempFile("serenity-config", ".conf")
    Files.writeString(
      configFile,
      """hotkey.command_palette = ctrl+k
        |hotkey.file_search = ctrl+alt+f
        |hotkey.previous_tab = ctrl+shift+tab
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.inputConfig.hotkeyConfig
      .bindingsFor(HotkeyAction.ToggleCommandRunner)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Character,
      character = Some('k'),
      modifiers = Set(Modifier.Ctrl)
    )
    config.inputConfig.hotkeyConfig
      .bindingsFor(HotkeyAction.FileSearch)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Character,
      character = Some('f'),
      modifiers = Set(Modifier.Ctrl, Modifier.Alt)
    )
    config.inputConfig.hotkeyConfig
      .bindingsFor(HotkeyAction.PreviousTab)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Tab,
      character = None,
      modifiers = Set(Modifier.Ctrl, Modifier.Shift)
    )
  }

  it should "load and write meta hotkey overrides using command-key aliases" in {
    val configFile = Files.createTempFile("serenity-config", ".conf")
    Files.writeString(
      configFile,
      """hotkey.command_palette = cmd+p
        |hotkey.file_search = command+shift+f
        |keymap.command_runner.submit = meta+enter
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.inputConfig.hotkeyConfig
      .bindingsFor(HotkeyAction.ToggleCommandRunner)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Character,
      character = Some('p'),
      modifiers = Set(Modifier.Meta)
    )
    config.inputConfig.hotkeyConfig
      .bindingsFor(HotkeyAction.FileSearch)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Character,
      character = Some('f'),
      modifiers = Set(Modifier.Meta, Modifier.Shift)
    )
    config.inputConfig.focusedKeymapConfig.commandRunner
      .bindingsFor(CommandRunnerKeyAction.Submit)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Enter,
      character = None,
      modifiers = Set(Modifier.Meta)
    )

    val written = ConfigManager.configToString(config)
    written should include("hotkey.command_palette = [\"meta+p\"]")
    written should include("hotkey.file_search = [\"meta+shift+f\"]")
    written should include("keymap.command_runner.submit = \"meta+enter\"")
  }

  it should "parse richer key trigger names for local keymap overrides" in {
    val configFile = Files.createTempFile("serenity-config", ".conf")
    Files.writeString(
      configFile,
      """keymap.editor.page_down = ctrl+pagedown
        |keymap.editor.extend_selection_right = shift+right
        |keymap.command_runner.submit = ctrl+enter
        |keymap.modal.dismiss = ctrl+escape
        |keymap.panel.activate = ctrl+enter
        |keymap.peek.accept = ctrl+enter
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.inputConfig.focusedKeymapConfig.editor
      .bindingsFor(EditorKeyAction.PageDown)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.PageDown,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
    config.inputConfig.focusedKeymapConfig.editor
      .bindingsFor(EditorKeyAction.ExtendSelectionRight)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.ArrowRight,
      character = None,
      modifiers = Set(Modifier.Shift)
    )
    config.inputConfig.focusedKeymapConfig.commandRunner
      .bindingsFor(CommandRunnerKeyAction.Submit)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Enter,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
    config.inputConfig.focusedKeymapConfig.modal
      .bindingsFor(ModalKeyAction.Dismiss)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Escape,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
    config.inputConfig.focusedKeymapConfig.panel
      .bindingsFor(PanelKeyAction.Activate)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Enter,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
    config.inputConfig.focusedKeymapConfig.peek
      .bindingsFor(PeekKeyAction.Accept)
      .head shouldBe com.serenity.config.HotkeyTrigger(
      keyType = InputKey.Enter,
      character = None,
      modifiers = Set(Modifier.Ctrl)
    )
  }

  it should "round-trip a legacy-format keymap config file across all five keymap groups" in {
    val configFile = Files.createTempFile("serenity-keymap-round-trip", ".conf")
    Files.writeString(
      configFile,
      """keymap.editor.move_left = alt+h
        |keymap.editor.page_down = ctrl+pagedown
        |keymap.command_runner.submit = ctrl+enter
        |keymap.command_runner.navigate_down = alt+j
        |keymap.modal.dismiss = ctrl+escape
        |keymap.modal.next_field = alt+n
        |keymap.panel.activate = ctrl+enter
        |keymap.panel.return_focus = alt+r
        |keymap.peek.accept = ctrl+enter
        |keymap.peek.dismiss = alt+d
        |""".stripMargin
    )

    val keymap = ConfigManagerTestSupport.loadConfig(Some(configFile.toString)).inputConfig.focusedKeymapConfig

    keymap.editor.bindingsFor(EditorKeyAction.MoveLeft).map(_.render) shouldBe List("alt+h")
    keymap.editor.bindingsFor(EditorKeyAction.PageDown).map(_.render) shouldBe List("ctrl+pagedown")
    keymap.commandRunner.bindingsFor(CommandRunnerKeyAction.Submit).map(_.render) shouldBe List("ctrl+enter")
    keymap.commandRunner.bindingsFor(CommandRunnerKeyAction.NavigateDown).map(_.render) shouldBe List("alt+j")
    keymap.modal.bindingsFor(ModalKeyAction.Dismiss).map(_.render) shouldBe List("ctrl+escape")
    keymap.modal.bindingsFor(ModalKeyAction.NextField).map(_.render) shouldBe List("alt+n")
    keymap.panel.bindingsFor(PanelKeyAction.Activate).map(_.render) shouldBe List("ctrl+enter")
    keymap.panel.bindingsFor(PanelKeyAction.ReturnFocus).map(_.render) shouldBe List("alt+r")
    keymap.peek.bindingsFor(PeekKeyAction.Accept).map(_.render) shouldBe List("ctrl+enter")
    keymap.peek.bindingsFor(PeekKeyAction.Dismiss).map(_.render) shouldBe List("alt+d")
  }

  it should "round-trip the focused keymap config through its JSON codec unchanged" in {
    import _root_.io.circe.syntax.*

    val customized = FocusedKeymapConfig()
      .withBinding(KeymapGroup.Editor)(EditorKeyAction.MoveLeft, "alt+h")
      .withBinding(KeymapGroup.CommandRunner)(CommandRunnerKeyAction.Submit, "meta+enter")
      .withBinding(KeymapGroup.Modal)(ModalKeyAction.Dismiss, "ctrl+escape")
      .withBinding(KeymapGroup.Panel)(PanelKeyAction.Activate, "ctrl+enter")
      .withBinding(KeymapGroup.Peek)(PeekKeyAction.Accept, "ctrl+enter")

    customized.asJson.as[FocusedKeymapConfig] shouldBe Right(customized)
  }

  it should "write default editor selection-extension keymap bindings" in {
    val written = ConfigManager.configToString(AppConfig.default)

    written should include("keymap.editor.extend_selection_left = \"shift+left\"")
    written should include("keymap.editor.extend_selection_right = \"shift+right\"")
    written should include("keymap.editor.extend_selection_up = \"shift+up\"")
    written should include("keymap.editor.extend_selection_down = \"shift+down\"")
  }

  it should "fall back to default editor key bindings when writing sparse keymap config" in {
    val config = AppConfig.default.withFocusedKeymapConfig(
      FocusedKeymapConfig(editor = KeymapGroupConfig[EditorKeyAction, EditorEvent](Map.empty))
    )
    val written = ConfigManager.configToString(config)

    written should include("keymap.editor.page_down = pagedown")
    written should include("keymap.editor.extend_selection_right = \"shift+right\"")
  }

  it should "serialize empty and custom key binding collections without crashing" in {
    val config = AppConfig.default
      .withHotkeyConfig(HotkeyConfig(Map.empty))
      .withFocusedKeymapConfig(FocusedKeymapConfig())

    noException should be thrownBy ConfigManager.configToString(config)
    ConfigManager.configToString(config) should include("keymap.command_runner.submit")
  }
