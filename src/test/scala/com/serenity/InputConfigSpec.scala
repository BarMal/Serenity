package com.serenity

import com.serenity.config.*
import com.serenity.keystroke.{InputKey, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InputConfigSpec extends AnyFlatSpec with Matchers:

  "InputConfig" should "own hotkey and keymap dynamic prefixes" in
    InputConfig.Schema.dynamicPrefixes.shouldBe(
      List(
        "hotkey.",
        "keymap.editor.",
        "keymap.command_runner.",
        "keymap.modal.",
        "keymap.panel.",
        "keymap.peek."
      )
    )

  it should "group hotkeys and focused keymaps under AppConfig" in {
    val hotkeys = HotkeyConfig().withBinding(
      HotkeyAction.ToggleCommandRunner,
      HotkeyTrigger(
        keyType = InputKey.Character,
        character = Some('k'),
        modifiers = Set(Modifier.Ctrl)
      )
    )
    val keymaps = FocusedKeymapConfig().withCommandRunnerBinding(CommandRunnerKeyAction.Submit, "meta+enter")

    val config = AppConfig.default
      .withHotkeyConfig(hotkeys)
      .withFocusedKeymapConfig(keymaps)

    config.inputConfig.hotkeyConfig.shouldBe(hotkeys)
    config.inputConfig.focusedKeymapConfig.shouldBe(keymaps)
  }

  it should "reject a hotkey already assigned to another action" in {
    val config = HotkeyConfig().withBinding(HotkeyAction.ToggleCommandRunner, "ctrl+shift+f")

    config.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render.shouldBe("ctrl+p")
    config.bindingsFor(HotkeyAction.FileSearch).head.render.shouldBe("ctrl+shift+f")
  }

  it should "reject a focused keymap binding already assigned to another action" in {
    val config = EditorKeymapConfig().withBinding(EditorKeyAction.PageDown, "pageup")

    config.bindingsFor(EditorKeyAction.PageDown).head.render.shouldBe("pagedown")
    config.bindingsFor(EditorKeyAction.PageUp).head.render.shouldBe("pageup")
  }
