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
