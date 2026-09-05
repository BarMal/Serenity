package com.serenity

import com.serenity.config.*
import com.serenity.keystroke.events.EditorEvent
import com.serenity.keystroke.{InputKey, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InputConfigSpec extends AnyFlatSpec with Matchers:

  "InputConfig" should "own hotkey and keymap dynamic prefixes" in
    ConfigKeySchema.dynamicPrefixes.should(
      contain allOf ("hotkey.", "keymap.")
    )

  it should "group hotkeys and focused keymaps under AppConfig" in {
    val hotkeys = HotkeyConfig
      .forOs("Linux")
      .withBinding(
        HotkeyAction.ToggleCommandRunner,
        HotkeyTrigger(
          keyType = InputKey.Character,
          character = Some('k'),
          modifiers = Set(Modifier.Ctrl)
        )
      )
    val keymaps =
      FocusedKeymapConfig().withBinding(KeymapGroup.CommandRunner)(CommandRunnerKeyAction.Submit, "meta+enter")

    val config = AppConfig.default
      .withHotkeyConfig(hotkeys)
      .withFocusedKeymapConfig(keymaps)

    config.inputConfig.hotkeyConfig.shouldBe(hotkeys)
    config.inputConfig.focusedKeymapConfig.shouldBe(keymaps)
  }

  it should "reject a hotkey already assigned to another action" in {
    val config = HotkeyConfig.forOs("Linux").withBinding(HotkeyAction.ToggleCommandRunner, "ctrl+shift+f")

    config.bindingsFor(HotkeyAction.ToggleCommandRunner).head.render.shouldBe("ctrl+p")
    config.bindingsFor(HotkeyAction.FileSearch).head.render.shouldBe("ctrl+shift+f")
  }

  it should "reject a focused keymap binding already assigned to another action" in {
    val config =
      KeymapGroupConfig.defaults[EditorKeyAction, EditorEvent].withBinding(EditorKeyAction.PageDown, "pageup")

    config.bindingsFor(EditorKeyAction.PageDown).head.render.shouldBe("pagedown")
    config.bindingsFor(EditorKeyAction.PageUp).head.render.shouldBe("pageup")
  }
