package com.serenity

import com.serenity.config.*
import com.serenity.keystroke.events.*
import com.serenity.keystroke.events.PanelInputEvent.ReturnFocus
import com.serenity.keystroke.translators.PinnedPanelTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PinnedPanelTranslatorSpec extends AnyFlatSpec with Matchers:

  private val translator = new PinnedPanelTranslator()

  "PinnedPanelTranslator" should "translate the default arrow keys into panel navigation" in {
    translator.translate(KeyStrokeInfo(InputKey.ArrowUp, None, Set.empty)) shouldBe
      PanelInputEvent.Navigate(Direction.Up)
    translator.translate(KeyStrokeInfo(InputKey.ArrowDown, None, Set.empty)) shouldBe
      PanelInputEvent.Navigate(Direction.Down)
  }

  it should "translate Enter into panel activation" in {
    translator.translate(KeyStrokeInfo(InputKey.Enter, None, Set.empty)) shouldBe PanelInputEvent.Activate
  }

  it should "return focus on an unmodified character" in {
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('x'), Set.empty)) shouldBe ReturnFocus
  }

  it should "return focus on a shift-only character" in {
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('X'), Set(Modifier.Shift))) shouldBe ReturnFocus
  }

  it should "dismiss the panel on Escape, apart from the keys that hand focus back to the editor" in {
    translator.translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty)) shouldBe PanelInputEvent.Dismiss
    List(InputKey.Backspace, InputKey.Delete, InputKey.Tab, InputKey.ReverseTab).foreach { key =>
      translator.translate(KeyStrokeInfo(key, None, Set.empty)) shouldBe ReturnFocus
    }
  }

  it should "still dismiss on Escape when a session saved before the split also binds it to return focus" in {
    val legacy = AppConfig.default.withFocusedKeymapConfig(
      FocusedKeymapConfig(panel =
        KeymapGroupConfig(
          PanelKeyAction.defaultBindings + (PanelKeyAction.ReturnFocus -> List(
            HotkeyTrigger(InputKey.Backspace, None, Set.empty),
            HotkeyTrigger(InputKey.Escape, None, Set.empty)
          ))
        )
      )
    )

    new PinnedPanelTranslator(legacy).translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty)) shouldBe
      PanelInputEvent.Dismiss
  }

  it should "respect a configured dismiss binding" in {
    val customTranslator = new PinnedPanelTranslator(
      AppConfig.default.withKeymapBinding(KeymapGroup.Panel)(PanelKeyAction.Dismiss, "ctrl+q")
    )

    customTranslator.translate(KeyStrokeInfo(InputKey.Character, Some('q'), Set(Modifier.Ctrl))) shouldBe
      PanelInputEvent.Dismiss
  }

  it should "leave a ctrl-modified character unhandled" in {
    translator
      .translate(KeyStrokeInfo(InputKey.Character, Some('x'), Set(Modifier.Ctrl)))
      .isInstanceOf[UnhandledEvent[?]] shouldBe true
  }

  it should "respect a configured panel keymap override" in {
    val customTranslator = new PinnedPanelTranslator(
      AppConfig.default.withKeymapBinding(KeymapGroup.Panel)(PanelKeyAction.Activate, "ctrl+enter")
    )

    customTranslator
      .translate(KeyStrokeInfo(InputKey.Enter, None, Set.empty))
      .isInstanceOf[UnhandledEvent[?]] shouldBe true
    customTranslator.translate(
      KeyStrokeInfo(InputKey.Enter, None, Set(Modifier.Ctrl))
    ) shouldBe PanelInputEvent.Activate
  }
