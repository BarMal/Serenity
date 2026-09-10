package com.serenity

import com.serenity.config.*
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.EditorInputTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorInputTranslatorSpec extends AnyFlatSpec with Matchers:

  private val translator = new EditorInputTranslator()

  "EditorInputTranslator" should "translate the default arrow keys into cursor movement" in {
    translator.translate(KeyStrokeInfo(InputKey.ArrowLeft, None, Set.empty)) shouldBe MoveLeft
    translator.translate(KeyStrokeInfo(InputKey.ArrowRight, None, Set.empty)) shouldBe MoveRight
  }

  it should "translate a plain character into insertion" in {
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty)) shouldBe InsertChar('a')
  }

  it should "translate Tab and Enter into their text-entry events" in {
    translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set.empty)) shouldBe TabKey
    translator.translate(KeyStrokeInfo(InputKey.Enter, None, Set.empty)) shouldBe NewLine
  }

  it should "leave a ctrl-tab stroke undefined here, for a higher-priority hotkey to claim" in {
    translator
      .translate(KeyStrokeInfo(InputKey.Tab, None, Set(Modifier.Ctrl)))
      .isInstanceOf[UnhandledEvent[?]] shouldBe true
  }

  it should "respect a configured editor keymap override" in {
    val customTranslator = new EditorInputTranslator(
      AppConfig.default.withKeymapBinding(KeymapGroup.Editor)(EditorKeyAction.PageDown, "ctrl+j")
    )

    customTranslator.translate(KeyStrokeInfo(InputKey.Character, Some('j'), Set(Modifier.Ctrl))) shouldBe PageDown
    customTranslator
      .translate(KeyStrokeInfo(InputKey.PageDown, None, Set.empty))
      .isInstanceOf[UnhandledEvent[?]] shouldBe true
  }
