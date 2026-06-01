package com.serenity

import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextEntryTranslatorCompositionSpec extends AnyFlatSpec with Matchers:

  private val translator = new TextEntryTranslator()

  "TextEntryTranslator" should "keep plain character input in the editor event family" in {
    val event = translator.translate(KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty))

    event shouldBe InsertChar('a')
    event.isInstanceOf[EditorEvent] shouldBe true
    event.isInstanceOf[TextInputEvent] shouldBe true
  }

  it should "prioritize tab hotkeys ahead of plain tab insertion" in {
    val ctrlTab       = translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set(Modifier.Ctrl)))
    val plainTab      = translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set.empty))
    val ctrlReverseTab = translator.translate(KeyStrokeInfo(InputKey.ReverseTab, None, Set(Modifier.Ctrl)))

    ctrlTab shouldBe NextTab
    plainTab shouldBe TabKey
    ctrlReverseTab shouldBe PreviousTab
  }

  it should "keep application hotkeys in the application event family" in {
    val openPalette = translator.translate(KeyStrokeInfo(InputKey.Character, Some('p'), Set(Modifier.Ctrl)))
    val quit        = translator.translate(KeyStrokeInfo(InputKey.EOF, None, Set.empty))

    openPalette shouldBe ToggleCommandRunner
    openPalette.isInstanceOf[AppEvent] shouldBe true

    quit shouldBe Quit
    quit.isInstanceOf[AppEvent] shouldBe true
  }

  it should "translate Ctrl+A into select-all in the editor event family" in {
    val selectAll = translator.translate(KeyStrokeInfo(InputKey.Character, Some('a'), Set(Modifier.Ctrl)))

    selectAll shouldBe SelectAll
    selectAll.isInstanceOf[EditorEvent] shouldBe true
  }
