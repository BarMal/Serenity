package com.serenity

import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextEntryTranslatorCompositionSpec extends AnyFlatSpec with Matchers:

  private val translator = new TextEntryTranslator()

  "TextEntryTranslator" should "keep plain character input in the editor event family" in {
    val event = translator.translate(new KeyStroke('a', false, false, false))

    event shouldBe InsertChar('a')
    event.isInstanceOf[EditorEvent] shouldBe true
    event.isInstanceOf[TextInputEvent] shouldBe true
  }

  it should "prioritize tab hotkeys ahead of plain tab insertion" in {
    val ctrlTab = translator.translate(new KeyStroke(KeyType.Tab, true, false, false))
    val plainTab = translator.translate(new KeyStroke(KeyType.Tab))
    val ctrlReverseTab = translator.translate(new KeyStroke(KeyType.ReverseTab, true, false, false))

    ctrlTab shouldBe NextTab
    plainTab shouldBe TabKey
    ctrlReverseTab shouldBe PreviousTab
  }

  it should "keep application hotkeys in the application event family" in {
    val openPalette = translator.translate(new KeyStroke('p', true, false, false))
    val quit = translator.translate(new KeyStroke(KeyType.EOF))

    openPalette shouldBe ToggleCommandRunner
    openPalette.isInstanceOf[AppEvent] shouldBe true

    quit shouldBe Quit
    quit.isInstanceOf[AppEvent] shouldBe true
  }
