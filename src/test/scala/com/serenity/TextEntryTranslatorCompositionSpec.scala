package com.serenity

import com.serenity.config.*
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.{LocalKeymapConverters, TextEntryTranslator, TextHotkeyConverters}
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextEntryTranslatorCompositionSpec extends AnyFlatSpec with Matchers:

  private val linuxConfig = AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs("Linux"))
  private val translator  = new TextEntryTranslator(linuxConfig)

  "TextEntryTranslator" should "keep plain character input in the editor event family" in {
    val event = translator.translate(KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty))

    event shouldBe InsertChar('a')
    event.isInstanceOf[EditorEvent] shouldBe true
    event.isInstanceOf[TextInputEvent] shouldBe true
  }

  it should "treat printable Option-produced characters as text entry on macOS" in {
    val event = translator.translate(KeyStrokeInfo(InputKey.Character, Some('#'), Set(Modifier.Alt)))

    event shouldBe InsertChar('#')
    event.isInstanceOf[EditorEvent] shouldBe true
    event.isInstanceOf[TextInputEvent] shouldBe true
  }

  it should "prioritize tab hotkeys ahead of plain tab insertion" in {
    val ctrlTab        = translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set(Modifier.Ctrl)))
    val plainTab       = translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set.empty))
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

  it should "translate the save hotkey into a file-save event" in {
    val save = translator.translate(KeyStrokeInfo(InputKey.Character, Some('s'), Set(Modifier.Ctrl)))

    save shouldBe SaveFile
    save.isInstanceOf[FileEvent] shouldBe true
  }

  it should "translate Ctrl+A into select-all in the editor event family" in {
    val selectAll = translator.translate(KeyStrokeInfo(InputKey.Character, Some('a'), Set(Modifier.Ctrl)))

    selectAll shouldBe SelectAll
    selectAll.isInstanceOf[EditorEvent] shouldBe true
  }

  it should "respect configured global hotkey overrides" in {
    val customConfig = AppConfig.default.withHotkeyOverride(
      HotkeyAction.ToggleCommandRunner,
      "ctrl+k"
    )
    val customTranslator = new TextEntryTranslator(customConfig)

    customTranslator.translate(
      KeyStrokeInfo(InputKey.Character, Some('k'), Set(Modifier.Ctrl))
    ) shouldBe ToggleCommandRunner
    customTranslator
      .translate(KeyStrokeInfo(InputKey.Character, Some('p'), Set(Modifier.Ctrl)))
      .isInstanceOf[
        UnhandledEvent[?]
      ] shouldBe true
  }

  it should "respect configured meta global hotkey overrides" in {
    val customConfig = AppConfig.default.withHotkeyOverride(
      HotkeyAction.ToggleCommandRunner,
      "cmd+p"
    )
    val customTranslator = new TextEntryTranslator(customConfig)

    customTranslator.translate(
      KeyStrokeInfo(InputKey.Character, Some('p'), Set(Modifier.Meta))
    ) shouldBe ToggleCommandRunner
    customTranslator
      .translate(KeyStrokeInfo(InputKey.Character, Some('p'), Set(Modifier.Ctrl)))
      .isInstanceOf[
        UnhandledEvent[?]
      ] shouldBe true
  }

  it should "respect configured editor-local keymap overrides" in {
    val customConfig = AppConfig.default.withEditorKeyOverride(
      EditorKeyAction.PageDown,
      "ctrl+j"
    )
    val customTranslator = new TextEntryTranslator(customConfig)

    customTranslator.translate(KeyStrokeInfo(InputKey.Character, Some('j'), Set(Modifier.Ctrl))) shouldBe PageDown
    customTranslator
      .translate(KeyStrokeInfo(InputKey.PageDown, None, Set.empty))
      .isInstanceOf[
        UnhandledEvent[?]
      ] shouldBe true
  }

  it should "leave unmatched local keymap partial functions undefined" in {
    val converter = LocalKeymapConverters.converter(
      Map(EditorKeyAction.PageDown -> List(com.serenity.config.HotkeyTrigger(InputKey.PageDown, None, Set.empty)))
    )
    val unmatched = KeyStrokeInfo(InputKey.F1, None, Set.empty)

    converter.isDefinedAt(unmatched) shouldBe false
    converter.applyOrElse[KeyStrokeInfo, Any](unmatched, _ => "fallback") shouldBe "fallback"
    a[MatchError] should be thrownBy converter(unmatched)
  }

  it should "leave unmatched text hotkey partial functions undefined" in {
    val converter = TextHotkeyConverters.hotkeyConverter()
    val unmatched = KeyStrokeInfo(InputKey.F1, None, Set.empty)

    converter.isDefinedAt(unmatched) shouldBe false
    converter.applyOrElse[KeyStrokeInfo, Any](unmatched, _ => "fallback") shouldBe "fallback"
    a[MatchError] should be thrownBy converter(unmatched)
  }
