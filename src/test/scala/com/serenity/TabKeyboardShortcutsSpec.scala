package com.serenity

import com.serenity.config.{AppConfig, HotkeyConfig}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TabKeyboardShortcutsSpec extends AnyFlatSpec with Matchers:

  private val translator = new TextEntryTranslator(AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs("Linux")))

  behavior of "Tab Management Keyboard Shortcuts"

  it should "translate Ctrl+T to NewTab event" in {
    val event = translator.translate(KeyStrokeInfo(InputKey.Character, Some('t'), Set(Modifier.Ctrl)))
    event.shouldBe(NewTab)
  }

  it should "translate Ctrl+W to CloseTab event" in {
    val event = translator.translate(KeyStrokeInfo(InputKey.Character, Some('w'), Set(Modifier.Ctrl)))
    event.shouldBe(CloseTab)
  }

  it should "translate Ctrl+Tab to NextTab event" in {
    val event = translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set(Modifier.Ctrl)))
    event.shouldBe(NextTab)
  }

  it should "translate Ctrl+Shift+Tab to PreviousTab event" in {
    val event = translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set(Modifier.Ctrl, Modifier.Shift)))
    event.shouldBe(PreviousTab)
  }

  it should "not insert tab character when Ctrl is pressed" in {
    val eventWithCtrl = translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set(Modifier.Ctrl)))
    eventWithCtrl.should(not).be(InsertChar('\t'))
    eventWithCtrl.shouldBe(NextTab)
  }

  it should "still insert tab character when Ctrl is not pressed" in {
    val eventWithoutCtrl = translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set.empty))
    eventWithoutCtrl.shouldBe(TabKey)
  }

  it should "handle case sensitivity correctly" in {
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('t'), Set(Modifier.Ctrl))).shouldBe(NewTab)
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('T'), Set(Modifier.Ctrl))).should(not).be(NewTab)

    translator.translate(KeyStrokeInfo(InputKey.Character, Some('w'), Set(Modifier.Ctrl))).shouldBe(CloseTab)
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('W'), Set(Modifier.Ctrl))).should(not).be(CloseTab)
  }

  it should "require Ctrl modifier for all tab shortcuts" in {
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('t'), Set.empty)).shouldBe(InsertChar('t'))
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('w'), Set.empty)).shouldBe(InsertChar('w'))
    translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set.empty)).shouldBe(TabKey)
  }
