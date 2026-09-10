package com.serenity

import com.serenity.config.*
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.GlobalHotkeyTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GlobalHotkeyTranslatorSpec extends AnyFlatSpec with Matchers:

  private val linuxConfig = AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs("Linux"))
  private val translator  = new GlobalHotkeyTranslator(linuxConfig)

  "GlobalHotkeyTranslator" should "dispatch the default save hotkey" in {
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('s'), Set(Modifier.Ctrl))) shouldBe SaveFile
  }

  it should "dispatch the default quit hotkey" in {
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('q'), Set(Modifier.Ctrl))) shouldBe Quit
  }

  it should "leave an unmodified character unhandled" in {
    translator
      .translate(KeyStrokeInfo(InputKey.Character, Some('s'), Set.empty))
      .isInstanceOf[UnhandledEvent[?]] shouldBe true
  }

  it should "respect a configured hotkey override" in {
    val customTranslator =
      new GlobalHotkeyTranslator(linuxConfig.withHotkeyOverride(HotkeyAction.ToggleCommandRunner, "ctrl+k"))

    customTranslator.translate(
      KeyStrokeInfo(InputKey.Character, Some('k'), Set(Modifier.Ctrl))
    ) shouldBe ToggleCommandRunner
    customTranslator
      .translate(KeyStrokeInfo(InputKey.Character, Some('p'), Set(Modifier.Ctrl)))
      .isInstanceOf[UnhandledEvent[?]] shouldBe true
  }

  // F2 stays genuinely unbound by default (see TextEntryTranslatorCompositionSpec), so this holds regardless of the
  // host OS the default AppConfig resolves hotkeys for.
  it should "fall back to AppConfig.default when constructed without one" in {
    new GlobalHotkeyTranslator()
      .translate(KeyStrokeInfo(InputKey.F2, None, Set.empty))
      .isInstanceOf[UnhandledEvent[?]] shouldBe true
  }
