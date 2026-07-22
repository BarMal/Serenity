package com.serenity

import com.serenity.config.{AppConfig, HotkeyConfig}
import com.serenity.keystroke.events.{PreviousTab, ReverseTabKey, TabKey}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReverseTabHandlingSpec extends AnyFlatSpec with Matchers:

  private val linuxConfig = AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs("Linux"))

  behavior of "ReverseTab handling in TextEntryTranslator"

  it should "translate ReverseTab to ReverseTabKey event" in {
    val translator = new TextEntryTranslator(linuxConfig)

    val result = translator.translate(KeyStrokeInfo(InputKey.ReverseTab, None, Set.empty))
    result.shouldBe(ReverseTabKey)
  }

  it should "handle ReverseTab without Ctrl modifier as ReverseTabKey" in {
    val translator = new TextEntryTranslator(linuxConfig)

    translator.translate(KeyStrokeInfo(InputKey.ReverseTab, None, Set(Modifier.Shift))).shouldBe(ReverseTabKey)
    translator.translate(KeyStrokeInfo(InputKey.ReverseTab, None, Set(Modifier.Alt))).shouldBe(ReverseTabKey)
  }

  it should "handle ReverseTab with Ctrl modifier as PreviousTab" in {
    val translator = new TextEntryTranslator(linuxConfig)

    val result = translator.translate(KeyStrokeInfo(InputKey.ReverseTab, None, Set(Modifier.Ctrl)))
    result shouldBe PreviousTab
  }

  it should "not interfere with regular Tab handling" in {
    val translator = new TextEntryTranslator(linuxConfig)

    val result = translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set.empty))
    result.shouldBe(TabKey)
  }
