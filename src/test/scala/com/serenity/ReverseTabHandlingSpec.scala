package com.serenity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.serenity.keystroke.events.{PreviousTab, ReverseTabKey, TabKey}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

class ReverseTabHandlingSpec extends AnyFlatSpec with Matchers:

  behavior of "ReverseTab handling in TextEntryTranslator"

  it should "translate ReverseTab to ReverseTabKey event" in {
    val translator = new TextEntryTranslator()

    val result = translator.translate(KeyStrokeInfo(InputKey.ReverseTab, None, Set.empty))
    result.shouldBe(ReverseTabKey)
  }

  it should "handle ReverseTab without Ctrl modifier as ReverseTabKey" in {
    val translator = new TextEntryTranslator()

    translator.translate(KeyStrokeInfo(InputKey.ReverseTab, None, Set(Modifier.Shift))).shouldBe(ReverseTabKey)
    translator.translate(KeyStrokeInfo(InputKey.ReverseTab, None, Set(Modifier.Alt))).shouldBe(ReverseTabKey)
  }

  it should "handle ReverseTab with Ctrl modifier as PreviousTab" in {
    val translator = new TextEntryTranslator()

    val result = translator.translate(KeyStrokeInfo(InputKey.ReverseTab, None, Set(Modifier.Ctrl)))
    result shouldBe PreviousTab
  }

  it should "not interfere with regular Tab handling" in {
    val translator = new TextEntryTranslator()

    val result = translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set.empty))
    result.shouldBe(TabKey)
  }
