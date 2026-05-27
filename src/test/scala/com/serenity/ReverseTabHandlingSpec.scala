package com.serenity

import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.serenity.keystroke.events.{PreviousTab, ReverseTabKey, TabKey}
import com.serenity.keystroke.translators.TextEntryTranslator
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReverseTabHandlingSpec extends AnyFlatSpec with Matchers:

  behavior of "ReverseTab handling in TextEntryTranslator"

  it should "translate ReverseTab to ReverseTabKey event" in {
    // Given: A TextEntryTranslator
    val translator = new TextEntryTranslator()

    // When: ReverseTab key is pressed (usually Shift+Tab)
    val reverseTabKeyStroke = new KeyStroke(KeyType.ReverseTab, false, false, false)

    // Then: Should translate to ReverseTabKey event
    val result = translator.translate(reverseTabKeyStroke)
    result.shouldBe(ReverseTabKey)
  }

  it should "handle ReverseTab without Ctrl modifier as ReverseTabKey" in {
    // Given: A TextEntryTranslator
    val translator = new TextEntryTranslator()

    // When: ReverseTab with non-Ctrl modifiers
    val reverseTabWithShift = new KeyStroke(KeyType.ReverseTab, false, false, true)
    val reverseTabWithAlt = new KeyStroke(KeyType.ReverseTab, false, true, false)

    // Then: Should translate to ReverseTabKey event
    translator.translate(reverseTabWithShift).shouldBe(ReverseTabKey)
    translator.translate(reverseTabWithAlt).shouldBe(ReverseTabKey)
  }

  it should "handle ReverseTab with Ctrl modifier as PreviousTab" in {
    // Given: A TextEntryTranslator
    val translator = new TextEntryTranslator()

    // When: ReverseTab with Ctrl modifier (should be handled by hotkey converter for PreviousTab)
    val reverseTabWithCtrl = new KeyStroke(KeyType.ReverseTab, true, false, false)

    // Then: Should translate to PreviousTab event
    val result = translator.translate(reverseTabWithCtrl)
    result shouldBe PreviousTab
  }

  it should "not interfere with regular Tab handling" in {
    // Given: A TextEntryTranslator  
    val translator = new TextEntryTranslator()

    // When: Regular Tab key (without Ctrl)
    val tabKeyStroke = new KeyStroke(KeyType.Tab, false, false, false)

    // Then: Should still insert tab character
    val result = translator.translate(tabKeyStroke)
    result.shouldBe(TabKey)
  }
