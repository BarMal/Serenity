package com.serenity

import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Test keyboard shortcut translation for tab management functionality */
class TabKeyboardShortcutsSpec extends AnyFlatSpec with Matchers:

  val translator = new TextEntryTranslator()

  behavior of "Tab Management Keyboard Shortcuts"

  it should "translate Ctrl+T to NewTab event" in {
    val keyStroke = new KeyStroke('t', true, false, false)
    val event = translator.translate(keyStroke)
    
    event.shouldBe(NewTab)
  }

  it should "translate Ctrl+W to CloseTab event" in {
    val keyStroke = new KeyStroke('w', true, false, false)
    val event = translator.translate(keyStroke)
    
    event.shouldBe(CloseTab)
  }

  it should "translate Ctrl+Tab to NextTab event" in {
    val keyStroke = new KeyStroke(KeyType.Tab, true, false, false)
    val event = translator.translate(keyStroke)
    
    event.shouldBe(NextTab)
  }

  it should "translate Ctrl+Shift+Tab to PreviousTab event" in {
    val keyStroke = new KeyStroke(KeyType.Tab, true, false, true)
    val event = translator.translate(keyStroke)
    
    event.shouldBe(PreviousTab)
  }

  it should "not insert tab character when Ctrl is pressed" in {
    val keyStrokeWithCtrl = new KeyStroke(KeyType.Tab, true, false, false)
    val eventWithCtrl = translator.translate(keyStrokeWithCtrl)
    
    // Should NOT be InsertChar('\t') when Ctrl is pressed
    eventWithCtrl.should(not).be(InsertChar('\t'))
    eventWithCtrl.shouldBe(NextTab)
  }

  it should "still insert tab character when Ctrl is not pressed" in {
    val keyStrokeWithoutCtrl = new KeyStroke(KeyType.Tab, false, false, false)
    val eventWithoutCtrl = translator.translate(keyStrokeWithoutCtrl)
    
    eventWithoutCtrl.shouldBe(InsertChar('\t'))
  }

  it should "handle case sensitivity correctly" in {
    // Lowercase 't' with Ctrl should work
    val lowerT = new KeyStroke('t', true, false, false)
    translator.translate(lowerT).shouldBe(NewTab)
    
    // Uppercase 'T' should NOT trigger NewTab (would require Shift+Ctrl)
    val upperT = new KeyStroke('T', true, false, false)
    translator.translate(upperT).should(not).be(NewTab)
    
    // Same for 'w'
    val lowerW = new KeyStroke('w', true, false, false)
    translator.translate(lowerW).shouldBe(CloseTab)
    
    val upperW = new KeyStroke('W', true, false, false)
    translator.translate(upperW).should(not).be(CloseTab)
  }

  it should "require Ctrl modifier for all tab shortcuts" in {
    // Plain 't' without Ctrl should be character insertion
    val plainT = new KeyStroke('t', false, false, false)
    translator.translate(plainT).shouldBe(InsertChar('t'))
    
    // Plain 'w' without Ctrl should be character insertion  
    val plainW = new KeyStroke('w', false, false, false)
    translator.translate(plainW).shouldBe(InsertChar('w'))
    
    // Plain Tab without Ctrl should be tab insertion
    val plainTab = new KeyStroke(KeyType.Tab, false, false, false)
    translator.translate(plainTab).shouldBe(InsertChar('\t'))
  }