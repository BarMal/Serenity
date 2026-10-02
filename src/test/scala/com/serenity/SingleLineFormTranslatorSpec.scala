package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.SingleLineFormTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SingleLineFormTranslatorSpec extends AnyFlatSpec with Matchers:

  private val translator = new SingleLineFormTranslator()

  "SingleLineFormTranslator" should "treat enter as submit rather than newline" in {
    translator.translate(KeyStrokeInfo(InputKey.Enter, None, Set.empty)) shouldBe ModalSubmit
  }

  it should "use field navigation semantics for tab and reverse tab" in {
    translator.translate(KeyStrokeInfo(InputKey.Tab, None, Set.empty)) shouldBe ModalNextField
    translator.translate(KeyStrokeInfo(InputKey.ReverseTab, None, Set.empty)) shouldBe ModalPreviousField
  }

  it should "preserve single-character entry and dismissal semantics" in {
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty)) shouldBe ModalInsertChar('a')
    translator.translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty)) shouldBe ModalDismiss
  }

  it should "insert a shift-only character" in {
    translator.translate(
      KeyStrokeInfo(InputKey.Character, Some('A'), Set(Modifier.Shift))
    ) shouldBe ModalInsertChar('A')
  }

  it should "not insert a ctrl-modified character" in {
    translator.translate(
      KeyStrokeInfo(InputKey.Character, Some('a'), Set(Modifier.Ctrl))
    ) should not be a[ModalInsertChar]
  }

  it should "not insert an alt-modified character" in {
    translator.translate(
      KeyStrokeInfo(InputKey.Character, Some('a'), Set(Modifier.Alt))
    ) should not be a[ModalInsertChar]
  }

  it should "read Home and End as line ends, Ctrl+Home and Ctrl+End as list ends, and PageUp and PageDown as pages" in {
    val ctrl = Set(Modifier.Ctrl)

    translator.translate(KeyStrokeInfo(InputKey.Home, None, Set.empty)) shouldBe ModalLineStart
    translator.translate(KeyStrokeInfo(InputKey.End, None, Set.empty)) shouldBe ModalLineEnd
    translator.translate(KeyStrokeInfo(InputKey.Home, None, ctrl)) shouldBe ModalFirst
    translator.translate(KeyStrokeInfo(InputKey.End, None, ctrl)) shouldBe ModalLast
    translator.translate(KeyStrokeInfo(InputKey.PageUp, None, Set.empty)) shouldBe ModalPage(-1)
    translator.translate(KeyStrokeInfo(InputKey.PageDown, None, Set.empty)) shouldBe ModalPage(1)
  }
