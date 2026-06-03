package com.serenity

import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.SingleLineFormTranslator
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
