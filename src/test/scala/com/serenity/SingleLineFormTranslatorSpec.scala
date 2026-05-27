package com.serenity

import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.SingleLineFormTranslator
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SingleLineFormTranslatorSpec extends AnyFlatSpec with Matchers:

  private val translator = new SingleLineFormTranslator()

  "SingleLineFormTranslator" should "treat enter as submit rather than newline" in {
    translator.translate(new KeyStroke(KeyType.Enter)) shouldBe ModalSubmit
  }

  it should "use field navigation semantics for tab and reverse tab" in {
    translator.translate(new KeyStroke(KeyType.Tab)) shouldBe ModalNextField
    translator.translate(new KeyStroke(KeyType.ReverseTab)) shouldBe ModalPreviousField
  }

  it should "preserve single-character entry and dismissal semantics" in {
    translator.translate(new KeyStroke('a', false, false, false)) shouldBe ModalInsertChar('a')
    translator.translate(new KeyStroke(KeyType.Escape)) shouldBe ModalDismiss
  }
