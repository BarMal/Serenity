package com.serenity

import com.serenity.keystroke.events.PeekInputEvent.OtherInput
import com.serenity.keystroke.translators.PeekOverlayTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PeekOverlayTranslatorSpec extends AnyFlatSpec with Matchers:

  private val translator = new PeekOverlayTranslator()

  "PeekOverlayTranslator" should "treat an unmodified character as other input" in {
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty)) shouldBe OtherInput
  }

  it should "treat a shift-only character as other input" in {
    translator.translate(
      KeyStrokeInfo(InputKey.Character, Some('A'), Set(Modifier.Shift))
    ) shouldBe OtherInput
  }

  it should "not treat a ctrl-modified character as other input" in {
    translator.translate(
      KeyStrokeInfo(InputKey.Character, Some('a'), Set(Modifier.Ctrl))
    ) should not be OtherInput
  }

  it should "not treat an alt-modified character as other input" in {
    translator.translate(
      KeyStrokeInfo(InputKey.Character, Some('a'), Set(Modifier.Alt))
    ) should not be OtherInput
  }
