package com.serenity

import com.serenity.keystroke.events.InsertChar
import com.serenity.keystroke.translators.TextCharacterConverters
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The GUI reports a Shift+Alt letter chord by its lowercase key so a hotkey can bind it; a chord nothing binds must
  * still type the capital it always did.
  */
class AltShiftLetterTypingSpec extends AnyFlatSpec with Matchers:

  private val convert = TextCharacterConverters.characterConverter.lift

  "An unbound Shift+Alt letter chord" should "type the capital letter" in {
    convert(KeyStrokeInfo(InputKey.Character, Some('x'), Set(Modifier.Alt, Modifier.Shift))) shouldBe
      Some(InsertChar('X'))
  }

  it should "leave a capital that is already typed alone" in {
    convert(KeyStrokeInfo(InputKey.Character, Some('X'), Set(Modifier.Alt, Modifier.Shift))) shouldBe
      Some(InsertChar('X'))
  }
