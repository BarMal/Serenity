package com.serenity

import com.serenity.keystroke.Modifier
import com.serenity.keystroke.translators.TextCharacterConverters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PrintableCharModifierSpec extends AnyFlatSpec with Matchers:

  "isPlainOrShiftOnly" should "accept no modifiers" in {
    TextCharacterConverters.isPlainOrShiftOnly(Set.empty) shouldBe true
  }

  it should "accept shift alone" in {
    TextCharacterConverters.isPlainOrShiftOnly(Set(Modifier.Shift)) shouldBe true
  }

  it should "reject ctrl" in {
    TextCharacterConverters.isPlainOrShiftOnly(Set(Modifier.Ctrl)) shouldBe false
  }

  it should "reject alt" in {
    TextCharacterConverters.isPlainOrShiftOnly(Set(Modifier.Alt)) shouldBe false
  }

  it should "reject meta" in {
    TextCharacterConverters.isPlainOrShiftOnly(Set(Modifier.Meta)) shouldBe false
  }

  it should "reject shift combined with another modifier" in {
    TextCharacterConverters.isPlainOrShiftOnly(Set(Modifier.Shift, Modifier.Ctrl)) shouldBe false
  }
