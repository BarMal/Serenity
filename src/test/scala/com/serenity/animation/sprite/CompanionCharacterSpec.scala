package com.serenity.animation.sprite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompanionCharacterSpec extends AnyFlatSpec with Matchers:

  "CompanionCharacter.fromConfigKey" should "parse every value's own id" in {
    CompanionCharacter.values.foreach { character =>
      CompanionCharacter.fromConfigKey(character.id) shouldBe Some(character)
    }
  }

  it should "be case- and whitespace-insensitive" in {
    CompanionCharacter.fromConfigKey(" Pixel-Wizard ") shouldBe Some(CompanionCharacter.PixelWizard)
  }

  it should "reject an id no bundled character owns" in {
    CompanionCharacter.fromConfigKey("hocus-pocus") shouldBe None
  }

  "CompanionCharacter.default" should "be the bundled placeholder character" in {
    CompanionCharacter.default shouldBe CompanionCharacter.PixelWizard
  }

  "every CompanionCharacter" should "name a sprite sheet resource under /sprites" in {
    CompanionCharacter.values.foreach(_.sheetResourcePath should startWith("/sprites/"))
  }
