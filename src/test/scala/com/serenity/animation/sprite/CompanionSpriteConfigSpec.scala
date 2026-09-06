package com.serenity.animation.sprite

import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompanionSpriteConfigSpec extends AnyFlatSpec with Matchers:

  "CompanionSpriteConfig.default" should "be disabled, with the placeholder character" in {
    val config = CompanionSpriteConfig.default
    config.enabled shouldBe false
    config.character shouldBe CompanionCharacter.default
  }

  "CompanionSpriteConfig.normalized" should "clamp size into the valid range" in {
    CompanionSpriteConfig(size = 0).normalized.size shouldBe CompanionSpriteConfig.MinSize
    CompanionSpriteConfig(size = 999).normalized.size shouldBe CompanionSpriteConfig.MaxSize
    CompanionSpriteConfig(size = CompanionSpriteConfig.MinSize + 1).normalized.size shouldBe
      CompanionSpriteConfig.MinSize + 1
  }

  it should "leave the position and character untouched" in {
    val config = CompanionSpriteConfig(position = PanelPosition.Bottom, character = CompanionCharacter.PixelWizard)
    config.normalized.position shouldBe PanelPosition.Bottom
    config.normalized.character shouldBe CompanionCharacter.PixelWizard
  }
