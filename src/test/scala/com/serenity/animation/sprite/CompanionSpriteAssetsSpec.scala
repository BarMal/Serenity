package com.serenity.animation.sprite

import java.awt.image.BufferedImage

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompanionSpriteAssetsSpec extends AnyFlatSpec with Matchers:

  "CompanionSpriteAssets.loadSheet" should "load the bundled placeholder sheet from the classpath" in {
    val sheet = CompanionSpriteAssets.loadSheet(CompanionCharacter.PixelWizard)
    sheet shouldBe defined
    sheet.get.getWidth shouldBe CompanionSpriteAssets.IdleFrameSize * CompanionSpriteAssets.IdleFrameCount
    sheet.get.getHeight shouldBe CompanionSpriteAssets.IdleFrameSize
  }

  "CompanionSpriteAssets.loadFrames" should "slice the idle clip into the declared frame count" in {
    val frames = CompanionSpriteAssets.loadFrames(CompanionCharacter.PixelWizard)
    frames.keySet shouldBe Set(CompanionSpriteAssets.IdleClipName)
    frames(CompanionSpriteAssets.IdleClipName) should have length CompanionSpriteAssets.IdleFrameCount
    frames(CompanionSpriteAssets.IdleClipName).foreach { frame =>
      frame.getWidth shouldBe CompanionSpriteAssets.IdleFrameSize
      frame.getHeight shouldBe CompanionSpriteAssets.IdleFrameSize
    }
  }

  "CompanionSpriteFrames.currentFrame" should "return the idle frame at the state's current frame index" in {
    val frames = CompanionSpriteAssets.loadFrames(CompanionCharacter.PixelWizard)
    val state  = CompanionSpriteState(frameIndex = 2, frameCounts = Map(CompanionSpriteAction.Idle -> 4))

    CompanionSpriteFrames.currentFrame(frames, state) shouldBe Some(frames(CompanionSpriteAssets.IdleClipName)(2))
  }

  it should "fall back to the idle clip for an action the loaded sheet has no clip for" in {
    val frames = CompanionSpriteAssets.loadFrames(CompanionCharacter.PixelWizard)
    val state = CompanionSpriteState(
      action = CompanionSpriteAction.Walk,
      frameIndex = 1,
      frameCounts = Map(CompanionSpriteAction.Idle -> 4, CompanionSpriteAction.Walk -> 3)
    )

    CompanionSpriteFrames.currentFrame(frames, state) shouldBe Some(frames(CompanionSpriteAssets.IdleClipName)(1))
  }

  it should "return None when no sheet could be loaded at all" in {
    CompanionSpriteFrames.currentFrame(Map.empty[String, Vector[BufferedImage]], CompanionSpriteState()) shouldBe None
  }
