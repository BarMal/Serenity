package com.serenity.animation.sprite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Migrated from the retired `WindowSitterFrameSelectionSpec` (issue #934) -- the pure per-cycle frame-selection math
  * is unchanged, only relocated and renamed to serve the companion sprite panel's typing-reactive frame stepping (issue
  * #934 v2) instead of a title-bar decoration.
  */
class SpriteFrameSelectionSpec extends AnyFlatSpec with Matchers:

  "SpriteFrameSelection.indices" should "play every frame forward for Cycle" in {
    SpriteFrameSelection.indices(SpriteFrameCycle.Cycle, 4) shouldBe Vector(0, 1, 2, 3)
  }

  it should "hold the last frame and ping-pong across the rest for Pulse" in {
    SpriteFrameSelection.indices(SpriteFrameCycle.Pulse, 4) shouldBe Vector(0, 1, 2)
  }

  it should "toggle only the first and last frame for Blink" in {
    SpriteFrameSelection.indices(SpriteFrameCycle.Blink, 4) shouldBe Vector(0, 3)
  }

  it should "never be empty, even for a degenerate 0-or-1-frame sheet" in
    SpriteFrameCycle.values.foreach { cycle =>
      SpriteFrameSelection.indices(cycle, 0) should not be empty
      SpriteFrameSelection.indices(cycle, 1) should not be empty
    }

  "SpriteFrameSelection.next" should "wrap Cycle back to the first frame after the last" in {
    SpriteFrameSelection.next(SpriteFrameCycle.Cycle, 4, currentIndex = 3, ascending = true) shouldBe
      SpriteFrameSelection.FrameStep(0, true)
  }

  it should "pulse back through intermediate frames instead of wrapping to rest" in {
    val steps = Iterator
      .iterate(SpriteFrameSelection.FrameStep(0, true)) { step =>
        SpriteFrameSelection.next(SpriteFrameCycle.Pulse, 4, step.index, step.ascending)
      }
      .take(5)
      .map(_.index)
      .toVector

    steps shouldBe Vector(0, 1, 2, 1, 0)
  }

  it should "toggle Blink between the first and last selected position" in {
    val first  = SpriteFrameSelection.next(SpriteFrameCycle.Blink, 4, currentIndex = 0, ascending = true)
    val second = SpriteFrameSelection.next(SpriteFrameCycle.Blink, 4, currentIndex = first.index, ascending = true)

    first shouldBe SpriteFrameSelection.FrameStep(1, true)
    second shouldBe SpriteFrameSelection.FrameStep(0, true)
  }

  "SpriteFrameCycle.fromConfigKey" should "parse the three configured keys" in {
    SpriteFrameCycle.fromConfigKey("cycle") shouldBe Some(SpriteFrameCycle.Cycle)
    SpriteFrameCycle.fromConfigKey("pulse") shouldBe Some(SpriteFrameCycle.Pulse)
    SpriteFrameCycle.fromConfigKey("blink") shouldBe Some(SpriteFrameCycle.Blink)
    SpriteFrameCycle.fromConfigKey("nonsense") shouldBe None
  }
