package com.serenity.animation.sprite

import scala.util.Random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `tick` is the render-loop entry point: `advance` unthrottled, or throttled to half rate at the "Reduced" visual
  * flair tier (`VisualFlairLevel.Reduced` lives in `com.serenity.config`, a layer above this package, so the
  * throttle itself is expressed as a plain `reducedRate: Boolean` rather than depending on that type).
  */
class CompanionSpriteStateTickSpec extends AnyFlatSpec with Matchers:

  "CompanionSpriteState.tick" should "advance every call at full rate" in {
    val random = new Random(0L)
    val state  = CompanionSpriteState(frameCounts = Map(CompanionSpriteAction.Idle -> 4))

    val ticked = (1 to 3).foldLeft(state)((s, _) => s.tick(random, reducedRate = false, actionChance = 0.0))

    ticked.frameIndex shouldBe 3
  }

  it should "advance only every second call at reduced rate" in {
    val random = new Random(0L)
    val state  = CompanionSpriteState(frameCounts = Map(CompanionSpriteAction.Idle -> 4))

    val afterOne = state.tick(random, reducedRate = true, actionChance = 0.0)
    afterOne.frameIndex shouldBe 0

    val afterTwo = afterOne.tick(random, reducedRate = true, actionChance = 0.0)
    afterTwo.frameIndex shouldBe 1

    val afterThree = afterTwo.tick(random, reducedRate = true, actionChance = 0.0)
    afterThree.frameIndex shouldBe 1

    val afterFour = afterThree.tick(random, reducedRate = true, actionChance = 0.0)
    afterFour.frameIndex shouldBe 2
  }
