package com.serenity.ui.theme

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeadingScaleSpec extends AnyFlatSpec with Matchers:

  private val h1 = 22.0f / 12.0f
  private val h2 = 18.0f / 12.0f
  private val h3 = 16.0f / 12.0f

  "HeadingScale.effective" should "leave a multiple untouched at or below the damping threshold" in {
    HeadingScale.effective(h1, logicalBodyPt = 12.0f, threshold = 24.0f, dampingOn = true) shouldBe h1
    HeadingScale.effective(h1, logicalBodyPt = 24.0f, threshold = 24.0f, dampingOn = true) shouldBe h1
  }

  it should "damp H1 to about 1.68x at a 36pt body" in {
    HeadingScale.effective(h1, 36.0f, 24.0f, dampingOn = true) shouldBe 1.68f +- 0.005f
  }

  it should "damp H1 to about 1.59x at a 48pt body" in {
    HeadingScale.effective(h1, 48.0f, 24.0f, dampingOn = true) shouldBe 1.59f +- 0.005f
  }

  it should "not damp when damping is off" in {
    HeadingScale.effective(h1, 48.0f, 24.0f, dampingOn = false) shouldBe h1
  }

  it should "honour a configured threshold" in {
    HeadingScale.effective(h1, 36.0f, 36.0f, dampingOn = true) shouldBe h1
    HeadingScale.effective(h1, 48.0f, 36.0f, dampingOn = true) should be < h1
  }

  it should "never shrink a heading below the body size" in {
    HeadingScale.effective(0.5f, 12.0f, 24.0f, dampingOn = true) shouldBe 1.0f
    HeadingScale.effective(0.5f, 96.0f, 24.0f, dampingOn = true) shouldBe 1.0f
  }

  it should "keep the H1 > H2 > H3 ordering at every body size" in
    (6 to 200 by 2).foreach { body =>
      val damped = List(h1, h2, h3).map(HeadingScale.effective(_, body.toFloat, 24.0f, dampingOn = true))
      damped.head should be > damped(1)
      damped(1) should be > damped(2)
    }

  "HeadingScale.logicalBodyPt" should "multiply the prose size by the manual multiplier and nothing else" in {
    HeadingScale.logicalBodyPt(proseSizePt = 12.0f, manualMultiplier = 1.0f) shouldBe 12.0f
    HeadingScale.logicalBodyPt(proseSizePt = 18.0f, manualMultiplier = 2.0f) shouldBe 36.0f
  }

  it should "leave a hi-DPI display's body undamped because device scale is not an input" in {
    val logical = HeadingScale.logicalBodyPt(proseSizePt = 12.0f, manualMultiplier = 1.0f)
    HeadingScale.effective(h1, logical, 24.0f, dampingOn = true) shouldBe h1
  }
