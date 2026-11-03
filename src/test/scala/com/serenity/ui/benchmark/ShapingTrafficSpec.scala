package com.serenity.ui.benchmark

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShapingTrafficSpec extends AnyFlatSpec with Matchers:

  "ShapingTraffic.of" should "give the mean and the largest frame of shapings and of promotions" in {
    val frames = Seq(
      ShapingTraffic.Frame(shapings = 2, promotions = 0),
      ShapingTraffic.Frame(shapings = 6, promotions = 4),
      ShapingTraffic.Frame(shapings = 1, promotions = 2)
    )

    ShapingTraffic.of(frames) shouldBe Some(
      ShapingTraffic(frames = 3, shapingsMean = 3.0, shapingsMax = 6L, promotionsMean = 2.0, promotionsMax = 4L)
    )
  }

  it should "count a frame that shaped nothing" in {
    ShapingTraffic.of(Seq(ShapingTraffic.Frame(0, 0), ShapingTraffic.Frame(0, 0))) shouldBe Some(
      ShapingTraffic(2, 0.0, 0L, 0.0, 0L)
    )
  }

  it should "be nothing for no frames" in {
    ShapingTraffic.of(Seq.empty) shouldBe None
  }
