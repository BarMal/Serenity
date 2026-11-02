package com.serenity.ui.benchmark

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RefreshRateSpec extends AnyFlatSpec with Matchers:

  "RefreshRate.of" should "take the rate the display reports" in {
    RefreshRate.of(144) shouldBe RefreshRate(144.0, assumed = false)
    RefreshRate.of(144).nanos shouldBe 6_944_444L
  }

  it should "assume 60 Hz where the display reports none" in {
    RefreshRate.of(0) shouldBe RefreshRate(60.0, assumed = true)
    RefreshRate.of(-1) shouldBe RefreshRate.Assumed60
  }

  "RefreshRate.Assumed60" should "last a sixtieth of a second, rounded to the nanosecond" in {
    RefreshRate.Assumed60.nanos shouldBe 16_666_667L
  }

  "RefreshRate.current" should "give a usable rate on any machine" in {
    RefreshRate.current.hz should be > 0.0
  }
