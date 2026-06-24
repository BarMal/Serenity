package com.serenity

import java.awt.geom.AffineTransform

import com.serenity.ui.display.DisplayScale
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DisplayScaleSpec extends AnyFlatSpec with Matchers:

  "DisplayScale" should "derive text scale from the largest device axis" in {
    DisplayScale.DeviceScale(1.25, 2.0).textScale shouldBe 2.0
  }

  it should "clamp transform scales below one to one" in {
    val scale = DisplayScale.fromTransform(AffineTransform.getScaleInstance(0.75, 0.5))

    scale shouldBe DisplayScale.One
  }

  it should "derive device scale from a graphics transform" in {
    val scale = DisplayScale.fromTransform(AffineTransform.getScaleInstance(1.5, 2.0))

    scale shouldBe DisplayScale.DeviceScale(1.5, 2.0)
  }
