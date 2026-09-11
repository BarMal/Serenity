package com.serenity

import com.serenity.ui.theme.NormalizedAlpha
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NormalizedAlphaSpec extends AnyFlatSpec with Matchers:

  "NormalizedAlpha" should "keep an in-range value unchanged" in {
    NormalizedAlpha(0.42).value shouldEqual 0.42
  }

  it should "clamp a value above 1.0 down to 1.0" in {
    NormalizedAlpha(1.5).value shouldEqual 1.0
  }

  it should "clamp a value below 0.0 up to 0.0" in {
    NormalizedAlpha(-0.3).value shouldEqual 0.0
  }

  it should "expose Opaque and Transparent as the 1.0 and 0.0 endpoints" in {
    NormalizedAlpha.Opaque.value shouldEqual 1.0
    NormalizedAlpha.Transparent.value shouldEqual 0.0
  }

  "toByteScale" should "convert the 0-1 scale to java.awt.Color's 0-255 byte scale" in {
    NormalizedAlpha.Opaque.toByteScale shouldEqual 255
    NormalizedAlpha.Transparent.toByteScale shouldEqual 0
    NormalizedAlpha(0.5).toByteScale shouldEqual 128
  }

  "fromByteScale" should "convert java.awt.Color's 0-255 byte scale back to the 0-1 scale" in {
    NormalizedAlpha.fromByteScale(255).value shouldEqual 1.0
    NormalizedAlpha.fromByteScale(0).value shouldEqual 0.0
    NormalizedAlpha.fromByteScale(128).toByteScale shouldEqual 128
  }
