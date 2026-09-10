package com.serenity.ui.display

import java.awt.{Component, GraphicsEnvironment}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.serenity.ui.display.DisplayScale.DeviceScale

class DisplayScaleSpec extends AnyFlatSpec with Matchers:

  "DeviceScale.textScale" should "return the larger axis when both exceed 1.0" in {
    DeviceScale(1.5, 2.0).textScale shouldBe 2.0
    DeviceScale(3.0, 1.25).textScale shouldBe 3.0
  }

  it should "clamp to 1.0 when both axes are below 1.0" in {
    DeviceScale(0.5, 0.75).textScale shouldBe 1.0
  }

  it should "clamp to 1.0 when both axes are exactly 1.0" in {
    DeviceScale(1.0, 1.0).textScale shouldBe 1.0
  }

  it should "clamp a sub-1.0 axis up to 1.0 even when the other axis is above 1.0" in {
    DeviceScale(0.5, 1.8).textScale shouldBe 1.8
  }

  "DisplayScale.One" should "be an identity device scale on both axes" in {
    DisplayScale.One shouldBe DeviceScale(1.0, 1.0)
    DisplayScale.One.textScale shouldBe 1.0
  }

  "DisplayScale.forComponent" should "fall back to One for a component with no graphics configuration" in {
    // A freshly constructed Component that has never been added to a container/shown has a null
    // GraphicsConfiguration -- forComponent's Option(...).map(...) chain must fall through to `One`.
    val orphanComponent = new Component() {}
    orphanComponent.getGraphicsConfiguration shouldBe null

    DisplayScale.forComponent(orphanComponent) shouldBe DisplayScale.One
  }

  "DisplayScale.defaultDeviceScale" should "always report both axes at or above 1.0" in {
    // The clamp (`.max(1.0)`) applies on both the resolved-config path and the exception-fallback path, so this
    // invariant holds regardless of whether the environment running the test is headless.
    val scale = DisplayScale.defaultDeviceScale
    scale.x should be >= 1.0
    scale.y should be >= 1.0
  }

  it should "fall back to One via the AWT catch branch in a headless environment" in {
    assume(GraphicsEnvironment.isHeadless, "This test only exercises the try/catch fallback when AWT is headless")

    DisplayScale.defaultDeviceScale shouldBe DisplayScale.One
  }

  it should "match the local default screen device's transform scale in a non-headless environment" in {
    assume(!GraphicsEnvironment.isHeadless, "This test only exercises the successful AWT lookup when a display is present")

    val config    = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration
    val transform = config.getDefaultTransform
    val expected  = DeviceScale(transform.getScaleX.max(1.0), transform.getScaleY.max(1.0))

    DisplayScale.defaultDeviceScale shouldBe expected
  }
