package com.serenity.ui.renderer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceSizingSpec extends AnyFlatSpec with Matchers:

  "SurfaceSizing.deviceImageDimension" should "scale logical pixels up to device pixels" in {
    SurfaceSizing.deviceImageDimension(logicalDimensionPx = 1024, deviceScale = 2.0) shouldBe 2048
    SurfaceSizing.deviceImageDimension(logicalDimensionPx = 801, deviceScale = 1.5) shouldBe 1202
    SurfaceSizing.deviceImageDimension(logicalDimensionPx = 0, deviceScale = 2.0) shouldBe 2
  }

  it should "never go below one pixel or scale below 1.0" in {
    SurfaceSizing.deviceImageDimension(logicalDimensionPx = -5, deviceScale = 0.5) shouldBe 1
    SurfaceSizing.deviceImageDimension(logicalDimensionPx = 100, deviceScale = 0.5) shouldBe 100
  }

  "SurfaceSizing.logicalCanvasDimension" should "use the current size once the canvas has one" in {
    SurfaceSizing.logicalCanvasDimension(currentPx = 640, preferredPx = 800) shouldBe 640
  }

  it should "fall back to the preferred size, and to one pixel, before it is laid out" in {
    SurfaceSizing.logicalCanvasDimension(currentPx = 0, preferredPx = 800) shouldBe 800
    SurfaceSizing.logicalCanvasDimension(currentPx = 0, preferredPx = 0) shouldBe 1
  }

  "SurfaceSizing.NoOpImageObserver" should "never ask for further notifications" in {
    SurfaceSizing.NoOpImageObserver.imageUpdate(null, 0, 0, 0, 1, 1) shouldBe false
  }
