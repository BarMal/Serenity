package com.serenity.perf

import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.ui.renderer.FrameSurfaceFactory
import com.serenity.ui.skia.{DiscardingImageLayer, SkiaRasterSurfaceFactory}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PresentBenchmarksBackendSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "PresentBenchmarks.factoryFor" should "know java2d, skia and layer-image, and refuse anything else" in {
    PresentBenchmarks.factoryFor("java2d") shouldBe Right(FrameSurfaceFactory.Java2D)
    PresentBenchmarks.factoryFor("skia") shouldBe Right(SkiaRasterSurfaceFactory)
    PresentBenchmarks.factoryFor("layer-image") shouldBe Right(DiscardingImageLayer.factory)
    PresentBenchmarks.factoryFor("skia-image").isLeft shouldBe true
    PresentBenchmarks.factoryFor("").isLeft shouldBe true
  }

  it should "say which backends it knows when it refuses one" in {
    PresentBenchmarks.factoryFor("bogus").left.getOrElse("") should (include("java2d") and include("layer-image"))
  }

  "A benchmark window on the layer's image frames" should "have no image to read back, and still say whether the frame drew" in {
    val cycle = PresentBenchmarks.scrollCycle(DiscardingImageLayer.factory, 400, 300, 1.0)
    val image = cycle.next()
    cycle.drew(image) shouldBe true
  }

  "A benchmark window on the raster surface" should "say whether the frame drew from the image it read back" in {
    val cycle = PresentBenchmarks.scrollCycle(SkiaRasterSurfaceFactory, 400, 300, 1.0)
    cycle.drew(cycle.next()) shouldBe true
  }
