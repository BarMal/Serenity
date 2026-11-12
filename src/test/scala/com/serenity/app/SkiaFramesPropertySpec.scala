package com.serenity.app

import com.serenity.config.SkiaFrames
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaFramesPropertySpec extends AnyFlatSpec with Matchers:

  "SkiaFramesProperty.property" should "hand the layer the configured frame form through the property it reads" in {
    SkiaFramesProperty.property(SkiaFrames.Picture, Map.empty) shouldBe
      Some("serenity.skia.layerFrames" -> "picture")
    SkiaFramesProperty.property(SkiaFrames.Image, Map.empty) shouldBe
      Some("serenity.skia.layerFrames" -> "image")
  }

  it should "leave a value the user already set with -D, whatever the config says" in {
    SkiaFramesProperty.property(SkiaFrames.Picture, Map("serenity.skia.layerFrames" -> "image")) shouldBe None
    SkiaFramesProperty.property(SkiaFrames.Image, Map("serenity.skia.layerFrames" -> "picture")) shouldBe None
  }

  it should "treat a blank property as not set" in {
    SkiaFramesProperty.property(SkiaFrames.Picture, Map("serenity.skia.layerFrames" -> "  ")) shouldBe
      Some("serenity.skia.layerFrames" -> "picture")
  }

  it should "use the key the layer parses" in
    SkiaFrames.values.foreach { frames =>
      com.serenity.ui.skia.LayerFrames.parse(Some(frames.configKey)).toString shouldBe frames.toString
    }
