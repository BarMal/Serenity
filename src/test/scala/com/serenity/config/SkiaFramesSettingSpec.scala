package com.serenity.config

import com.serenity.config.AppConfigOps.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaFramesSettingSpec extends AnyFlatSpec with Matchers:

  "SkiaFrames.fromConfigKey" should "read image and picture, in any case, and nothing else" in {
    SkiaFrames.fromConfigKey("image") shouldBe Some(SkiaFrames.Image)
    SkiaFrames.fromConfigKey(" Picture ") shouldBe Some(SkiaFrames.Picture)
    SkiaFrames.fromConfigKey("raster") shouldBe None
    SkiaFrames.fromConfigKey("") shouldBe None
  }

  "The default config" should "draw images" in {
    AppConfig.default.surfaceConfig.renderSkiaFrames shouldBe SkiaFrames.Image
  }

  "ui.render.skia_frames" should "round-trip through the config file" in {
    SkiaFrames.values.foreach { frames =>
      val text   = ConfigManager.configToString(AppConfig.default.withRenderSkiaFrames(frames))
      text should include(s"ui.render.skia_frames = ${frames.configKey}")
    }
  }

  it should "be a registered setting under its documented names" in {
    ConfigRegistry.find("ui.render.skia_frames").map(_.key) shouldBe Some("ui.render.skia_frames")
    ConfigRegistry.find("render.skia_frames").map(_.key) shouldBe Some("ui.render.skia_frames")
  }
