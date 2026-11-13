package com.serenity.app

import com.serenity.app.RendererChoice.Candidate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererSettingsSpec extends AnyFlatSpec with Matchers:

  "RendererSettings.of" should "name the settings that put the GPU layer on image frames" in {
    RendererSettings.of(Candidate.LayerImage) shouldBe
      List("ui.render.backend" -> "skia", "ui.render.skia_frames" -> "image")
  }

  it should "name the settings that put the GPU layer on picture frames" in {
    RendererSettings.of(Candidate.LayerPicture) shouldBe
      List("ui.render.backend" -> "skia", "ui.render.skia_frames" -> "picture")
  }

  it should "name only the backend for the Skia raster surface, which has no frame form to choose" in {
    RendererSettings.of(Candidate.SkiaRaster) shouldBe List("ui.render.backend" -> "skia-raster")
  }

  "RendererSettings.line" should "print the settings as they are written in config.conf" in {
    RendererSettings.line(Candidate.LayerPicture) shouldBe "ui.render.backend = skia, ui.render.skia_frames = picture"
  }
