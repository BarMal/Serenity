package com.serenity.app

import com.serenity.app.RendererChoice.Candidate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererRunOrderSpec extends AnyFlatSpec with Matchers:

  "RendererRunOrder.candidates" should "compare layer+image with skia-raster, and add layer+picture only when thorough" in {
    RendererRunOrder.candidates(thorough = false) shouldBe List(Candidate.LayerImage, Candidate.SkiaRaster)
    RendererRunOrder.candidates(thorough = true) shouldBe
      List(Candidate.LayerImage, Candidate.SkiaRaster, Candidate.LayerPicture)
  }

  "RendererRunOrder.order" should "run each mode twice, mirrored, so a drifting machine favours none: A B B A" in {
    RendererRunOrder.order(thorough = false) shouldBe
      List(Candidate.LayerImage, Candidate.SkiaRaster, Candidate.SkiaRaster, Candidate.LayerImage)
  }

  it should "mirror a thorough run the same way: A B C C B A" in {
    RendererRunOrder.order(thorough = true) shouldBe List(
      Candidate.LayerImage,
      Candidate.SkiaRaster,
      Candidate.LayerPicture,
      Candidate.LayerPicture,
      Candidate.SkiaRaster,
      Candidate.LayerImage
    )
  }

  it should "give every candidate at least two runs, so the spread between a mode's own runs is real" in
    List(false, true).foreach { thorough =>
      val order = RendererRunOrder.order(thorough)
      RendererRunOrder.candidates(thorough).foreach(c => order.count(_ == c) shouldBe 2)
    }

  "RendererRunOrder.minutes" should "say how long a run takes, at about two minutes per trial" in {
    RendererRunOrder.minutes(thorough = false) shouldBe 8
    RendererRunOrder.minutes(thorough = true) shouldBe 12
  }
