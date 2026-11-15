package com.serenity.ui.skia

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LayerRenderLineSpec extends AnyFlatSpec with Matchers:

  "LayerRenderLine.of" should "name the render API and the device Skiko reported" in {
    LayerRenderLine.of("OPENGL", Some("Mesa Intel(R) UHD Graphics 620 (KBL GT2)")) shouldBe
      "[RENDER] skia layer renderApi=OPENGL device=Mesa Intel(R) UHD Graphics 620 (KBL GT2)"
  }

  it should "say the device is unknown when Skiko reported none" in {
    LayerRenderLine.of("SOFTWARE_FAST", None) shouldBe "[RENDER] skia layer renderApi=SOFTWARE_FAST device=unknown"
  }

  it should "say the device is unknown when Skiko reported a blank name" in {
    LayerRenderLine.of("OPENGL", Some("  ")) shouldBe "[RENDER] skia layer renderApi=OPENGL device=unknown"
  }

  it should "keep the line to one line whatever the device name holds" in {
    LayerRenderLine.of("OPENGL", Some("llvmpipe\n(LLVM 17)")) shouldBe
      "[RENDER] skia layer renderApi=OPENGL device=llvmpipe (LLVM 17)"
  }
