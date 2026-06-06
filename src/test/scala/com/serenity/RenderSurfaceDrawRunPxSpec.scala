package com.serenity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RenderSurfaceDrawRunPxSpec extends AnyFlatSpec with Matchers:

  "MockRenderSurface.drawRunPx" should "record a call with pixel-precision xPx and bgWidthPx" in {
    val surface = new MockRenderSurface(80, 24)
    surface.drawRunPx(xPx = 10.5f, yPx = 20, bgWidthPx = 15.0f, lineHeightPx = 14, ascentPx = 11, s = "Hi")

    val calls = surface.drawRunPxCalls
    calls should have size 1
    calls.head.xPx shouldBe 10.5f
    calls.head.bgWidthPx shouldBe 15.0f
    calls.head.yPx shouldBe 20
    calls.head.ascentPx shouldBe 11
    calls.head.s shouldBe "Hi"
  }

  it should "record xPx as a Float, not rounded to a cell boundary" in {
    val surface = new MockRenderSurface(80, 24)
    surface.drawRunPx(xPx = 7.3f, yPx = 0, bgWidthPx = 5.1f, lineHeightPx = 14, ascentPx = 11, s = "x")
    surface.drawRunPxCalls.head.xPx shouldBe 7.3f +- 0.001f
  }

  it should "record multiple calls in order" in {
    val surface = new MockRenderSurface(80, 24)
    surface.drawRunPx(0.0f, 0, 7.5f, 14, 11, "a")
    surface.drawRunPx(7.5f, 0, 5.5f, 14, 11, "b")
    surface.drawRunPx(13.0f, 0, 6.5f, 14, 11, "c")

    val calls = surface.drawRunPxCalls
    calls should have size 3
    calls(0).s shouldBe "a"
    calls(1).s shouldBe "b"
    calls(2).s shouldBe "c"
  }
