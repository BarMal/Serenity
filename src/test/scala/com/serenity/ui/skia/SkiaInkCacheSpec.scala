package com.serenity.ui.skia

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaInkCacheSpec extends AnyFlatSpec with Matchers:

  "A colour's shader" should "be made once and reused" in {
    val inks = new SkiaInkCache()
    inks.shaderOf(0xffddeeff.toInt) should be theSameInstanceAs inks.shaderOf(0xffddeeff.toInt)
    inks.size shouldBe 1
  }

  it should "differ between colours, alpha included" in {
    val inks = new SkiaInkCache()
    inks.shaderOf(0xffddeeff.toInt) should not be theSameInstanceAs(inks.shaderOf(0x80ddeeff))
    inks.size shouldBe 2
  }

  "A cache" should "hold no more than its limit of colours, dropping them all when it is full" in {
    val inks = new SkiaInkCache(limit = 4)
    (0 until 4).foreach(i => inks.shaderOf(0xff000000 | i))
    inks.size shouldBe 4
    inks.shaderOf(0xff0000ff)
    inks.size shouldBe 1
  }
