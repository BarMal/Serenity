package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PixelRectSpec extends AnyFlatSpec with Matchers:

  "PixelRect.intersects" should "hold for overlapping rectangles" in {
    PixelRect(0, 0, 10, 10).intersects(PixelRect(5, 5, 10, 10)) shouldBe true
  }

  it should "be symmetric" in {
    val a = PixelRect(0, 0, 10, 10)
    val b = PixelRect(5, 5, 10, 10)
    a.intersects(b) shouldBe b.intersects(a)
  }

  it should "not hold for disjoint rectangles" in {
    PixelRect(0, 0, 10, 10).intersects(PixelRect(20, 20, 5, 5)) shouldBe false
  }

  it should "not hold for rectangles that only touch at an edge" in {
    PixelRect(0, 0, 10, 10).intersects(PixelRect(10, 0, 10, 10)) shouldBe false
  }

  it should "hold when one rectangle fully contains the other" in {
    PixelRect(0, 0, 20, 20).intersects(PixelRect(5, 5, 2, 2)) shouldBe true
  }
