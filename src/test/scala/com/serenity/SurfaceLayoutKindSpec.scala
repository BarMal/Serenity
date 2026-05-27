package com.serenity

import com.serenity.ui.layout.{LayoutRect, SurfaceLayoutKind}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceLayoutKindSpec extends AnyFlatSpec with Matchers:

  "SurfaceLayoutKind.classify" should "classify wide rectangles as horizontal" in {
    SurfaceLayoutKind.classify(LayoutRect(0, 0, 60, 12)) shouldBe SurfaceLayoutKind.Horizontal
  }

  it should "classify tall rectangles as vertical" in {
    SurfaceLayoutKind.classify(LayoutRect(0, 0, 18, 40)) shouldBe SurfaceLayoutKind.Vertical
  }

  it should "classify balanced rectangles as square" in {
    SurfaceLayoutKind.classify(LayoutRect(0, 0, 24, 20)) shouldBe SurfaceLayoutKind.Square
  }

  it should "classify very constrained rectangles as compact" in {
    SurfaceLayoutKind.classify(LayoutRect(0, 0, 14, 4)) shouldBe SurfaceLayoutKind.Compact
  }
