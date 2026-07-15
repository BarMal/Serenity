package com.serenity

import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FloatingSurfaceGeometrySpec extends AnyFlatSpec with Matchers:

  "FloatingSurfaceGeometry" should "preserve fractional item gaps in logical pixels" in {
    val geometry = FloatingSurfaceGeometry.forItems(
      frame = LayoutRect(10, 4, 20, 8),
      metrics = CellMetrics(8, 16, 12),
      itemCount = 3,
      itemGapRows = 0.25
    )

    geometry.frame shouldBe PixelRect(80.0, 64.0, 160.0, 128.0)
    geometry.itemRects.map(_.y) shouldBe List(64.0, 84.0, 104.0)
    geometry.itemIndexAt(81.0, 72.0) shouldBe Some(0)
    geometry.itemIndexAt(81.0, 80.0) shouldBe None
  }

  it should "use pixel input when available and retain cell input as a fallback" in {
    val geometry = FloatingSurfaceGeometry.forItems(
      frame = LayoutRect(0, 0, 10, 4),
      metrics = CellMetrics(10, 20, 15),
      itemCount = 2,
      itemGapRows = 0.5
    )

    geometry.itemIndexAt(pixelX = Some(5), pixelY = Some(31), cellX = 0, cellY = 0) shouldBe Some(1)
    geometry.itemIndexAt(pixelX = None, pixelY = None, cellX = 0, cellY = 0) shouldBe Some(0)
  }

  it should "derive the visible window from fractional pixel strides" in {
    val geometry = FloatingSurfaceGeometry.forItems(
      frame = LayoutRect(0, 0, 10, 7),
      metrics = CellMetrics(10, 20, 15),
      itemCount = 12,
      itemGapRows = 0.25,
      headerRows = 1,
      footerRows = 1,
      selectedIndex = 8
    )

    geometry.itemWindow shouldBe SurfaceItemWindow(6, 4)
    geometry.itemRects.map(_.y) shouldBe List(20.0, 45.0, 70.0, 95.0)
  }

  it should "expose toolbar rows through the same interactive rectangle contract" in {
    val geometry = FloatingSurfaceGeometry.forItems(
      frame = LayoutRect(2, 3, 12, 5),
      metrics = CellMetrics(8, 16, 12),
      itemCount = 2,
      itemGapRows = 0.5,
      borderCells = 1
    )

    geometry.itemIndexAt(25.0, 65.0) shouldBe Some(0)
    geometry.itemIndexAt(25.0, 82.0) shouldBe None
    geometry.itemIndexAt(25.0, 89.0) shouldBe Some(1)
  }
