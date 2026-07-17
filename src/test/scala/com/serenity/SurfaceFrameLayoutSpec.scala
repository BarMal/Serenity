package com.serenity

import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceFrameLayoutSpec extends AnyFlatSpec with Matchers:

  "FloatingSurfaceGeometry" should "retain fractional row gaps in logical pixels" in {
    val geometry = FloatingSurfaceGeometry(
      frame = LogicalPixelRect(80.0, 40.0, 320.0, 240.0),
      cellWidthPx = 8.0,
      lineHeightPx = 20.0,
      borderXPx = 2.0,
      borderYPx = 2.0,
      itemGapRows = 0.25
    )

    geometry.contentRect shouldBe LogicalPixelRect(82.0, 42.0, 316.0, 236.0)
    geometry.itemRect(index = 1, headerRows = 1) shouldBe LogicalPixelRect(82.0, 87.0, 316.0, 20.0)
    geometry.itemIndexAt(100.0, 70.0, itemCount = 3, headerRows = 1) shouldBe Some(0)
    geometry.itemIndexAt(100.0, 84.0, itemCount = 3, headerRows = 1) shouldBe None
  }

  it should "convert cell frames once at the floating-surface boundary" in {
    val geometry = FloatingSurfaceGeometry.fromCells(
      LayoutRect(4, 3, 20, 8),
      CellMetrics(charWidth = 9, lineHeight = 18, ascent = 13),
      itemGapRows = 0.5
    )

    geometry.frame shouldBe LogicalPixelRect(36.0, 54.0, 180.0, 144.0)
    geometry.itemRect(index = 1) shouldBe LogicalPixelRect(36.0, 81.0, 180.0, 18.0)
  }

  it should "retain fractional surface placement at non-integer device scale" in {
    val placement = FloatingSurfacePlacement(LayoutRect(4, 3, 20, 8), yOffsetRows = 0.5)
    val metrics   = CellMetrics(charWidth = 9, lineHeight = 15, ascent = 11)

    placement.logicalFrame(metrics) shouldBe LogicalPixelRect(36.0, 52.5, 180.0, 120.0)
    placement.geometry(metrics, itemGapRows = 0.25).itemRect(1) shouldBe
      LogicalPixelRect(36.0, 71.25, 180.0, 15.0)
  }

  it should "apply non-square cell border insets on their matching axes" in {
    val geometry = FloatingSurfaceGeometry.fromCells(
      LayoutRect(2, 1, 10, 6),
      CellMetrics(charWidth = 7, lineHeight = 19, ascent = 14),
      borderCells = 1
    )

    geometry.contentRect shouldBe LogicalPixelRect(21.0, 38.0, 56.0, 76.0)
  }

  "SurfaceFrameLayout" should "derive content bounds from a framed overlay rectangle" in {
    val frame = SurfaceFrameLayout(LayoutRect(10, 4, 40, 8))

    frame.frameRect.shouldBe(LayoutRect(10, 4, 40, 8))
    frame.contentRect.shouldBe(LayoutRect(11, 5, 38, 6))
    frame.maxContentRows.shouldBe(6)
  }

  it should "calculate visible item rows after header, footer, and reserved detail rows" in {
    val frame = SurfaceFrameLayout(LayoutRect(0, 0, 40, 10))

    frame.visibleItemRows(hasHeader = true, hasFooter = true).shouldBe(6)
    frame.visibleItemRows(hasHeader = true, hasFooter = true, reservedContentRows = 1).shouldBe(5)
    frame.visibleItemRows(hasHeader = false, hasFooter = false).shouldBe(8)
  }

  it should "calculate the frame height required for visible item rows" in {
    SurfaceFrameLayout
      .frameHeightForItemRows(
        itemRows = 4,
        hasHeader = true,
        hasFooter = true
      )
      .shouldBe(8)

    SurfaceFrameLayout
      .frameHeightForItemRows(
        itemRows = 4,
        hasHeader = true,
        hasFooter = true,
        reservedContentRows = 1
      )
      .shouldBe(9)
  }

  it should "reserve blank rows between spaced items while keeping header and footer slots fixed" in {
    val frame = SurfaceFrameLayout(LayoutRect(10, 4, 40, 8), borderCells = 0)

    frame.visibleItemRows(hasHeader = true, hasFooter = true, itemGapRows = 1) shouldBe 3
    frame.contentRowSlots(itemCount = 5, hasHeader = true, hasFooter = true, itemGapRows = 1) shouldBe List(
      SurfaceContentRowSlot(SurfaceContentRowKind.Header, 4),
      SurfaceContentRowSlot(SurfaceContentRowKind.Item(0), 5),
      SurfaceContentRowSlot(SurfaceContentRowKind.Item(1), 7),
      SurfaceContentRowSlot(SurfaceContentRowKind.Item(2), 9),
      SurfaceContentRowSlot(SurfaceContentRowKind.Footer, 11)
    )
    frame
      .itemIndexAt(
        row = 6,
        itemCount = 5,
        selectedIndex = 0,
        hasHeader = true,
        hasFooter = true,
        itemGapRows = 1
      )
      .shouldBe(None)
  }

  it should "derive a centered item window from the framed surface content contract" in {
    val frame = SurfaceFrameLayout(LayoutRect(0, 0, 40, 8))

    val window = frame.itemWindow(
      itemCount = 12,
      selectedIndex = 7,
      hasHeader = true,
      hasFooter = true
    )

    window.offset.shouldBe(5)
    window.rowCount.shouldBe(4)
    window.slice((0 until 12).toList).shouldBe(List(5, 6, 7, 8))
    window.adjustedSelectedIndex(7).shouldBe(2)
  }

  it should "map frame rows to item indices and reject chrome rows" in {
    val frame = SurfaceFrameLayout(LayoutRect(10, 4, 40, 8))

    frame
      .itemIndexAt(
        row = 4,
        itemCount = 12,
        selectedIndex = 0,
        hasHeader = true,
        hasFooter = true
      )
      .shouldBe(None)
    frame
      .itemIndexAt(
        row = 5,
        itemCount = 12,
        selectedIndex = 0,
        hasHeader = true,
        hasFooter = true
      )
      .shouldBe(None)
    frame
      .itemIndexAt(
        row = 6,
        itemCount = 12,
        selectedIndex = 0,
        hasHeader = true,
        hasFooter = true
      )
      .shouldBe(Some(0))
    frame
      .itemIndexAt(
        row = 9,
        itemCount = 12,
        selectedIndex = 0,
        hasHeader = true,
        hasFooter = true
      )
      .shouldBe(Some(3))
    frame
      .itemIndexAt(
        row = 10,
        itemCount = 12,
        selectedIndex = 0,
        hasHeader = true,
        hasFooter = true
      )
      .shouldBe(None)
  }

  it should "exclude reserved detail rows from item hit testing" in {
    val frame = SurfaceFrameLayout(LayoutRect(0, 0, 40, 8))

    frame
      .itemIndexAt(
        row = 5,
        itemCount = 12,
        selectedIndex = 0,
        hasHeader = true,
        hasFooter = true,
        reservedContentRows = 1
      )
      .shouldBe(None)
  }

  it should "derive header item and footer row slots from the framed content contract" in {
    val frame = SurfaceFrameLayout(LayoutRect(10, 4, 40, 8))

    val slots = frame.contentRowSlots(itemCount = 12, hasHeader = true, hasFooter = true)

    slots
      .map(slot => slot.kind -> slot.y)
      .shouldBe(
        List(
          SurfaceContentRowKind.Header  -> 5,
          SurfaceContentRowKind.Item(0) -> 6,
          SurfaceContentRowKind.Item(1) -> 7,
          SurfaceContentRowKind.Item(2) -> 8,
          SurfaceContentRowKind.Item(3) -> 9,
          SurfaceContentRowKind.Footer  -> 10
        )
      )
  }

  it should "pin footer row slots to the bottom of the content rect when there are few items" in {
    val frame = SurfaceFrameLayout(LayoutRect(10, 4, 40, 8))

    val slots = frame.contentRowSlots(itemCount = 1, hasHeader = true, hasFooter = true)

    slots
      .map(slot => slot.kind -> slot.y)
      .shouldBe(
        List(
          SurfaceContentRowKind.Header  -> 5,
          SurfaceContentRowKind.Item(0) -> 6,
          SurfaceContentRowKind.Footer  -> 10
        )
      )
  }
