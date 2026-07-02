package com.serenity

import com.serenity.ui.layout.{LayoutRect, SurfaceFrameLayout}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceFrameLayoutSpec extends AnyFlatSpec with Matchers:

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
