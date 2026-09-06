package com.serenity

import com.serenity.config.CornerPosition
import com.serenity.state.models.SurfaceId
import com.serenity.ui.layout.LayoutEngine.{CornerPanelSlot, calculateCornerOverlayStack}
import com.serenity.ui.layout.LayoutRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `LayoutEngine.calculateCornerOverlayStack` (issue #1310, mode 3): lays out every panel assigned to one screen corner
  * as a vertical list, stacking from the corner outward, collapsing the tail when it doesn't all fit.
  */
class CornerOverlayLayoutSpec extends AnyFlatSpec with Matchers:

  private val content = LayoutRect(x = 0, y = 0, width = 40, height = 20)

  "calculateCornerOverlayStack" should "place nothing for an empty slot list" in {
    val result = calculateCornerOverlayStack(CornerPosition.BottomRight, Nil, content, gapRows = 1)

    result.stack shouldBe Nil
    result.collapsedSurfaceIds shouldBe Set.empty
  }

  it should "anchor a single slot to the bottom-right corner" in {
    val slot   = CornerPanelSlot(SurfaceId("a"), preferredWidth = 10, preferredHeight = 4)
    val result = calculateCornerOverlayStack(CornerPosition.BottomRight, List(slot), content, gapRows = 1)

    result.stack shouldBe List(SurfaceId("a") -> LayoutRect(x = 30, y = 16, width = 10, height = 4))
    result.collapsedSurfaceIds shouldBe Set.empty
  }

  it should "anchor a single slot to the top-left corner" in {
    val slot   = CornerPanelSlot(SurfaceId("a"), preferredWidth = 10, preferredHeight = 4)
    val result = calculateCornerOverlayStack(CornerPosition.TopLeft, List(slot), content, gapRows = 1)

    result.stack shouldBe List(SurfaceId("a") -> LayoutRect(x = 0, y = 0, width = 10, height = 4))
  }

  it should "anchor a single slot to the top-right corner" in {
    val slot   = CornerPanelSlot(SurfaceId("a"), preferredWidth = 10, preferredHeight = 4)
    val result = calculateCornerOverlayStack(CornerPosition.TopRight, List(slot), content, gapRows = 1)

    result.stack shouldBe List(SurfaceId("a") -> LayoutRect(x = 30, y = 0, width = 10, height = 4))
  }

  it should "anchor a single slot to the bottom-left corner" in {
    val slot   = CornerPanelSlot(SurfaceId("a"), preferredWidth = 10, preferredHeight = 4)
    val result = calculateCornerOverlayStack(CornerPosition.BottomLeft, List(slot), content, gapRows = 1)

    result.stack shouldBe List(SurfaceId("a") -> LayoutRect(x = 0, y = 16, width = 10, height = 4))
  }

  it should "stack multiple slots outward from the corner, first slot closest to the edge, with gaps between them" in {
    val a      = CornerPanelSlot(SurfaceId("a"), preferredWidth = 10, preferredHeight = 4)
    val b      = CornerPanelSlot(SurfaceId("b"), preferredWidth = 10, preferredHeight = 3)
    val result = calculateCornerOverlayStack(CornerPosition.BottomRight, List(a, b), content, gapRows = 1)

    result.stack shouldBe List(
      SurfaceId("a") -> LayoutRect(x = 30, y = 16, width = 10, height = 4), // closest to the bottom edge
      SurfaceId("b") -> LayoutRect(x = 30, y = 12, width = 10, height = 3)  // stacked above it, with a 1-row gap
    )
    result.collapsedSurfaceIds shouldBe Set.empty
  }

  it should "stack multiple slots downward from a top corner" in {
    val a      = CornerPanelSlot(SurfaceId("a"), preferredWidth = 10, preferredHeight = 4)
    val b      = CornerPanelSlot(SurfaceId("b"), preferredWidth = 10, preferredHeight = 3)
    val result = calculateCornerOverlayStack(CornerPosition.TopLeft, List(a, b), content, gapRows = 1)

    result.stack shouldBe List(
      SurfaceId("a") -> LayoutRect(x = 0, y = 0, width = 10, height = 4),
      SurfaceId("b") -> LayoutRect(x = 0, y = 5, width = 10, height = 3)
    )
  }

  it should "collapse slots that don't fit within the available height, in order, rather than shrinking them" in {
    val a = CornerPanelSlot(SurfaceId("a"), preferredWidth = 10, preferredHeight = 8)
    val b = CornerPanelSlot(SurfaceId("b"), preferredWidth = 10, preferredHeight = 8)
    val c = CornerPanelSlot(SurfaceId("c"), preferredWidth = 10, preferredHeight = 8)
    // a (8) + gap (1) + b (8) = 17 fits in a height-20 area; adding c would need 26.
    val result = calculateCornerOverlayStack(CornerPosition.BottomRight, List(a, b, c), content, gapRows = 1)

    result.stack.map(_._1) shouldBe List(SurfaceId("a"), SurfaceId("b"))
    result.collapsedSurfaceIds shouldBe Set(SurfaceId("c"))
  }

  it should "collapse everything when even the first slot doesn't fit" in {
    val tooTall = CornerPanelSlot(SurfaceId("a"), preferredWidth = 10, preferredHeight = 100)

    val result = calculateCornerOverlayStack(CornerPosition.BottomRight, List(tooTall), content, gapRows = 1)

    result.stack shouldBe Nil
    result.collapsedSurfaceIds shouldBe Set(SurfaceId("a"))
  }
