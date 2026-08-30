package com.serenity

import com.serenity.state.models.{SurfaceId, SurfacePlacement}
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers `LayoutEngine.resolveFrozenCursorPeekStack`: the box-layout-style, ordered-stack-with-insertion resolver
  * for the cursor-peek prototype's frozen anchor. Unlike `calculateFloatingSurfaceRect`/`floatingAnchor`, this
  * function takes its anchor screen position as a plain parameter rather than deriving it from `AppState`/the
  * active buffer -- these specs exercise it as the pure geometry function it is, with no `AppState` at all, which is
  * itself evidence the anchor really is frozen rather than live-recomputed.
  */
class LayoutEngineFrozenCursorPeekSpec extends AnyFlatSpec with Matchers:

  private val ContentRect = LayoutRect(x = 0, y = 0, width = 80, height = 24)
  private val SlotId      = SurfaceId("command-runner-peek")

  private def slot(width: Int = 40, height: Int = 6) =
    LayoutEngine.FrozenPeekSlot(SlotId, preferredWidth = width, preferredHeight = height)

  "a single slot with room below the anchor" should "place below.cursor per configured placement" in {
    val anchor = ScreenPosition(x = 20, y = 5)
    val result =
      LayoutEngine.resolveFrozenCursorPeekStack(List(slot()), anchor, ContentRect, SurfacePlacement.BelowCursor, gapRows = 1)

    result should have size 1
    val rect = result.head.rect
    rect.y shouldBe (anchor.y + 1 + 1)
    rect.height shouldBe 6
  }

  it should "place above.cursor per configured placement when there is room above" in {
    val anchor = ScreenPosition(x = 20, y = 15)
    val result =
      LayoutEngine.resolveFrozenCursorPeekStack(List(slot()), anchor, ContentRect, SurfacePlacement.AboveCursor, gapRows = 1)

    result should have size 1
    val rect = result.head.rect
    rect.y shouldBe (anchor.y - 1 - 6)
    rect.height shouldBe 6
  }

  "a below.cursor preference with no room below" should "fall back to above when that side fits" in {
    val anchor = ScreenPosition(x = 20, y = 22) // only 1 row below in a 24-row content rect
    val result =
      LayoutEngine.resolveFrozenCursorPeekStack(List(slot()), anchor, ContentRect, SurfacePlacement.BelowCursor, gapRows = 1)

    result should have size 1
    val rect = result.head.rect
    rect.bottom should be <= anchor.y
  }

  "an above.cursor preference with no room above" should "fall back to below when that side fits" in {
    val anchor = ScreenPosition(x = 20, y = 1) // only 1 row above
    val result =
      LayoutEngine.resolveFrozenCursorPeekStack(List(slot()), anchor, ContentRect, SurfacePlacement.AboveCursor, gapRows = 1)

    result should have size 1
    val rect = result.head.rect
    rect.y should be >= anchor.y
  }

  "a slot taller than the room on either side" should "be capped to whatever fits, not overflow the content rect" in {
    val anchor = ScreenPosition(x = 20, y = 12)
    val tallSlot =
      LayoutEngine.FrozenPeekSlot(SlotId, preferredWidth = 40, preferredHeight = 100)
    val result =
      LayoutEngine.resolveFrozenCursorPeekStack(List(tallSlot), anchor, ContentRect, SurfacePlacement.BelowCursor, gapRows = 0)

    result should have size 1
    val rect = result.head.rect
    rect.y should be >= ContentRect.y
    rect.bottom should be <= ContentRect.bottom
    rect.height should be < 100
  }

  "a slot wider than the content rect" should "be capped to the content rect's width" in {
    val anchor = ScreenPosition(x = 20, y = 5)
    val wideSlot =
      LayoutEngine.FrozenPeekSlot(SlotId, preferredWidth = 1000, preferredHeight = 6)
    val result =
      LayoutEngine.resolveFrozenCursorPeekStack(List(wideSlot), anchor, ContentRect, SurfacePlacement.BelowCursor, gapRows = 0)

    result.head.rect.width shouldBe ContentRect.width
  }

  "a slot's x position" should "center on the anchor's column, clipped to the content rect" in {
    val anchor = ScreenPosition(x = 5, y = 5) // near the left edge -- centering would go negative without clipping
    val result =
      LayoutEngine.resolveFrozenCursorPeekStack(List(slot()), anchor, ContentRect, SurfacePlacement.BelowCursor, gapRows = 0)

    val rect = result.head.rect
    rect.x should be >= ContentRect.x
    rect.right should be <= ContentRect.right
  }

  "multiple ordered slots" should "stack in order, separated by the gap, box-layout style" in {
    val anchor = ScreenPosition(x = 20, y = 2)
    val first  = LayoutEngine.FrozenPeekSlot(SurfaceId("first"), preferredWidth = 40, preferredHeight = 4)
    val second = LayoutEngine.FrozenPeekSlot(SurfaceId("second"), preferredWidth = 40, preferredHeight = 4)
    val result =
      LayoutEngine.resolveFrozenCursorPeekStack(List(first, second), anchor, ContentRect, SurfacePlacement.BelowCursor, gapRows = 1)

    result.map(_.id) shouldBe List(first.id, second.id)
    val firstRect  = result.head.rect
    val secondRect = result(1).rect
    secondRect.y shouldBe (firstRect.bottom + 1)
  }

  "a later slot with no height budget left" should "be dropped from the result rather than overflow" in {
    // A cramped 6-row content rect: the first slot alone (height 6) already exhausts the available room, so the
    // second slot's height budget is driven to exactly zero.
    val crampedRect = LayoutRect(x = 0, y = 0, width = 80, height = 6)
    val anchor      = ScreenPosition(x = 20, y = 0)
    val first       = LayoutEngine.FrozenPeekSlot(SurfaceId("first"), preferredWidth = 40, preferredHeight = 6)
    val second      = LayoutEngine.FrozenPeekSlot(SurfaceId("second"), preferredWidth = 40, preferredHeight = 4)
    val result =
      LayoutEngine.resolveFrozenCursorPeekStack(List(first, second), anchor, crampedRect, SurfacePlacement.BelowCursor, gapRows = 1)

    result.map(_.id) should contain(first.id)
    result.map(_.id) should not contain second.id
  }

  "an empty slot list" should "resolve to no placements" in {
    val anchor = ScreenPosition(x = 20, y = 5)
    LayoutEngine.resolveFrozenCursorPeekStack(Nil, anchor, ContentRect, SurfacePlacement.BelowCursor, gapRows = 1) shouldBe Nil
  }

  "calling the resolver twice with the same frozen anchor" should "always produce the same placement" in {
    val anchor = ScreenPosition(x = 20, y = 5)
    val first  = LayoutEngine.resolveFrozenCursorPeekStack(List(slot()), anchor, ContentRect, SurfacePlacement.BelowCursor, 1)
    val second = LayoutEngine.resolveFrozenCursorPeekStack(List(slot()), anchor, ContentRect, SurfacePlacement.BelowCursor, 1)

    first shouldBe second
  }
