package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `ContextualToolbarSurfaceComposition` (issue #819, slice 1): the contextual toolbar resolved into one
  * paint/hit-test plan, the same pattern `ContextMenuSurfaceComposition` and `CommandRunnerSurfaceComposition` already
  * establish for the other migrated surfaces. Row *content* stays sourced from `ContextualToolbarContentResolver`'s
  * existing, separately-tested row builder (via `SurfaceContentResolver.resolveContextualToolbar`) -- this composition
  * owns row *position* and *hit-testing* only, built directly on `ContextualToolbarLayout`'s own geometry helpers, so a
  * click can no longer resolve to a different item than the one painted there.
  */
class ContextualToolbarSurfaceCompositionSpec extends AnyFlatSpec with Matchers with ContextualToolbarTestSupport:

  "forToolbar" should
    "map the toolbar's first row to a Distributed paint box with the same segments SurfaceContentResolver paints (issue #819)" in {
      val stateManager = createStateManager("ContextualToolbarSurfaceCompositionSpec-distributed-mapping")

      stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
      seedToolbarDocument(stateManager)
      stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

      val state        = stateManager.getCurrentState.unsafeRunSync()
      val toolbarState = toolbarStateFrom(state)
      val rect         = toolbarRect(state)

      val expectedRow = SurfaceContentResolver
        .resolveContextualToolbar(toolbarState, state, rect, SurfaceRenderMode.Floating)
        .rows
        .headOption
        .getOrElse(fail("Expected at least one resolved toolbar row"))

      val resolved    = ContextualToolbarSurfaceComposition.forToolbar(toolbarState, state, rect)
      val firstRowBox = resolved.paintBoxes.headOption.getOrElse(fail("Expected at least one painted toolbar row"))

      firstRowBox.layout shouldBe SurfacePaintLayout.Distributed
      firstRowBox.segments shouldBe expectedRow.segments
      firstRowBox.text shouldBe Some(expectedRow.plainText)
    }

  it should "resolve a click at a top-level item's cell to the same ContextualToolbarHit ContextualToolbarLayout.hitAt returns" in {
    val stateManager = createStateManager("ContextualToolbarSurfaceCompositionSpec-top-level-hit")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state        = stateManager.getCurrentState.unsafeRunSync()
    val toolbarState = toolbarStateFrom(state)
    val rect         = toolbarRect(state)
    val point        = toolbarItemPoint(state, itemId = "italic")

    val resolved = ContextualToolbarSurfaceComposition.forToolbar(toolbarState, state, rect)
    val hit = resolved
      .hitAt(point.x.toDouble, point.y.toDouble)
      .flatMap(region => ContextualToolbarSurfaceComposition.hitFromFocusId(region.focusId))

    val expected = ContextualToolbarLayout.hitAt(
      rowIndex = 0,
      columnOffset =
        point.x - SurfaceFrameLayout.forContent(rect, SurfaceContent.ContextualToolbar(toolbarState)).contentRect.x,
      contentWidth = toolbarContentWidth(state),
      toolbarState = toolbarState,
      state = state
    )

    hit shouldBe defined
    hit shouldBe expected
  }

  it should "resolve a click on a group-separator gutter to no hit, matching ContextualToolbarLayout.hitAt" in {
    val stateManager = createStateManager("ContextualToolbarSurfaceCompositionSpec-separator-gap")

    stateManager.applyEvent(ResizeEvent(ViewportSize(160, 40))).unsafeRunSync()
    seedToolbarDocument(stateManager)
    stateManager.applyEvent(ToggleContextualToolbar).unsafeRunSync()

    val state        = stateManager.getCurrentState.unsafeRunSync()
    val toolbarState = toolbarStateFrom(state)
    val rect         = toolbarRect(state)
    val point        = toolbarSeparatorPoint(state, separatorIndex = 0)

    val resolved = ContextualToolbarSurfaceComposition.forToolbar(toolbarState, state, rect)
    val hit = resolved
      .hitAt(point.x.toDouble, point.y.toDouble)
      .flatMap(region => ContextualToolbarSurfaceComposition.hitFromFocusId(region.focusId))

    hit shouldBe None
  }

  it should "round-trip every ContextualToolbarHit through focusId/hitFromFocusId" in {
    val hits = List(
      ContextualToolbarHit.TopLevelItem(3),
      ContextualToolbarHit.DropdownOption("font-family", 2),
      ContextualToolbarHit.InputDetail("font-size")
    )

    hits.foreach { hit =>
      ContextualToolbarSurfaceComposition.hitFromFocusId(
        ContextualToolbarSurfaceComposition.focusId(hit)
      ) shouldBe Some(
        hit
      )
    }
  }
