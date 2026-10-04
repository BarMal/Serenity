package com.serenity

import com.serenity.AboveCursorStackFixtures.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.OverlayViewModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Several surfaces anchored above the cursor -- the comment lens, an LSP peek, the command runner's cursor peek --
  * stack upward from the cursor instead of each being placed against it independently and painted on top of one
  * another.
  */
class AboveCursorOverlayStackSpec extends AnyFlatSpec with Matchers:

  private val farFromTop = 20

  private def layoutOf(state: AppState): CalculatedLayout =
    LayoutEngine.calculateLayout(state, StackViewport)

  private def rectOf(layout: CalculatedLayout, surfaceId: SurfaceId): LayoutRect =
    layout.aboveCursorOverlayStack
      .collectFirst { case (`surfaceId`, rect) => rect }
      .getOrElse(fail(s"Expected ${surfaceId.value} in the above-cursor stack"))

  private def cursorScreenY(state: AppState, layout: CalculatedLayout, line: Int): Int =
    CursorLayout.contentRectForPane(LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))).y + line

  private def contentTopY(state: AppState, layout: CalculatedLayout): Int =
    CursorLayout.contentRectForPane(LayoutEngine.calculatePaneLayouts(state, layout)(PaneId(0))).y

  "LayoutEngine.calculateLayout" should "stack a peek above an open comment lens, the lens nearest the cursor" in {
    val state  = lensAndPeek(farFromTop)
    val layout = layoutOf(state)
    val lens   = rectOf(layout, lensId(state))
    val peek   = rectOf(layout, peekId(state))

    layout.aboveCursorOverlayStack.map(_._1) shouldBe List(lensId(state), peekId(state))
    lens.bottom should be <= cursorScreenY(state, layout, farFromTop)
    peek.bottom should be <= lens.y
    peek.y should be >= contentTopY(state, layout)
    layout.collapsedFloatingSurfaceIds shouldBe empty
  }

  it should "leave the stack's nearest surface exactly where it sits on its own" in {
    val lensAlone = withLens(farFromTop)
    val stacked   = lensAndPeek(farFromTop)

    rectOf(layoutOf(stacked), lensId(stacked)) shouldBe rectOf(layoutOf(lensAlone), lensId(lensAlone))
    layoutOf(stacked).aboveCursorOverlayRect shouldBe layoutOf(lensAlone).aboveCursorOverlayRect
  }

  it should "separate stacked surfaces by the configured stack gap" in {
    val state   = lensAndPeek(farFromTop)
    val layout  = layoutOf(state)
    val gapRows = math.floor(math.max(0.0, state.effectiveUiElementGap)).toInt

    rectOf(layout, lensId(state)).y - rectOf(layout, peekId(state)).bottom should be >= gapRows
  }

  it should "lay out a single peek exactly as it did before stacking existed" in {
    val peekOnly = withPeek(editorWithComment(farFromTop), farFromTop)
    val layout   = layoutOf(peekOnly)
    val peek     = rectOf(layout, peekId(peekOnly))

    layout.aboveCursorOverlayStack.map(_._1) shouldBe List(peekId(peekOnly))
    peek.bottom should be <= cursorScreenY(peekOnly, layout, farFromTop)
    layout.collapsedFloatingSurfaceIds shouldBe empty
  }

  it should "put the command runner's above-cursor peek farthest from the cursor" in {
    val state  = withRunnerCursorPeek(lensAndPeek(farFromTop), farFromTop)
    val layout = layoutOf(state)

    layout.aboveCursorOverlayStack.map(_._1).take(2) shouldBe List(lensId(state), peekId(state))
  }

  it should "collapse the unfocused lens to a one-line summary when the full stack does not fit above the cursor" in {
    val line   = roomForLensAndCollapsedPeekOnly
    val state  = unfocusedLensAndPeek(line)
    val layout = layoutOf(state)
    val lens   = rectOf(layout, lensId(state))
    val peek   = rectOf(layout, peekId(state))

    layout.collapsedFloatingSurfaceIds shouldBe Set(lensId(state))
    lens.height shouldBe 3
    lens.bottom should be <= cursorScreenY(state, layout, line)
    peek.bottom should be <= lens.y
    peek.y should be >= contentTopY(state, layout)
  }

  it should "clamp the stack into the pane, without overlap, when even collapsing leaves no room above the cursor" in {
    val state  = unfocusedLensAndPeek(1)
    val layout = layoutOf(state)
    val lens   = rectOf(layout, lensId(state))
    val peek   = rectOf(layout, peekId(state))

    layout.collapsedFloatingSurfaceIds shouldBe Set(lensId(state))
    peek.y shouldBe contentTopY(state, layout)
    lens.y should be >= peek.bottom
  }

  it should "keep the stack free of layout contract violations" in {
    val state = lensAndPeek(farFromTop)

    EditorLayoutContract.from(state, StackViewport, layoutOf(state)).violations shouldBe empty
  }

  "UiSceneSnapshot" should "carry a separate, non-overlapping floating node for each stacked surface" in {
    val state = lensAndPeek(farFromTop)
    val scene = UiSceneSnapshot.from(state, StackViewport)

    val lens = scene.floatingRect(lensId(state)).getOrElse(fail("Expected a lens node"))
    val peek = scene.floatingRect(peekId(state)).getOrElse(fail("Expected a peek node"))
    peek.bottom should be <= lens.y
    scene.editorContract.overlayRect(lensId(state)) shouldBe Some(lens)
    scene.editorContract.overlayRect(peekId(state)) shouldBe Some(peek)
  }

  "OverlayViewModel.fromState" should "build a view for every surface in the above-cursor stack" in {
    val state = lensAndPeek(farFromTop)
    val views = OverlayViewModel.fromState(state, layoutOf(state))

    views.aboveCursorStack.flatMap(_.surfaceId) shouldBe List(lensId(state), peekId(state))
    views.aboveCursor.flatMap(_.surfaceId) shouldBe Some(lensId(state))
  }

  it should "render a collapsed stacked surface as its one-line summary rather than its full composition" in {
    val line      = roomForLensAndCollapsedPeekOnly
    val state     = unfocusedLensAndPeek(line)
    val lensAlone = withLens(line)
    val fullLens = OverlayViewModel
      .fromState(lensAlone, layoutOf(lensAlone))
      .aboveCursor
      .getOrElse(fail("Expected the full lens view"))

    val lensView = OverlayViewModel
      .fromState(state, layoutOf(state))
      .aboveCursorStack
      .find(_.surfaceId.contains(lensId(state)))
      .getOrElse(fail("Expected the collapsed lens view"))
    lensView.rect.height shouldBe 3
    lensView.composition.map(_.paintBoxes.size) shouldBe Some(1)
    fullLens.composition.map(_.paintBoxes.size).getOrElse(0) should be > 1
  }

  /** An editable lens holds focus, so it never collapses; a read-only one is a peek and leaves focus in the editor. */
  private def unfocusedLensAndPeek(line: Int): AppState =
    lensAndPeek(line, CommentLensMode.ReadOnly)

  /** A cursor line with room above it for the peek plus a collapsed lens, but not for the peek plus the full lens. */
  private def roomForLensAndCollapsedPeekOnly: Int =
    val reference = lensAndPeek(farFromTop)
    val layout    = layoutOf(reference)
    val gapRows   = rectOf(layout, lensId(reference)).y - rectOf(layout, peekId(reference)).bottom
    val cursorGap = cursorScreenY(reference, layout, farFromTop) - rectOf(layout, lensId(reference)).bottom
    rectOf(layout, peekId(reference)).height + gapRows + 3 + cursorGap
