package com.serenity.state.manager

import com.serenity.keystroke.events.{ModalSubmit, ScrollDown, ScrollRight, ScrollUp}
import com.serenity.rope.{Balance, Rope}
import com.serenity.session.SessionViewport
import com.serenity.state.models.*
import com.serenity.state.reducers.{EditorEventReducer, ModalEventReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A viewport either rests where it was placed or is waiting for the caret to place it. Anything that scrolls it on
  * purpose must leave it `Placed`, or a later resolution would drag the scroll back to the caret.
  */
class ViewportPlacementSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(0)

  private val following =
    Viewport(topLine = 5, leftColumn = 2, visibleLines = 10, visibleColumns = 20, topVisualLine = 1)
      .copy(placement = ViewportPlacement.FollowCaret)

  private def withBuffer(viewport: Viewport): AppState =
    val buffer = AppState.initial.persisted.buffers(bufferId)
    val updated = buffer.copy(
      document = buffer.document.copy(content = Rope((1 to 100).map(i => s"Line $i " + "x" * 60).mkString("\n"))),
      editing = EditingState(List(CursorPosition(50, 0))),
      viewport = viewport
    )
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = AppState.initial.persisted.buffers.updated(bufferId, updated))
    )

  private def viewportIn(state: AppState): Viewport = state.persisted.buffers(bufferId).viewport

  "A viewport" should "start placed" in {
    Viewport.default.placement shouldBe ViewportPlacement.Placed
  }

  "Viewport.scrolledTo" should "set the scroll position and leave the viewport placed" in {
    val scrolled = following.scrolledTo(topLine = 9, leftColumn = 4, topVisualLine = 3)

    scrolled shouldBe Viewport(
      topLine = 9,
      leftColumn = 4,
      visibleLines = 10,
      visibleColumns = 20,
      topVisualLine = 3
    )
    scrolled.placement shouldBe ViewportPlacement.Placed
  }

  "A wheel scroll" should "leave a viewport that was following the caret placed" in {
    val state = withBuffer(following)

    viewportIn(EditorEventReducer.reduce(ScrollDown(3), paneId, state).state).placement shouldBe
      ViewportPlacement.Placed
    viewportIn(EditorEventReducer.reduce(ScrollUp(3), paneId, state).state).placement shouldBe
      ViewportPlacement.Placed
  }

  "A horizontal scroll" should "leave a viewport that was following the caret placed" in {
    val base      = withBuffer(following)
    val unwrapped = base.copy(persisted = base.persisted.copy(config = base.persisted.config.withWordWrap(false)))

    val result = EditorEventReducer.reduce(ScrollRight(1), paneId, unwrapped)

    viewportIn(result.state).leftColumn should be > following.leftColumn
    viewportIn(result.state).placement shouldBe ViewportPlacement.Placed
  }

  "A minimap click" should "leave a viewport that was following the caret placed" in {
    val result = ViewportStateReducer.clickMinimap(paneId, 80, withBuffer(following))

    viewportIn(result.state).topLine shouldBe 75
    viewportIn(result.state).placement shouldBe ViewportPlacement.Placed
  }

  "Go to line" should "leave a viewport that was following the caret placed" in {
    val promptId = SurfaceId("prompt")
    val base     = withBuffer(following)
    val prompting = base.copy(
      persisted = base.persisted.copy(focus = Focus.Surface(promptId)),
      runtime = base.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            promptId,
            SurfaceContent.ModalWorkflow(Modal.TextPrompt(TextPrompt.gotoLine("60"))),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

    val jumped = ModalEventReducer.reduce(ModalType.TextPrompt, ModalSubmit, prompting).state

    viewportIn(jumped).topLine shouldBe 54
    viewportIn(jumped).placement shouldBe ViewportPlacement.Placed
  }

  "A session round trip" should "drop the pending placement" in {
    val restored = SessionViewport.toViewport(SessionViewport.fromViewport(following))

    restored shouldBe following.copy(placement = ViewportPlacement.Placed)
  }
