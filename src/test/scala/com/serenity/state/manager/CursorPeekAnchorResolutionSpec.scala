package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{ScreenPosition, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers `CursorPeekAnchorResolution.resolve`: the render-time (not reducer-time -- reducers may not reach
  * `LayoutEngine`) resolution of the cursor-peek prototype's frozen `CursorPosition` to an actual on-screen
  * `ScreenPosition`, done once and cached rather than re-derived on every subsequent call -- the mechanism that lets
  * a frozen peek anchor survive a reformat underneath it.
  */
class CursorPeekAnchorResolutionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def stateWithBuffer(text: String, cursorLine: Int, cursorColumn: Int): AppState =
    val buffer = Buffer.fromString(BufferId(1), text)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        )
      ),
      runtime = AppState.initial.runtime.copy(
        viewportSize = Some(ViewportSize(100, 30)),
        cursorPeekAnchor = Some(CursorPosition(cursorLine, cursorColumn))
      )
    )

  "resolve" should "leave state untouched when no peek anchor is pending" in {
    val state = AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(100, 30))))

    CursorPeekAnchorResolution.resolve(state) shouldBe state
  }

  it should "resolve a pending cursor anchor to a screen position exactly once" in {
    val state = stateWithBuffer("alpha\nbeta\ngamma", cursorLine = 1, cursorColumn = 2)

    val resolved = CursorPeekAnchorResolution.resolve(state)

    resolved.runtime.cursorPeekResolvedAnchor shouldBe defined
  }

  it should "never re-derive an already-resolved anchor, even if the underlying document changes" in {
    val state    = stateWithBuffer("alpha\nbeta\ngamma", cursorLine = 1, cursorColumn = 2)
    val resolved = CursorPeekAnchorResolution.resolve(state)
    val firstResolvedAnchor = resolved.runtime.cursorPeekResolvedAnchor.getOrElse(fail("expected a resolved anchor"))

    // Simulate a reformat: the buffer content underneath changes, but cursorPeekAnchor (the frozen logical
    // position) and cursorPeekResolvedAnchor (the frozen screen position) are left as they were.
    val reformattedBuffer = Buffer.fromString(BufferId(1), "a much longer first line that reflows everything\nbeta\ngamma")
    val reformatted = resolved.copy(persisted =
      resolved.persisted.copy(buffers = resolved.persisted.buffers.updated(BufferId(1), reformattedBuffer))
    )

    val reResolved = CursorPeekAnchorResolution.resolve(reformatted)

    reResolved.runtime.cursorPeekResolvedAnchor shouldBe Some(firstResolvedAnchor)
  }

  it should "not resolve when the viewport size is not yet known" in {
    val state = stateWithBuffer("alpha\nbeta", 0, 0).copy(
      runtime = stateWithBuffer("alpha\nbeta", 0, 0).runtime.copy(viewportSize = None)
    )

    CursorPeekAnchorResolution.resolve(state).runtime.cursorPeekResolvedAnchor shouldBe None
  }

  it should "not resolve when there is no active editor pane" in {
    val state = stateWithBuffer("alpha\nbeta", 0, 0)
    val withoutPane =
      state.copy(persisted = state.persisted.copy(layout = state.persisted.layout.copy(activeEditorPaneId = None)))

    CursorPeekAnchorResolution.resolve(withoutPane).runtime.cursorPeekResolvedAnchor shouldBe None
  }
