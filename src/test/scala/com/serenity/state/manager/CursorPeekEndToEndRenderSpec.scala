package com.serenity.state.manager

import com.serenity.command.CommandRegistry
import com.serenity.keystroke.Modifier
import com.serenity.keystroke.events.CursorPeekModifierPressed
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.AppEventReducer
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end confirmation that the cursor-peek prototype actually produces something to paint: reducing a
  * `CursorPeekModifierPressed` (as `SwingInputHandler` would emit it), resolving the frozen anchor (as
  * `StateManagerEventPipeline` does right after), and finally calling `LayoutEngine.calculateLayout` -- the exact
  * entry point both the GUI `Renderer` and the TUI painter share, since neither has its own separate layout pass --
  * yields a real, positioned rect for the peek surface in `belowCursorOverlayStack`/`aboveCursorOverlayStack`.
  */
class CursorPeekEndToEndRenderSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.withToggleUI

  private def enabledStateWithBuffer: AppState =
    val buffer = Buffer.fromString(BufferId(1), "alpha\nbeta\ngamma\ndelta\nepsilon")
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        ),
        focus = Focus.EditorPane(PaneId(0)),
        config = AppState.initial.persisted.config.withCommandRunnerCursorPeekEnabled(true)
      ),
      runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(100, 30)))
    )

  private def peekRectIn(layout: CalculatedLayout): Option[LayoutRect] =
    (layout.aboveCursorOverlayStack ++ layout.belowCursorOverlayStack)
      .collectFirst { case (id, rect) if id == SurfaceId.CursorPeek => rect }

  "a peek begun end-to-end" should "render as a real, positioned overlay rect via the shared LayoutEngine pass" in {
    val initial = enabledStateWithBuffer

    val pressed  = AppEventReducer.reduce(CursorPeekModifierPressed(Modifier.Meta, 0L), initial, registry).state
    val resolved = CursorPeekAnchorResolution.resolve(pressed)

    resolved.runtime.cursorPeekResolvedAnchor shouldBe defined
    val peekSurface = resolved.runtime.uiSurfaces.find(_.id == SurfaceId.CursorPeek)
    peekSurface shouldBe defined

    val layout   = LayoutEngine.calculateLayout(resolved, ViewportSize(100, 30))
    val peekRect = peekRectIn(layout)

    peekRect shouldBe defined
    peekRect.foreach { rect =>
      rect.width should be > 0
      rect.height should be > 0
    }
  }

  it should "keep rendering the peek at the same position even if the document reflows underneath it" in {
    val initial = enabledStateWithBuffer

    val pressed  = AppEventReducer.reduce(CursorPeekModifierPressed(Modifier.Meta, 0L), initial, registry).state
    val resolved = CursorPeekAnchorResolution.resolve(pressed)

    val firstLayout = LayoutEngine.calculateLayout(resolved, ViewportSize(100, 30))
    val firstRect   = peekRectIn(firstLayout).getOrElse(fail("expected a peek rect"))

    val reformattedBuffer = Buffer.fromString(
      BufferId(1),
      "a much longer first line that would reflow visual positions if it were re-derived live\nbeta\ngamma\ndelta\nepsilon"
    )
    val reformatted = resolved.copy(persisted =
      resolved.persisted.copy(buffers = resolved.persisted.buffers.updated(BufferId(1), reformattedBuffer))
    )
    // The anchor is already resolved and cached -- re-resolving is a no-op, matching what StateManagerEventPipeline
    // would do on every subsequent render tick.
    val reResolved = CursorPeekAnchorResolution.resolve(reformatted)

    val secondLayout = LayoutEngine.calculateLayout(reResolved, ViewportSize(100, 30))
    val secondRect    = peekRectIn(secondLayout).getOrElse(fail("expected a peek rect"))

    secondRect shouldBe firstRect
  }
