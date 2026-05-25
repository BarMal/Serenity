package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, LayoutEngine, PeekContent, PeekOverlay, TerminalSize}
import com.serenity.ui.renderer.OverlayViewModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OverlayViewModelSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWithQuickInfo(text: String): AppState =
    val buffer = Buffer.fromString(bufferId, "one\ntwo\nthree").copy(
      cursors = List(CursorPosition(1, 2))
    )
    val pane   = EditorPane.withBuffer(paneId, bufferId)

    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.PeekOverlay,
      peekOverlay = Some(PeekOverlay(PeekContent.QuickInfo(text), CursorPosition(1, 2)))
    )

  "OverlayViewModel.fromState" should "derive an above-cursor quick-info overlay view from peek state" in {
    val state  = stateWithQuickInfo("List.map(f)")
    val layout = LayoutEngine.calculateLayout(state, TerminalSize(100, 24))

    val overlays = OverlayViewModel.fromState(state, layout)

    overlays.aboveCursor shouldBe defined
    overlays.belowCursor shouldBe None

    val overlay = overlays.aboveCursor.get
    overlay.lines shouldBe List("List.map(f)")
    overlay.rect shouldBe layout.aboveCursorOverlayRect.get
  }
