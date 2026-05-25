package com.serenity

import com.serenity.command.CommandRunner
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, LayoutEngine, PeekContent, PeekOverlay, TerminalSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CursorOverlayLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def baseState(cursor: CursorPosition = CursorPosition(6, 18)): AppState =
    val buffer = Buffer.fromString(
      bufferId,
      List.fill(20)("abcdefghijklmnopqrstuvwxyz").mkString("\n")
    ).copy(cursors = List(cursor))
    val pane   = EditorPane.withBuffer(paneId, bufferId)

    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.EditorPane(paneId)
    )

  "LayoutEngine.calculateLayout" should "place a peek overlay above the anchored cursor when space is available" in {
    val state = baseState().copy(
      peekOverlay = Some(
        PeekOverlay(
          PeekContent.QuickInfo("map(value: A => B): List[B]"),
          CursorPosition(6, 18)
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, TerminalSize(100, 30))

    layout.aboveCursorOverlayRect shouldBe defined
    layout.belowCursorOverlayRect shouldBe None

    val rect          = layout.aboveCursorOverlayRect.get
    val paneRect      = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentTopY   = paneRect.y + 1
    val cursorScreenY = contentTopY + 6

    rect.bottom should be <= cursorScreenY
    rect.x should be >= paneRect.x
    rect.right should be <= paneRect.right
    rect.y should be >= contentTopY
  }

  it should "clamp an above-cursor peek overlay into the active pane when the cursor is near the top" in {
    val state = baseState(cursor = CursorPosition(0, 5)).copy(
      peekOverlay = Some(
        PeekOverlay(
          PeekContent.QuickInfo("near-top"),
          CursorPosition(0, 5)
        )
      )
    )

    val layout = LayoutEngine.calculateLayout(state, TerminalSize(100, 20))

    layout.aboveCursorOverlayRect shouldBe defined

    val rect        = layout.aboveCursorOverlayRect.get
    val paneRect    = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentTopY = paneRect.y + 1

    rect.y shouldBe contentTopY
    rect.bottom should be <= paneRect.bottom
  }

  it should "place an active command runner below the anchored cursor when space is available" in {
    val state = baseState().copy(
      focus = Focus.CommandRunner,
      commandRunner = CommandRunner(
        isActive = true,
        searchTerm = "",
        selectedIndex = 0,
        filteredCommands = List.empty
      )
    )

    val layout = LayoutEngine.calculateLayout(state, TerminalSize(100, 30))

    layout.aboveCursorOverlayRect shouldBe None
    layout.belowCursorOverlayRect shouldBe defined

    val rect          = layout.belowCursorOverlayRect.get
    val paneRect      = LayoutEngine.calculatePaneLayouts(state, layout)(paneId)
    val contentTopY   = paneRect.y + 1
    val cursorScreenY = contentTopY + 6

    rect.y should be > cursorScreenY
    rect.x should be >= paneRect.x
    rect.right should be <= paneRect.right
    rect.bottom should be <= paneRect.bottom
  }
end CursorOverlayLayoutSpec
