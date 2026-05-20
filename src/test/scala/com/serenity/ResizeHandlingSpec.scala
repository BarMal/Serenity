package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.ResizeEvent
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.{LayoutEngine, TerminalSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResizeHandlingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "ResizeEvent" should "be created from terminal size change" in {
    val newWidth  = 120
    val newHeight = 40

    val resizeEvent = ResizeEvent(TerminalSize(newWidth, newHeight))

    resizeEvent.newSize.width shouldBe newWidth
    resizeEvent.newSize.height shouldBe newHeight
  }

  it should "trigger layout recalculation when applied to state manager" in {
    // Create state manager with initial buffer and pane
    val stateManager = StateManager.apply.unsafeRunSync()
    val bufferId     = stateManager.createBuffer("Initial content").unsafeRunSync()
    val paneId       = stateManager.createPane(Some(bufferId)).unsafeRunSync()

    // Get initial state and verify initial layout
    val initialState  = stateManager.getCurrentState.unsafeRunSync()
    val initialLayout = LayoutEngine.calculateLayout(initialState, TerminalSize(80, 24))

    initialLayout.editorPanelRect.width shouldBe 56 // 80 - 2*12 (15% spacers)
    initialLayout.editorPanelRect.height shouldBe 24

    // Apply resize event
    val newSize     = TerminalSize(120, 40)
    val resizeEvent = ResizeEvent(newSize)
    stateManager.applyEvent(resizeEvent).unsafeRunSync()

    // Get updated state and verify layout was recalculated
    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val newLayout    = LayoutEngine.calculateLayout(updatedState, newSize)

    newLayout.editorPanelRect.width shouldBe 84 // 120 - 2*18 (15% spacers)
    newLayout.editorPanelRect.height shouldBe 40

    // Verify that editor pane viewport was updated
    updatedState.layout.editorPanes.values.headOption match
      case Some(pane) =>
        pane.viewport.visibleLines.shouldBe(40)
        pane.viewport.visibleColumns.shouldBe(84)
      case None => fail("No pane found in state")
  }

  it should "handle text wrapping recalculation on resize" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    // Create a long line of text that will wrap differently at different widths
    val longText =
      "This is a very long line of text that should wrap differently when the terminal width changes and we need to test that the rope structure handles this properly"
    val bufferId = stateManager.createBuffer(longText).unsafeRunSync()
    val paneId   = stateManager.createPane(Some(bufferId)).unsafeRunSync()

    // Start with narrow width (40 chars)
    val initialSize  = TerminalSize(40, 20)
    val resizeEvent1 = ResizeEvent(initialSize)
    stateManager.applyEvent(resizeEvent1).unsafeRunSync()

    val state1  = stateManager.getCurrentState.unsafeRunSync()
    val layout1 = LayoutEngine.calculateLayout(state1, initialSize)

    // Now resize to wider (120 chars)
    val widerSize    = TerminalSize(120, 20)
    val resizeEvent2 = ResizeEvent(widerSize)
    stateManager.applyEvent(resizeEvent2).unsafeRunSync()

    val state2  = stateManager.getCurrentState.unsafeRunSync()
    val layout2 = LayoutEngine.calculateLayout(state2, widerSize)

    // Verify that layout dimensions changed
    layout1.editorPanelRect.width should be < layout2.editorPanelRect.width

    // Verify viewport dimensions were updated in state
    state2.layout.editorPanes.values.headOption match
      case Some(pane) =>
        pane.viewport.visibleColumns.shouldBe(layout2.editorPanelRect.width)
      case None => fail("No pane found in state")
  }

  it should "detect resize from terminal input" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    // Initial state - has default viewport dimensions
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    initialState.layout.editorPanes.values.headOption match
      case Some(pane) =>
        // Get initial dimensions (should be non-zero default values)
        val initialLines   = pane.viewport.visibleLines
        val initialColumns = pane.viewport.visibleColumns
        initialLines should be > 0
        initialColumns should be > 0
      case None => fail("No pane found in initial state")

    // Simulate terminal resize detection
    val newTerminalSize = TerminalSize(100, 30)
    val resizeEvent     = ResizeEvent(newTerminalSize)

    // Apply the resize event
    stateManager.applyEvent(resizeEvent).unsafeRunSync()

    // State should now reflect the resize
    val resizedState = stateManager.getCurrentState.unsafeRunSync()
    resizedState.layout.editorPanes.values.headOption match
      case Some(pane) =>
        // After resize, viewport dimensions should be updated
        pane.viewport.visibleLines.shouldBe(30)   // Full height
        pane.viewport.visibleColumns.shouldBe(70) // 100 - 30% for spacers
      case None => fail("No pane found after resize")
  }

  it should "recalculate text wrapping when terminal width changes" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    // Create buffer with text that will wrap at narrow width
    val longLine =
      "This is a very long line that should definitely wrap when displayed in a narrow terminal window but should fit on one line in a wide terminal"
    val bufferId = stateManager.createBuffer(longLine).unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Start with narrow terminal (40 chars wide)
    val narrowSize   = TerminalSize(40, 20)
    val resizeEvent1 = ResizeEvent(narrowSize)
    stateManager.applyEvent(resizeEvent1).unsafeRunSync()

    val stateAfterNarrow  = stateManager.getCurrentState.unsafeRunSync()
    val layoutAfterNarrow = LayoutEngine.calculateLayout(stateAfterNarrow, narrowSize)

    // Resize to wide terminal (120 chars wide)
    val wideSize     = TerminalSize(120, 20)
    val resizeEvent2 = ResizeEvent(wideSize)
    stateManager.applyEvent(resizeEvent2).unsafeRunSync()

    val stateAfterWide  = stateManager.getCurrentState.unsafeRunSync()
    val layoutAfterWide = LayoutEngine.calculateLayout(stateAfterWide, wideSize)

    // Verify that layout calculations reflect the size change
    layoutAfterNarrow.editorPanelRect.width.should(be < layoutAfterWide.editorPanelRect.width)

    // Verify that the state has been updated to reflect new viewport
    stateAfterWide.layout.editorPanes.values.headOption match
      case Some(pane) =>
        pane.viewport.visibleColumns.shouldBe(layoutAfterWide.editorPanelRect.width)
        pane.viewport.visibleLines.shouldBe(layoutAfterWide.editorPanelRect.height)
      case None => fail("No pane found after wide resize")
  }
