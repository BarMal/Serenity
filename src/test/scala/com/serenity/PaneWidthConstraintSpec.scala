package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.NewTab
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class PaneWidthConstraintSpec extends AnyFlatSpec with Matchers:

  behavior of "Layout Engine Width Constraints with Buffer Management"

  trait PaneConstraintFixture:
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO]         = Slf4jFactory.create[IO]
    val logger                      = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager                = StateManager.apply(logger).unsafeRunSync()
    val defaultMinPaneWidth         = 50 // Expected default minimum

  it should "enforce minimum pane width of 50 characters by default" in new PaneConstraintFixture:
    // Given: Narrow terminal width that can only fit 1 pane at minimum width
    val viewportSize = ViewportSize(80, 24) // About 70 chars editor area after UI elements
    stateManager
      .updateState(state => state.copy(runtime = state.runtime.copy(viewportSize = Some(viewportSize))))
      .unsafeRunSync()

    // When: Try to create multiple buffers
    stateManager.applyEvent(NewTab).unsafeRunSync() // Create 2nd buffer
    stateManager.applyEvent(NewTab).unsafeRunSync() // Create 3rd buffer
    val finalState = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should have 3 buffers in memory but only 1 pane visible due to width constraint
    finalState.persisted.buffers should have size 3
    finalState.persisted.bufferOrder should have size 3

    // Layout engine should create only 1 pane due to width constraint
    finalState.persisted.layout.editorPanes should have size 1

    // The single pane should be assigned the focused buffer
    val focusedBufferId = finalState.focusedBufferId.get
    val singlePane      = finalState.persisted.layout.editorPanes.values.head
    singlePane.bufferId.get shouldBe focusedBufferId

    // Verify the layout respects minimum width
    val calculatedLayout = LayoutEngine.calculateLayout(finalState, viewportSize)
    val paneLayout       = calculatedLayout.editorPanelRect
    paneLayout.width should be >= defaultMinPaneWidth

  it should "allow more panes when terminal is wider" in new PaneConstraintFixture:
    // Given: Wide terminal that can fit multiple panes at minimum width
    val viewportSize = ViewportSize(200, 24) // About 170 chars editor area, can fit 3+ panes
    stateManager
      .updateState(state => state.copy(runtime = state.runtime.copy(viewportSize = Some(viewportSize))))
      .unsafeRunSync()

    // When: Create multiple buffers
    (1 to 4).foreach(_ => stateManager.applyEvent(NewTab).unsafeRunSync())
    val bufferState = stateManager.getCurrentState.unsafeRunSync()
    val firstPane   = bufferState.persisted.layout.activeEditorPaneId.get
    val secondPane =
      stateManager.splitPaneHorizontal(firstPane, Some(bufferState.persisted.bufferOrder.head)).unsafeRunSync()
    stateManager.splitPaneHorizontal(secondPane, Some(bufferState.persisted.bufferOrder(1))).unsafeRunSync()
    val finalState = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should have all buffers in memory
    finalState.persisted.buffers should have size 5 // Initial + 4 new
    finalState.persisted.bufferOrder should have size 5

    // Layout engine should create multiple panes based on terminal width
    finalState.persisted.layout.editorPanes.size should be > 1
    finalState.persisted.layout.editorPanes.size should be <= 3 // Reasonable maximum based on width

    // Each visible pane should be assigned a buffer
    finalState.persisted.layout.editorPanes.values.foreach { pane =>
      pane.bufferId shouldBe defined
      finalState.persisted.buffers should contain key pane.bufferId.get
    }

    // Verify each pane respects minimum width
    val calculatedLayout = LayoutEngine.calculateLayout(finalState, viewportSize)
    val paneLayouts = LayoutEngine.calculatePaneLayoutsWithMinWidth(finalState, calculatedLayout, defaultMinPaneWidth)

    paneLayouts.values.foreach(rect => rect.width should be >= defaultMinPaneWidth)

  it should "support configurable minimum pane width" in new PaneConstraintFixture:
    // Given: Custom minimum width of 30 characters
    val customMinWidth = 30
    val viewportSize   = ViewportSize(130, 24) // About 110 chars editor area

    // Configure custom minimum width
    stateManager
      .updateState(state =>
        state.copy(
          persisted = state.persisted.copy(config = state.persisted.config.withMinimumPaneWidth(customMinWidth)),
          runtime = state.runtime.copy(viewportSize = Some(viewportSize))
        )
      )
      .unsafeRunSync()

    // When: Create multiple buffers
    (1 to 4).foreach(_ => stateManager.applyEvent(NewTab).unsafeRunSync())
    val bufferState = stateManager.getCurrentState.unsafeRunSync()
    val firstPane   = bufferState.persisted.layout.activeEditorPaneId.get
    val secondPane =
      stateManager.splitPaneHorizontal(firstPane, Some(bufferState.persisted.bufferOrder.head)).unsafeRunSync()
    stateManager.splitPaneHorizontal(secondPane, Some(bufferState.persisted.bufferOrder(1))).unsafeRunSync()
    val finalState = stateManager.getCurrentState.unsafeRunSync()

    // Then: Should respect custom minimum width in layout
    finalState.persisted.buffers should have size 5
    finalState.persisted.config.minimumPaneWidth shouldBe customMinWidth

    // More panes should fit with smaller minimum width
    finalState.persisted.layout.editorPanes.size should be >= 2

    // Verify custom minimum width is respected
    val calculatedLayout = LayoutEngine.calculateLayout(finalState, viewportSize)
    val paneLayouts      = LayoutEngine.calculatePaneLayoutsWithMinWidth(finalState, calculatedLayout, customMinWidth)

    paneLayouts.values.foreach(rect => rect.width should be >= customMinWidth)

  it should "assign focused buffer to visible pane when buffer switching occurs" in new PaneConstraintFixture:
    // Given: Terminal that can show 2 panes, with 4 buffers total
    val viewportSize = ViewportSize(150, 24) // About 130 chars editor area, 2-3 panes possible
    stateManager
      .updateState(state => state.copy(runtime = state.runtime.copy(viewportSize = Some(viewportSize))))
      .unsafeRunSync()

    // Create 4 buffers
    (1 to 3).foreach(_ => stateManager.applyEvent(NewTab).unsafeRunSync())
    val state     = stateManager.getCurrentState.unsafeRunSync()
    val bufferIds = state.persisted.bufferOrder

    bufferIds should have size 4

    // When: Navigate to different buffers using buffer navigation
    state.focusedBufferId.get

    @annotation.tailrec
    def navigateUntil(targetBufferId: BufferId, event: com.serenity.keystroke.events.Event): Unit =
      if stateManager.getCurrentState.unsafeRunSync().focusedBufferId.get != targetBufferId then
        stateManager.applyEvent(event).unsafeRunSync()
        navigateUntil(targetBufferId, event)

    // Navigate to first buffer
    navigateUntil(bufferIds.head, com.serenity.keystroke.events.PreviousTab)
    val stateOnFirstBuffer = stateManager.getCurrentState.unsafeRunSync()

    // Navigate to last buffer
    navigateUntil(bufferIds.last, com.serenity.keystroke.events.NextTab)
    val stateOnLastBuffer = stateManager.getCurrentState.unsafeRunSync()

    // Then: Each time we navigate, the focused buffer should be assigned to a visible pane
    stateOnFirstBuffer.focusedBufferId.get shouldBe bufferIds.head
    stateOnLastBuffer.focusedBufferId.get shouldBe bufferIds.last

    // The focused buffer should be displayed in one of the visible panes
    val visiblePanes               = stateOnLastBuffer.persisted.layout.editorPanes.values
    val focusedBufferInVisiblePane = visiblePanes.exists(_.bufferId.contains(bufferIds.last))
    focusedBufferInVisiblePane shouldBe true

    // Verify minimum width is still respected
    val calculatedLayout = LayoutEngine.calculateLayout(stateOnLastBuffer, viewportSize)
    val paneLayouts =
      LayoutEngine.calculatePaneLayoutsWithMinWidth(stateOnLastBuffer, calculatedLayout, defaultMinPaneWidth)

    paneLayouts.values.foreach(rect => rect.width should be >= defaultMinPaneWidth)

  it should "keep one persistent pane after widening a terminal used for tab creation" in new PaneConstraintFixture:
    val narrowTerminal = ViewportSize(80, 24)
    val wideTerminal   = ViewportSize(200, 24)

    stateManager
      .updateState(state => state.copy(runtime = state.runtime.copy(viewportSize = Some(narrowTerminal))))
      .unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()

    val narrowState = stateManager.getCurrentState.unsafeRunSync()
    narrowState.persisted.buffers should have size 2
    narrowState.persisted.layout.editorPanes should have size 1

    stateManager.handleViewportResize(wideTerminal).unsafeRunSync()

    val widenedState = stateManager.getCurrentState.unsafeRunSync()
    widenedState.persisted.layout.editorPanes.keySet shouldBe narrowState.persisted.layout.editorPanes.keySet
    widenedState.persisted.layout.editorPanes should have size 1
    widenedState.focusedBufferId shouldBe narrowState.focusedBufferId

  it should "preserve explicit pane leaves while resizing through constrained widths" in new PaneConstraintFixture:
    val wideTerminal   = ViewportSize(200, 24)
    val narrowTerminal = ViewportSize(80, 24)

    stateManager
      .updateState(state => state.copy(runtime = state.runtime.copy(viewportSize = Some(wideTerminal))))
      .unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    val bufferState = stateManager.getCurrentState.unsafeRunSync()
    val firstPane   = bufferState.persisted.layout.activeEditorPaneId.get
    stateManager.splitPaneHorizontal(firstPane, Some(bufferState.persisted.bufferOrder.head)).unsafeRunSync()

    val wideState = stateManager.getCurrentState.unsafeRunSync()
    wideState.persisted.layout.editorPanes should have size 2
    val paneIds = wideState.persisted.layout.editorPanes.keySet

    val wideLayout = LayoutEngine.calculateLayout(wideState, wideTerminal)
    val visibleWidePanes =
      LayoutEngine
        .calculatePaneLayouts(wideState, wideLayout)
        .values
        .count(rect => rect.x >= wideLayout.editorPanelRect.x && rect.right <= wideLayout.editorPanelRect.right)
    visibleWidePanes.should(be >= 2)

    stateManager.handleViewportResize(narrowTerminal).unsafeRunSync()

    val narrowState  = stateManager.getCurrentState.unsafeRunSync()
    val narrowLayout = LayoutEngine.calculateLayout(narrowState, narrowTerminal)
    val visibleNarrowPanes =
      LayoutEngine
        .calculatePaneLayouts(narrowState, narrowLayout)
        .values
        .count(rect => rect.x >= narrowLayout.editorPanelRect.x && rect.right <= narrowLayout.editorPanelRect.right)
    visibleNarrowPanes.shouldBe(2)
    narrowState.persisted.layout.editorPanes.keySet shouldBe paneIds

    stateManager.handleViewportResize(wideTerminal).unsafeRunSync()

    val restoredState  = stateManager.getCurrentState.unsafeRunSync()
    val restoredLayout = LayoutEngine.calculateLayout(restoredState, wideTerminal)
    val restoredVisiblePanes =
      LayoutEngine
        .calculatePaneLayouts(restoredState, restoredLayout)
        .values
        .count(rect => rect.x >= restoredLayout.editorPanelRect.x && rect.right <= restoredLayout.editorPanelRect.right)
    restoredVisiblePanes.shouldBe(2)
    restoredState.persisted.layout.editorPanes.keySet shouldBe paneIds
