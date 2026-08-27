package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.NewTab
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class TerminalResizeHandlingSpec extends AnyFlatSpec with Matchers:

  behavior of "Terminal Resize Event Handling"

  trait ResizeFixture:
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO]         = Slf4jFactory.create[IO]
    val logger                      = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager                = StateManager.apply(logger).unsafeRunSync()

  it should "trigger re-layout when terminal is resized" in new ResizeFixture:
    // Given: Wide terminal with multiple buffers
    val wideTerminal = ViewportSize(400, 24)
    stateManager.updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(wideTerminal)))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    val bufferState = stateManager.getCurrentState.unsafeRunSync()
    val firstPane   = bufferState.persisted.layout.activeEditorPaneId.get
    val secondPane =
      stateManager.splitPaneHorizontal(firstPane, Some(bufferState.persisted.bufferOrder.head)).unsafeRunSync()
    stateManager.splitPaneHorizontal(secondPane, Some(bufferState.persisted.bufferOrder(1))).unsafeRunSync()

    val wideState = stateManager.getCurrentState.unsafeRunSync()
    wideState.persisted.buffers should have size 3
    wideState.persisted.layout.editorPanes.size should be > 1 // Multiple panes in wide terminal

    val originalPaneCount = wideState.persisted.layout.editorPanes.size

    // When: Terminal is resized to narrow
    val narrowTerminal = ViewportSize(80, 24)
    stateManager.handleViewportResize(narrowTerminal).unsafeRunSync()

    val narrowState = stateManager.getCurrentState.unsafeRunSync()

    // Then: Geometry is recalculated without changing persistent pane topology.
    narrowState.runtime.viewportSize shouldBe Some(narrowTerminal)
    narrowState.persisted.buffers should have size 3
    narrowState.persisted.layout.editorPanes.size shouldBe originalPaneCount

    // And: All buffers should still be accessible via navigation
    val bufferIds = narrowState.persisted.bufferOrder
    bufferIds should have size 3

  it should "preserve buffer assignment and focus during resize" in new ResizeFixture:
    // Given: Wide terminal with multiple buffers
    val wideTerminal = ViewportSize(400, 24)
    stateManager.updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(wideTerminal)))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()

    val beforeResize              = stateManager.getCurrentState.unsafeRunSync()
    val focusedBufferBeforeResize = beforeResize.focusedBufferId.get
    val bufferOrderBeforeResize   = beforeResize.persisted.bufferOrder

    // When: Terminal is resized
    val narrowTerminal = ViewportSize(100, 24)
    stateManager.handleViewportResize(narrowTerminal).unsafeRunSync()

    val afterResize = stateManager.getCurrentState.unsafeRunSync()

    // Then: Focus and buffer order should be preserved
    afterResize.focusedBufferId.get shouldBe focusedBufferBeforeResize
    afterResize.persisted.bufferOrder shouldBe bufferOrderBeforeResize

    // And: All buffers should still exist
    bufferOrderBeforeResize.foreach(bufferId => afterResize.persisted.buffers should contain key bufferId)

  it should "expand layout when terminal grows wider" in new ResizeFixture:
    // Given: Narrow terminal with multiple buffers (limited panes)
    val narrowTerminal = ViewportSize(80, 24)
    stateManager.updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(narrowTerminal)))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()

    val narrowState       = stateManager.getCurrentState.unsafeRunSync()
    val originalPaneCount = narrowState.persisted.layout.editorPanes.size

    // When: Terminal is resized to wide
    val wideTerminal = ViewportSize(400, 24)
    stateManager.handleViewportResize(wideTerminal).unsafeRunSync()

    val wideState = stateManager.getCurrentState.unsafeRunSync()

    // Then: More panes should be available
    wideState.runtime.viewportSize shouldBe Some(wideTerminal)
    wideState.persisted.layout.editorPanes.size should be >= originalPaneCount // Same or more panes

    // And: Additional buffers should be displayed in new panes
    val assignedBufferIds = wideState.persisted.layout.editorPanes.values.flatMap(_.bufferId).toSet
    assignedBufferIds.size should be >= originalPaneCount

  it should "handle repeated resize events correctly" in new ResizeFixture:
    // Given: Initial state with multiple buffers
    val initialTerminal = ViewportSize(200, 24)
    stateManager
      .updateState(s => s.copy(runtime = s.runtime.copy(viewportSize = Some(initialTerminal))))
      .unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()

    val originalState         = stateManager.getCurrentState.unsafeRunSync()
    val originalBufferOrder   = originalState.persisted.bufferOrder
    val originalFocusedBuffer = originalState.focusedBufferId.get

    // When: Multiple resize events occur
    val sizes = List(
      ViewportSize(80, 24),  // Narrow
      ViewportSize(300, 24), // Wide
      ViewportSize(120, 24), // Medium
      ViewportSize(400, 24)  // Very wide
    )

    sizes.foreach(size => stateManager.handleViewportResize(size).unsafeRunSync())

    val finalState = stateManager.getCurrentState.unsafeRunSync()

    // Then: Buffer state should remain consistent
    finalState.persisted.bufferOrder shouldBe originalBufferOrder
    finalState.persisted.buffers should have size 3
    finalState.focusedBufferId.get shouldBe originalFocusedBuffer
    finalState.runtime.viewportSize shouldBe Some(sizes.last)

    // And: All buffers should still be navigable
    originalBufferOrder.foreach(bufferId => finalState.persisted.buffers should contain key bufferId)

  it should "respect minimum pane width constraints during resize" in new ResizeFixture:
    // Given: Multiple buffers and custom minimum pane width
    val customMinWidth = 60
    stateManager
      .updateState(state =>
        state.copy(
          persisted = state.persisted.copy(config = state.persisted.config.withMinimumPaneWidth(customMinWidth)),
          runtime = state.runtime.copy(viewportSize = Some(ViewportSize(300, 24)))
        )
      )
      .unsafeRunSync()

    (1 to 4).foreach(_ => stateManager.applyEvent(NewTab).unsafeRunSync())

    // When: Terminal is resized to various widths
    val testWidths = List(80, 120, 180, 240, 360)

    testWidths.foreach { width =>
      val viewportSize = ViewportSize(width, 24)
      stateManager.handleViewportResize(viewportSize).unsafeRunSync()

      val state       = stateManager.getCurrentState.unsafeRunSync()
      val layout      = com.serenity.ui.layout.LayoutEngine.calculateLayout(state, viewportSize)
      val paneLayouts = com.serenity.ui.layout.LayoutEngine.calculatePaneLayouts(state, layout)

      // Then: All visible panes should respect minimum width
      paneLayouts.values.foreach(rect => rect.width should be >= customMinWidth)
    }
