package com.serenity

import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.keystroke.events.NewTab
import com.serenity.ui.layout.TerminalSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

class TerminalResizeHandlingSpec extends AnyFlatSpec with Matchers:

  behavior of "Terminal Resize Event Handling"

  trait ResizeFixture:
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager.apply(logger).unsafeRunSync()

  it should "trigger re-layout when terminal is resized" in new ResizeFixture {
    // Given: Wide terminal with multiple buffers
    val wideTerminal = TerminalSize(400, 24)
    stateManager.updateState(_.copy(terminalSize = Some(wideTerminal))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    
    val wideState = stateManager.getCurrentState.unsafeRunSync()
    wideState.buffers should have size 3
    wideState.layout.editorPanes.size should be > 1 // Multiple panes in wide terminal
    
    val originalPaneCount = wideState.layout.editorPanes.size
    
    // When: Terminal is resized to narrow
    val narrowTerminal = TerminalSize(80, 24)
    stateManager.handleTerminalResize(narrowTerminal).unsafeRunSync()
    
    val narrowState = stateManager.getCurrentState.unsafeRunSync()
    
    // Then: Layout should be recalculated with fewer panes
    narrowState.terminalSize shouldBe Some(narrowTerminal)
    narrowState.buffers should have size 3 // Buffers preserved
    narrowState.layout.editorPanes.size should be <= originalPaneCount // Fewer or same panes
    
    // And: All buffers should still be accessible via navigation
    val bufferIds = narrowState.bufferOrder
    bufferIds should have size 3
  }

  it should "preserve buffer assignment and focus during resize" in new ResizeFixture {
    // Given: Wide terminal with multiple buffers
    val wideTerminal = TerminalSize(400, 24)
    stateManager.updateState(_.copy(terminalSize = Some(wideTerminal))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    
    val beforeResize = stateManager.getCurrentState.unsafeRunSync()
    val focusedBufferBeforeResize = beforeResize.focusedBufferId.get
    val bufferOrderBeforeResize = beforeResize.bufferOrder
    
    // When: Terminal is resized
    val narrowTerminal = TerminalSize(100, 24)
    stateManager.handleTerminalResize(narrowTerminal).unsafeRunSync()
    
    val afterResize = stateManager.getCurrentState.unsafeRunSync()
    
    // Then: Focus and buffer order should be preserved
    afterResize.focusedBufferId.get shouldBe focusedBufferBeforeResize
    afterResize.bufferOrder shouldBe bufferOrderBeforeResize
    
    // And: All buffers should still exist
    bufferOrderBeforeResize.foreach { bufferId =>
      afterResize.buffers should contain key bufferId
    }
  }

  it should "expand layout when terminal grows wider" in new ResizeFixture {
    // Given: Narrow terminal with multiple buffers (limited panes)
    val narrowTerminal = TerminalSize(80, 24)
    stateManager.updateState(_.copy(terminalSize = Some(narrowTerminal))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    
    val narrowState = stateManager.getCurrentState.unsafeRunSync()
    val originalPaneCount = narrowState.layout.editorPanes.size
    
    // When: Terminal is resized to wide
    val wideTerminal = TerminalSize(400, 24)
    stateManager.handleTerminalResize(wideTerminal).unsafeRunSync()
    
    val wideState = stateManager.getCurrentState.unsafeRunSync()
    
    // Then: More panes should be available
    wideState.terminalSize shouldBe Some(wideTerminal)
    wideState.layout.editorPanes.size should be >= originalPaneCount // Same or more panes
    
    // And: Additional buffers should be displayed in new panes
    val assignedBufferIds = wideState.layout.editorPanes.values.flatMap(_.bufferId).toSet
    assignedBufferIds.size should be >= originalPaneCount
  }

  it should "handle repeated resize events correctly" in new ResizeFixture {
    // Given: Initial state with multiple buffers
    val initialTerminal = TerminalSize(200, 24)
    stateManager.updateState(_.copy(terminalSize = Some(initialTerminal))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync()
    
    val originalState = stateManager.getCurrentState.unsafeRunSync()
    val originalBufferOrder = originalState.bufferOrder
    val originalFocusedBuffer = originalState.focusedBufferId.get
    
    // When: Multiple resize events occur
    val sizes = List(
      TerminalSize(80, 24),   // Narrow
      TerminalSize(300, 24),  // Wide
      TerminalSize(120, 24),  // Medium
      TerminalSize(400, 24)   // Very wide
    )
    
    sizes.foreach { size =>
      stateManager.handleTerminalResize(size).unsafeRunSync()
    }
    
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    
    // Then: Buffer state should remain consistent
    finalState.bufferOrder shouldBe originalBufferOrder
    finalState.buffers should have size 3
    finalState.focusedBufferId.get shouldBe originalFocusedBuffer
    finalState.terminalSize shouldBe Some(sizes.last)
    
    // And: All buffers should still be navigable
    originalBufferOrder.foreach { bufferId =>
      finalState.buffers should contain key bufferId
    }
  }

  it should "respect minimum pane width constraints during resize" in new ResizeFixture {
    // Given: Multiple buffers and custom minimum pane width
    val customMinWidth = 60
    stateManager.updateState(state =>
      state.copy(
        terminalSize = Some(TerminalSize(300, 24)),
        config = state.config.withMinimumPaneWidth(customMinWidth)
      )
    ).unsafeRunSync()
    
    (1 to 4).foreach(_ => stateManager.applyEvent(NewTab).unsafeRunSync())
    
    // When: Terminal is resized to various widths
    val testWidths = List(80, 120, 180, 240, 360)
    
    testWidths.foreach { width =>
      val terminalSize = TerminalSize(width, 24)
      stateManager.handleTerminalResize(terminalSize).unsafeRunSync()
      
      val state = stateManager.getCurrentState.unsafeRunSync()
      val layout = com.serenity.ui.layout.LayoutEngine.calculateLayout(state, terminalSize)
      val paneLayouts = com.serenity.ui.layout.LayoutEngine.calculatePaneLayouts(state, layout)
      
      // Then: All visible panes should respect minimum width
      paneLayouts.values.foreach { rect =>
        rect.width should be >= customMinWidth
      }
    }
  }