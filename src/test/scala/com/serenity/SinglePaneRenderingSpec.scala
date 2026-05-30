package com.serenity

import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

class SinglePaneRenderingSpec extends AnyFlatSpec with Matchers:

  behavior of "Single Pane Rendering Layout"

  it should "generate exactly one pane layout for initial state" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      state        <- stateManager.getCurrentState
    yield
      // Verify we start with 1 pane
      state.layout.editorPanes.should(have).size(1)
      
      // When: Calculate layout for rendering
      val viewportSize = ViewportSize(100, 30)
      val calculatedLayout = LayoutEngine.calculateLayout(state, viewportSize)
      val paneLayouts = LayoutEngine.calculatePaneLayouts(state, calculatedLayout)
      
      // Then: Should produce exactly one pane layout
      paneLayouts.should(have).size(1)
      
      val paneId = state.layout.editorPanes.keys.head
      val paneRect = paneLayouts(paneId)
      
      // And the single pane should use the full editor area
      val editorRect = calculatedLayout.editorPanelRect
      paneRect.x.shouldBe(editorRect.x)
      paneRect.y.shouldBe(editorRect.y)
      paneRect.width.shouldBe(editorRect.width)
      paneRect.height.shouldBe(editorRect.height)
      
      println(s"Initial state panes: ${state.layout.editorPanes.size}")
      println(s"Layout generates pane rects: ${paneLayouts.size}")
      println(s"Pane rect: $paneRect")
      println(s"Editor rect: $editorRect")

    program.unsafeRunSync()
  }

  it should "show the difference between state panes and rendered panes" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger           <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager     <- StateManager.apply(logger)
      initialState     <- stateManager.getCurrentState
      _                <- stateManager.applyEvent(com.serenity.keystroke.events.NewTab)
      stateAfterNewTab <- stateManager.getCurrentState
    yield
      // Check initial state
      println(s"=== INITIAL STATE ===")
      println(s"Panes in state: ${initialState.layout.editorPanes.size}")
      println(s"Pane IDs: ${initialState.layout.editorPanes.keys.toList}")
      println(s"Active pane: ${initialState.layout.activeEditorPaneId}")
      println(s"Focus: ${initialState.focus}")
      
      // Check layout calculation
      val viewportSize = ViewportSize(100, 30)
      val calculatedLayout = LayoutEngine.calculateLayout(initialState, viewportSize)
      val paneLayouts = LayoutEngine.calculatePaneLayouts(initialState, calculatedLayout)
      
      println(s"=== CALCULATED LAYOUT ===")
      println(s"Pane layouts generated: ${paneLayouts.size}")
      paneLayouts.foreach { (paneId, rect) =>
        println(s"PaneId($paneId) -> $rect")
      }
      
      println(s"=== AFTER NEW TAB ===")
      println(s"Panes in state: ${stateAfterNewTab.layout.editorPanes.size}")
      println(s"Pane IDs: ${stateAfterNewTab.layout.editorPanes.keys.toList}")
      println(s"Active pane: ${stateAfterNewTab.layout.activeEditorPaneId}")
      println(s"Focus: ${stateAfterNewTab.focus}")
      
      val newTabLayouts = LayoutEngine.calculatePaneLayouts(stateAfterNewTab, calculatedLayout)
      println(s"Pane layouts generated: ${newTabLayouts.size}")
      newTabLayouts.foreach { (paneId, rect) =>
        println(s"PaneId($paneId) -> $rect")
      }
      
      // Assertions
      initialState.layout.editorPanes.should(have).size(1)
      paneLayouts.should(have).size(1)
      
      // NewTab creates a new buffer, not a new pane - pane count stays the same
      stateAfterNewTab.layout.editorPanes.should(have).size(1)
      newTabLayouts.should(have).size(1)
      
      // But the buffer count should increase
      initialState.buffers.should(have).size(1)
      stateAfterNewTab.buffers.should(have).size(2)

    program.unsafeRunSync()
  }