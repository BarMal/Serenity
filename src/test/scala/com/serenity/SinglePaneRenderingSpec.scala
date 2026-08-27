package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class SinglePaneRenderingSpec extends AnyFlatSpec with Matchers:

  behavior of "Single Pane Rendering Layout"

  it should "generate exactly one pane layout for initial state" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO]         = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      state        <- stateManager.getCurrentState
    yield
      // Verify we start with 1 pane
      state.persisted.layout.editorPanes.should(have).size(1)

      // When: Calculate layout for rendering
      val viewportSize     = ViewportSize(100, 30)
      val calculatedLayout = LayoutEngine.calculateLayout(state, viewportSize)
      val paneLayouts      = LayoutEngine.calculatePaneLayouts(state, calculatedLayout)

      // Then: Should produce exactly one pane layout
      paneLayouts.should(have).size(1)

      val paneId   = state.persisted.layout.editorPanes.keys.head
      val paneRect = paneLayouts(paneId)

      // And the single pane should use the full editor area
      val editorRect = calculatedLayout.editorPanelRect
      paneRect.x.shouldBe(editorRect.x)
      paneRect.y.shouldBe(editorRect.y)
      paneRect.width.shouldBe(editorRect.width)
      paneRect.height.shouldBe(editorRect.height)

    program.unsafeRunSync()
  }

  it should "keep one rendered pane when a new tab adds a buffer" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO]         = Slf4jFactory.create[IO]

    val program = for
      logger           <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager     <- StateManager.apply(logger)
      initialState     <- stateManager.getCurrentState
      _                <- stateManager.applyEvent(com.serenity.keystroke.events.NewTab)
      stateAfterNewTab <- stateManager.getCurrentState
    yield
      val viewportSize     = ViewportSize(100, 30)
      val calculatedLayout = LayoutEngine.calculateLayout(initialState, viewportSize)
      val paneLayouts      = LayoutEngine.calculatePaneLayouts(initialState, calculatedLayout)
      val newTabLayout     = LayoutEngine.calculateLayout(stateAfterNewTab, viewportSize)
      val newTabLayouts    = LayoutEngine.calculatePaneLayouts(stateAfterNewTab, newTabLayout)

      // Assertions
      initialState.persisted.layout.editorPanes.should(have).size(1)
      paneLayouts.should(have).size(1)

      // NewTab creates a new buffer, not a new pane - pane count stays the same
      stateAfterNewTab.persisted.layout.editorPanes.should(have).size(1)
      newTabLayouts.should(have).size(1)

      // But the buffer count should increase
      initialState.persisted.buffers.should(have).size(1)
      stateAfterNewTab.persisted.buffers.should(have).size(2)

    program.unsafeRunSync()
  }
