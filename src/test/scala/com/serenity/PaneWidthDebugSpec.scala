package com.serenity

import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.*
import com.serenity.keystroke.events.NewTab
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

class PaneWidthDebugSpec extends AnyFlatSpec with Matchers:

  behavior of "Debug Pane Width Behavior"

  trait DebugFixture:
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager.apply(logger).unsafeRunSync()

  it should "debug the actual behavior when creating many small panes" in new DebugFixture {
    // Given: Small terminal that you mentioned (dozens of characters per line)
    val viewportSize = ViewportSize(80, 24) // Typical small terminal
    val editorArea = (viewportSize.width * 0.7).toInt // 56 chars after spacers
    println(s"Terminal width: ${viewportSize.width}, Editor area: $editorArea")
    
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    println(s"Initial: ${initialState.layout.editorPanes.size} panes")
    
    // When: Create 6 panes like you mentioned
    (1 to 5).foreach { i =>
      stateManager.applyEvent(NewTab).unsafeRunSync()
      val state = stateManager.getCurrentState.unsafeRunSync()
      println(s"After NewTab $i: ${state.layout.editorPanes.size} panes")
    }
    
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    println(s"Final state: ${finalState.layout.editorPanes.size} panes, ${finalState.buffers.size} buffers")
    
    // Check what the layout engine produces
    val calculatedLayout = LayoutEngine.calculateLayout(finalState, viewportSize)
    val paneLayouts1 = LayoutEngine.calculatePaneLayouts(finalState, calculatedLayout)
    val paneLayouts50 = LayoutEngine.calculatePaneLayoutsWithMinWidth(finalState, calculatedLayout, 50)
    val paneLayouts1Actual = LayoutEngine.calculatePaneLayoutsWithMinWidth(finalState, calculatedLayout, 1)
    
    println(s"\n=== Layout Results ===")
    println(s"Default calculatePaneLayouts: ${paneLayouts1.size} layouts")
    paneLayouts1.foreach { (paneId, rect) =>
      println(s"  PaneId(${paneId.value}) -> x=${rect.x}, width=${rect.width}")
    }
    
    println(s"\nWith 50 char minimum: ${paneLayouts50.size} layouts")
    paneLayouts50.foreach { (paneId, rect) =>
      println(s"  PaneId(${paneId.value}) -> x=${rect.x}, width=${rect.width}")
    }
    
    println(s"\nWith 1 char minimum: ${paneLayouts1Actual.size} layouts")
    paneLayouts1Actual.foreach { (paneId, rect) =>
      println(s"  PaneId(${paneId.value}) -> x=${rect.x}, width=${rect.width}")
    }
    
    // Count visible vs hidden panes with 1 char minimum (should show the problem)
    val editorRect = calculatedLayout.editorPanelRect
    val visiblePanes1 = paneLayouts1Actual.values.count(rect => 
      rect.x >= editorRect.x && rect.x < (editorRect.x + editorRect.width)
    )
    
    println(s"\nWith 1 char minimum: $visiblePanes1 visible panes out of ${finalState.layout.editorPanes.size}")
    println(s"Expected problem: All 6 panes visible with ~${editorArea/6} chars each = ${editorArea/6} chars per pane")
    
    // This should demonstrate the issue you're seeing
    if (visiblePanes1 == finalState.layout.editorPanes.size) {
      println("ISSUE REPRODUCED: All panes are visible with tiny widths!")
      val avgWidth = paneLayouts1Actual.values.map(_.width).sum / paneLayouts1Actual.size
      println(s"Average pane width: $avgWidth characters")
      
      if (avgWidth < 20) {
        println("CONFIRMED: Panes are too narrow (< 20 chars)")
      }
    }
  }
