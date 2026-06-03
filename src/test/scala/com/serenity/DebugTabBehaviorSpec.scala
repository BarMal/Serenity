package com.serenity

import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.keystroke.events.{NewTab, NextTab, PreviousTab}
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

class DebugTabBehaviorSpec extends AnyFlatSpec with Matchers:

  behavior of "Debug Tab Behavior"

  it should "show what happens when creating a new tab" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger = LoggerFactory[IO].getLogger(using LoggerName("DebugTest"))
    val stateManager = StateManager.apply(logger).unsafeRunSync()
    
    // Set narrow terminal size to test single-pane behavior
    stateManager.updateState(_.copy(viewportSize = Some(ViewportSize(80, 24)))).unsafeRunSync()

    // Initial state
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    println(s"=== INITIAL STATE ===")
    println(s"Buffers: ${initialState.buffers.keys.toList.sortBy(_.value)}")
    println(s"BufferOrder: ${initialState.bufferOrder}")
    println(s"Panes: ${initialState.layout.editorPanes.keys.toList.sortBy(_.value)}")
    println(s"Focused: ${initialState.focusedBufferId}")
    
    // Create new tab
    println(s"\n=== CREATING NEW TAB ===")
    stateManager.applyEvent(NewTab).unsafeRunSync()
    val stateAfterNewTab = stateManager.getCurrentState.unsafeRunSync()
    
    println(s"Buffers: ${stateAfterNewTab.buffers.keys.toList.sortBy(_.value)}")
    println(s"BufferOrder: ${stateAfterNewTab.bufferOrder}")
    println(s"Panes: ${stateAfterNewTab.layout.editorPanes.keys.toList.sortBy(_.value)}")
    println(s"Focused: ${stateAfterNewTab.focusedBufferId}")
    println(s"FocusObject: ${stateAfterNewTab.focus}")
    println(s"ActiveEditorPaneId: ${stateAfterNewTab.layout.activeEditorPaneId}")
    
    // Show pane-buffer assignments
    stateAfterNewTab.layout.editorPanes.foreach { case (paneId, pane) =>
      println(s"  Pane $paneId -> Buffer ${pane.bufferId}")
    }
    
    // Try manually calling nextBuffer to see if buffer order is working
    println(s"\n=== TESTING BUFFER ORDER LOGIC ===")
    val currentBufferId = BufferId(0)
    val nextBuffer = stateAfterNewTab.nextBufferInOrder(currentBufferId)
    val prevBuffer = stateAfterNewTab.previousBufferInOrder(currentBufferId)
    println(s"Current: $currentBufferId, Next: $nextBuffer, Prev: $prevBuffer")
  }