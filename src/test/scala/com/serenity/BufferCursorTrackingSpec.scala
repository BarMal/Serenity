package com.serenity

import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.keystroke.events.{NewTab, InsertChar, NextTab, PreviousTab}
import com.serenity.ui.layout.TerminalSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

class BufferCursorTrackingSpec extends AnyFlatSpec with Matchers:

  behavior of "Buffer-specific Cursor Position Tracking"

  trait CursorTrackingFixture:
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager.apply(logger).unsafeRunSync()
    val wideTerminal = TerminalSize(400, 24) // Wide enough for multiple panes

  it should "maintain cursor position per buffer when switching between buffers" in new CursorTrackingFixture {
    // Given: Wide terminal and two buffers with different content
    stateManager.updateState(_.copy(terminalSize = Some(wideTerminal))).unsafeRunSync()
    
    // Type some text in first buffer
    stateManager.applyEvent(InsertChar('A')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('B')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('C')).unsafeRunSync()
    
    val stateAfterFirstBuffer = stateManager.getCurrentState.unsafeRunSync()
    val firstBufferId = stateAfterFirstBuffer.bufferOrder.head
    val firstBuffer = stateAfterFirstBuffer.buffers(firstBufferId)
    
    // Should have "ABC" and cursor at position 3
    firstBuffer.content.collect() shouldBe "ABC"
    firstBuffer.cursors.head.column shouldBe 3
    firstBuffer.cursors.head.line shouldBe 0
    
    // Create second buffer and type different content
    stateManager.applyEvent(NewTab).unsafeRunSync()
    stateManager.applyEvent(InsertChar('X')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('Y')).unsafeRunSync()
    
    val stateAfterSecondBuffer = stateManager.getCurrentState.unsafeRunSync()
    val secondBufferId = stateAfterSecondBuffer.bufferOrder.last
    val secondBuffer = stateAfterSecondBuffer.buffers(secondBufferId)
    
    // Should have "XY" and cursor at position 2
    secondBuffer.content.collect() shouldBe "XY"
    secondBuffer.cursors.head.column shouldBe 2
    secondBuffer.cursors.head.line shouldBe 0
    
    // When: Switch back to first buffer
    stateManager.applyEvent(PreviousTab).unsafeRunSync()
    val stateAfterSwitch = stateManager.getCurrentState.unsafeRunSync()
    
    val firstBufferAfterSwitch = stateAfterSwitch.buffers(firstBufferId)
    val secondBufferAfterSwitch = stateAfterSwitch.buffers(secondBufferId)
    
    // Then: Both buffers should maintain their cursor positions
    firstBufferAfterSwitch.cursors.head.column shouldBe 3 // Still at end of "ABC"
    firstBufferAfterSwitch.cursors.head.line shouldBe 0
    secondBufferAfterSwitch.cursors.head.column shouldBe 2 // Still at end of "XY"
    secondBufferAfterSwitch.cursors.head.line shouldBe 0
    
    // And: Content should be preserved
    firstBufferAfterSwitch.content.collect() shouldBe "ABC"
    secondBufferAfterSwitch.content.collect() shouldBe "XY"
  }

  it should "track viewport position per buffer" in new CursorTrackingFixture {
    // Given: Wide terminal and two buffers
    stateManager.updateState(_.copy(terminalSize = Some(wideTerminal))).unsafeRunSync()
    
    // Add many lines to first buffer to trigger scrolling
    (1 to 30).foreach { i =>
      stateManager.applyEvent(InsertChar('A')).unsafeRunSync()
      stateManager.applyEvent(com.serenity.keystroke.events.NewLine).unsafeRunSync()
    }
    
    val stateAfterScrolling = stateManager.getCurrentState.unsafeRunSync()
    val firstBufferId = stateAfterScrolling.bufferOrder.head
    val firstBuffer = stateAfterScrolling.buffers(firstBufferId)
    val firstViewport = firstBuffer.viewport
    
    // Create second buffer (should have default viewport)
    stateManager.applyEvent(NewTab).unsafeRunSync()
    val stateWithSecondBuffer = stateManager.getCurrentState.unsafeRunSync()
    val secondBufferId = stateWithSecondBuffer.bufferOrder.last
    val secondBuffer = stateWithSecondBuffer.buffers(secondBufferId)
    
    // When: Switch back to first buffer
    stateManager.applyEvent(PreviousTab).unsafeRunSync()
    val stateAfterSwitchBack = stateManager.getCurrentState.unsafeRunSync()
    
    val firstBufferAfterSwitch = stateAfterSwitchBack.buffers(firstBufferId)
    val secondBufferAfterSwitch = stateAfterSwitchBack.buffers(secondBufferId)
    
    // Then: Each buffer should maintain its viewport
    firstBufferAfterSwitch.viewport.topLine should be > 0 // Should be scrolled
    secondBufferAfterSwitch.viewport.topLine shouldBe 0 // Should be at top
  }

  it should "handle cursor position when adding content to different buffers" in new CursorTrackingFixture {
    // Given: Wide terminal and multiple buffers
    stateManager.updateState(_.copy(terminalSize = Some(wideTerminal))).unsafeRunSync()
    
    // Add content to buffer 0
    stateManager.applyEvent(InsertChar('1')).unsafeRunSync()
    val stateBuffer0 = stateManager.getCurrentState.unsafeRunSync()
    val buffer0Id = stateBuffer0.bufferOrder.head
    
    // Create buffer 1 and add content
    stateManager.applyEvent(NewTab).unsafeRunSync()
    stateManager.applyEvent(InsertChar('2')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('3')).unsafeRunSync()
    val stateBuffer1 = stateManager.getCurrentState.unsafeRunSync()
    val buffer1Id = stateBuffer1.bufferOrder.last
    
    // Create buffer 2 and add content
    stateManager.applyEvent(NewTab).unsafeRunSync()
    stateManager.applyEvent(InsertChar('4')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('5')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('6')).unsafeRunSync()
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer2Id = finalState.bufferOrder.last
    
    // Then: Each buffer should have the correct content and cursor position
    val buffer0 = finalState.buffers(buffer0Id)
    val buffer1 = finalState.buffers(buffer1Id)
    val buffer2 = finalState.buffers(buffer2Id)
    
    buffer0.content.collect() shouldBe "1"
    buffer0.cursors.head.column shouldBe 1
    
    buffer1.content.collect() shouldBe "23"
    buffer1.cursors.head.column shouldBe 2
    
    buffer2.content.collect() shouldBe "456"
    buffer2.cursors.head.column shouldBe 3
    
    // When: Navigate back through buffers and verify cursor positions remain
    stateManager.applyEvent(PreviousTab).unsafeRunSync() // Go to buffer 1
    val stateOnBuffer1 = stateManager.getCurrentState.unsafeRunSync()
    stateOnBuffer1.focusedBufferId.get shouldBe buffer1Id
    
    stateManager.applyEvent(PreviousTab).unsafeRunSync() // Go to buffer 0
    val stateOnBuffer0 = stateManager.getCurrentState.unsafeRunSync()
    stateOnBuffer0.focusedBufferId.get shouldBe buffer0Id
    
    // Cursor positions should still be preserved
    stateOnBuffer0.buffers(buffer0Id).cursors.head.column shouldBe 1
    stateOnBuffer1.buffers(buffer1Id).cursors.head.column shouldBe 2
    stateOnBuffer0.buffers(buffer2Id).cursors.head.column shouldBe 3
  }