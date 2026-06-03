package com.serenity

import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.keystroke.events.NewTab
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

class CtrlReverseTabNavigationSpec extends AnyFlatSpec with Matchers:

  behavior of "Ctrl+ReverseTab Navigation Behavior"

  trait CtrlReverseTabFixture:
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager.apply(logger).unsafeRunSync()
    val wideTerminal = ViewportSize(400, 24) // Wide enough for multiple panes

  it should "handle PreviousTab event and navigate to previous buffer" in new CtrlReverseTabFixture {
    // Given: Wide terminal and multiple buffers
    stateManager.updateState(_.copy(viewportSize = Some(wideTerminal))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync() // Create second buffer
    val state = stateManager.getCurrentState.unsafeRunSync()
    
    state.buffers should have size 2
    val bufferIds = state.bufferOrder
    val firstBufferId = bufferIds(0)
    val secondBufferId = bufferIds(1)
    
    // Should start focused on second buffer
    state.focusedBufferId.get shouldBe secondBufferId
    
    // When: Send PreviousTab event (which Ctrl+ReverseTab should map to)
    stateManager.applyEvent(com.serenity.keystroke.events.PreviousTab).unsafeRunSync()
    val stateAfterEvent = stateManager.getCurrentState.unsafeRunSync()
    
    // Then: Should navigate to first buffer
    stateAfterEvent.focusedBufferId.get shouldBe firstBufferId
  }

  it should "cycle through multiple buffers with PreviousTab events" in new CtrlReverseTabFixture {
    // Given: Wide terminal and three buffers
    stateManager.updateState(_.copy(viewportSize = Some(wideTerminal))).unsafeRunSync()
    stateManager.applyEvent(NewTab).unsafeRunSync() // Buffer 1
    stateManager.applyEvent(NewTab).unsafeRunSync() // Buffer 2
    val state = stateManager.getCurrentState.unsafeRunSync()
    
    state.buffers should have size 3
    val bufferIds = state.bufferOrder
    
    // Should start focused on third buffer (last created)
    state.focusedBufferId.get shouldBe bufferIds(2)
    
    // When: Press PreviousTab multiple times
    stateManager.applyEvent(com.serenity.keystroke.events.PreviousTab).unsafeRunSync()
    val stateAfter1 = stateManager.getCurrentState.unsafeRunSync()
    stateAfter1.focusedBufferId.get shouldBe bufferIds(1) // Third -> Second
    
    stateManager.applyEvent(com.serenity.keystroke.events.PreviousTab).unsafeRunSync()
    val stateAfter2 = stateManager.getCurrentState.unsafeRunSync()
    stateAfter2.focusedBufferId.get shouldBe bufferIds(0) // Second -> First
    
    stateManager.applyEvent(com.serenity.keystroke.events.PreviousTab).unsafeRunSync()
    val stateAfter3 = stateManager.getCurrentState.unsafeRunSync()
    stateAfter3.focusedBufferId.get shouldBe bufferIds(2) // First -> Third (wrap around)
  }

  it should "verify translator maps Ctrl+ReverseTab to PreviousTab" in new CtrlReverseTabFixture {
    // Given: A TextEntryTranslator and Ctrl+ReverseTab keystroke
    import com.serenity.keystroke.translators.TextEntryTranslator
    import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
    val translator = new TextEntryTranslator()
    val ctrlReverseTab = KeyStrokeInfo(InputKey.ReverseTab, None, Set(Modifier.Ctrl))

    // When: Translate the keystroke
    val result = translator.translate(ctrlReverseTab)
    
    // Then: Should map to PreviousTab
    result shouldBe com.serenity.keystroke.events.PreviousTab
  }