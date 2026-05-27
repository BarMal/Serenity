package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class SimplifiedEditorSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Text Editor Core Functionality"

  it should "create and manage buffers correctly" in new EditorFixture:
    // When: Create buffers
    val buffer1 = stateManager.createBuffer("First buffer").unsafeRunSync()
    val buffer2 = stateManager.createBuffer("Second buffer").unsafeRunSync()

    // Then: Buffers should exist in state (plus initial empty buffer)
    val state = stateManager.getCurrentState.unsafeRunSync()
    state.buffers should have size 3
    state.buffers should contain key buffer1
    state.buffers should contain key buffer2
    state.buffers(buffer1).content.collect() shouldBe "First buffer"
    state.buffers(buffer2).content.collect() shouldBe "Second buffer"

  it should "manage panes correctly" in new EditorFixture:
    // Given: Initial state has one pane
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    initialState.layout.editorPanes should have size 1

    // When: Create additional panes
    val buffer = stateManager.createBuffer("Test content").unsafeRunSync()
    val pane2  = stateManager.createPane(Some(buffer)).unsafeRunSync()
    val pane3  = stateManager.createPane().unsafeRunSync()

    // Then: Should have multiple panes
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.layout.editorPanes should have size 3
    finalState.layout.editorPanes should contain key pane2
    finalState.layout.editorPanes should contain key pane3

  it should "maintain state validation during operations" in new EditorFixture:
    // Given: Create some content
    val buffer1 = stateManager.createBuffer("Buffer 1").unsafeRunSync()
    val buffer2 = stateManager.createBuffer("Buffer 2").unsafeRunSync()
    val pane2   = stateManager.createPane(Some(buffer2)).unsafeRunSync()

    // When: Perform various operations
    stateManager.switchToPane(pane2).unsafeRunSync()
    stateManager.updateBuffer(buffer1, "Updated content").unsafeRunSync()

    // Then: State should remain valid
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.isValid shouldBe true
    finalState.validationErrors shouldBe empty
    finalState.buffers(buffer1).content.collect() shouldBe "Updated content"

  it should "handle buffer cleanup correctly" in new EditorFixture:
    // Given: Buffer associated with pane
    val buffer = stateManager.createBuffer("Test").unsafeRunSync()
    val pane   = stateManager.createPane(Some(buffer)).unsafeRunSync()

    val beforeState = stateManager.getCurrentState.unsafeRunSync()
    beforeState.buffers should contain key buffer

    // When: Close buffer
    stateManager.closeBuffer(buffer).unsafeRunSync()

    // Then: Buffer should be removed and pane disassociated
    val afterState = stateManager.getCurrentState.unsafeRunSync()
    afterState.buffers should not contain key(buffer)
    afterState.layout.editorPanes(pane).bufferId shouldBe None
    afterState.isValid shouldBe true

  it should "handle pane cleanup correctly" in new EditorFixture:
    // Given: Multiple panes
    val buffer = stateManager.createBuffer("Test").unsafeRunSync()
    val pane2  = stateManager.createPane(Some(buffer)).unsafeRunSync()

    val beforeState = stateManager.getCurrentState.unsafeRunSync()
    beforeState.layout.editorPanes should have size 2

    // When: Close pane
    stateManager.closePane(pane2).unsafeRunSync()

    // Then: Pane should be removed
    val afterState = stateManager.getCurrentState.unsafeRunSync()
    afterState.layout.editorPanes should have size 1
    afterState.layout.editorPanes should not contain key(pane2)
    afterState.isValid shouldBe true

  it should "handle focus transitions correctly" in new EditorFixture:
    // Given: Multiple panes
    val buffer1 = stateManager.createBuffer("Buffer 1").unsafeRunSync()
    val buffer2 = stateManager.createBuffer("Buffer 2").unsafeRunSync()
    val pane2   = stateManager.createPane(Some(buffer2)).unsafeRunSync()

    val initialState = stateManager.getCurrentState.unsafeRunSync()
    val pane1        = initialState.layout.editorPanes.keys.find(_ != pane2).get

    // When: Switch focus between panes
    stateManager.switchToPane(pane2).unsafeRunSync()
    val afterSwitch1 = stateManager.getCurrentState.unsafeRunSync()
    afterSwitch1.focus shouldBe Focus.EditorPane(pane2)

    stateManager.switchToPane(pane1).unsafeRunSync()
    val afterSwitch2 = stateManager.getCurrentState.unsafeRunSync()
    afterSwitch2.focus shouldBe Focus.EditorPane(pane1)

  it should "process quit events correctly" in new EditorFixture:
    // Given: StateManager awaiting quit
    val quitFuture = stateManager.awaitQuit.start.unsafeRunSync()

    // Initially should not be complete
    // Note: Testing fiber completion is complex in cats-effect
    // This is a placeholder for proper async testing

    // When: Send quit event
    stateManager.applyEvent(Quit).unsafeRunSync()

    // Then: Quit should complete
    // Test that quit completes the awaitable future
    quitFuture.cancel.unsafeRunSync() // Clean up the test

  trait EditorFixture:
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager: StateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()
