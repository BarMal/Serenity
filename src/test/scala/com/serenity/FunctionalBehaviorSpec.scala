package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class FunctionalBehaviorSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Functional Programming Patterns and State Immutability"

  it should "maintain immutable state throughout event processing" in new FunctionalFixture:
    // Given: Initial state
    stateManager.createBuffer("Initial").unsafeRunSync()
    val initialState         = stateManager.getCurrentState.unsafeRunSync()
    val initialStateSnapshot = initialState.copy() // Snapshot for comparison

    // When: Apply events that should create new state instances
    stateManager.applyEvent(InsertChar('!')).unsafeRunSync()
    val afterInsertState = stateManager.getCurrentState.unsafeRunSync()

    stateManager.applyEvent(MoveLeft).unsafeRunSync()
    val afterMoveState = stateManager.getCurrentState.unsafeRunSync()

    // Then: Each state should be a different instance
    initialState should not be theSameInstanceAs(afterInsertState)
    afterInsertState should not be theSameInstanceAs(afterMoveState)

    // Original state should remain unchanged
    initialStateSnapshot.persisted.buffers shouldBe initialState.persisted.buffers
    initialStateSnapshot.persisted.layout shouldBe initialState.persisted.layout

  it should "demonstrate referential transparency in event processing" in new FunctionalFixture:
    // Given: Same initial state and events
    stateManager.createBuffer("Test").unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync()

    // Create second state manager with same initial state
//    given LoggerFactory[IO] = Slf4jFactory.create[IO]
//    val logger = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager2 = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()
    stateManager2.createBuffer("Test").unsafeRunSync()
    stateManager2.getCurrentState.unsafeRunSync()

    // When: Apply identical event sequences to both
    val eventSequence = List(
      InsertChar('!'),
      MoveToStart,
      InsertChar('*'),
      MoveToEnd
    )

    eventSequence.foreach(stateManager.applyEvent(_).unsafeRunSync())
    eventSequence.foreach(stateManager2.applyEvent(_).unsafeRunSync())

    // Then: Final states should be equivalent (referential transparency)
    val final1 = stateManager.getCurrentState.unsafeRunSync()
    val final2 = stateManager2.getCurrentState.unsafeRunSync()

    // Content should be identical
    final1.persisted.buffers.headOption.map(_._2.document.content.collect()) shouldBe
      final2.persisted.buffers.headOption.map(_._2.document.content.collect())

  it should "handle state transitions through monadic composition" in new FunctionalFixture:
    // Given: Initial state wrapped in IO
    val bufferId = stateManager.createBuffer("monad").unsafeRunSync()
    stateManager.createPane(Some(bufferId)).unsafeRunSync()

    // When: Chain operations using IO monad
    val monadicChain = for
      _          <- stateManager.applyEvent(MoveToEnd)
      _          <- stateManager.applyEvent(InsertChar('i'))
      _          <- stateManager.applyEvent(InsertChar('c'))
      _          <- stateManager.applyEvent(MoveToStart)
      _          <- stateManager.applyEvent(InsertChar('M'))
      finalState <- stateManager.getCurrentState
    yield finalState

    val result = monadicChain.unsafeRunSync()

    // Then: Should chain operations correctly
    result.persisted.buffers(bufferId).document.content.collect() shouldBe "Mmonadic"

  it should "demonstrate immutable data structure benefits" in new FunctionalFixture:
    // Given: Shared state between multiple "views"
    val bufferId  = stateManager.createBuffer("Shared content").unsafeRunSync()
    val baseState = stateManager.getCurrentState.unsafeRunSync()

    // When: Create multiple derived states (simulating undo/redo or multiple views)
    val state1 = baseState // Original
    val state2 =
      val buffer = state1.persisted.buffers(bufferId)
      val newContent = buffer.document.content
        .insert(buffer.document.content.weight, " - modified")
        .getOrElse(fail("expected insert to succeed"))
      val updatedBuffer = buffer.copy(document = buffer.document.copy(content = newContent))
      state1.copy(persisted = state1.persisted.copy(buffers = state1.persisted.buffers + (bufferId -> updatedBuffer)))
    val state3 =
      val buffer        = state2.persisted.buffers(bufferId)
      val newContent    = buffer.document.content.replaceAll("modified", "enhanced")
      val updatedBuffer = buffer.copy(document = buffer.document.copy(content = newContent))
      state2.copy(persisted = state2.persisted.copy(buffers = state2.persisted.buffers + (bufferId -> updatedBuffer)))

    // Then: All states should coexist without interference
    state1.persisted.buffers(bufferId).document.content.collect() shouldBe "Shared content"
    state2.persisted.buffers(bufferId).document.content.collect() shouldBe "Shared content - modified"
    state3.persisted.buffers(bufferId).document.content.collect() shouldBe "Shared content - enhanced"

    // Memory sharing through structural sharing (rope benefits)
    state1.persisted
      .buffers(bufferId)
      .document
      .content should not be theSameInstanceAs(state2.persisted.buffers(bufferId).document.content)
    state2.persisted
      .buffers(bufferId)
      .document
      .content should not be theSameInstanceAs(state3.persisted.buffers(bufferId).document.content)

  trait FunctionalFixture:

    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))

    val stateManager: StateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()
