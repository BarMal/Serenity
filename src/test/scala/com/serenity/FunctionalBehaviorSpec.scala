package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FunctionalBehaviorSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Functional Programming Patterns and State Immutability"

  it should "maintain immutable state throughout event processing" in new FunctionalFixture:
    // Given: Initial state
    val bufferId             = stateManager.createBuffer("Initial").unsafeRunSync()
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
    initialStateSnapshot.buffers shouldBe initialState.buffers
    initialStateSnapshot.layout shouldBe initialState.layout

  it should "demonstrate referential transparency in event processing" in new FunctionalFixture:
    // Given: Same initial state and events
    val bufferId1 = stateManager.createBuffer("Test").unsafeRunSync()
    val state1    = stateManager.getCurrentState.unsafeRunSync()

    // Create second state manager with same initial state
    val stateManager2 = StateManager.apply.unsafeRunSync()
    val bufferId2     = stateManager2.createBuffer("Test").unsafeRunSync()
    val state2        = stateManager2.getCurrentState.unsafeRunSync()

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
    final1.buffers.headOption.map(_._2.content.collect()) shouldBe
      final2.buffers.headOption.map(_._2.content.collect())

  it should "compose operations functionally without side effects" in new FunctionalFixture:
    // Given: Buffer with content
    val bufferId     = stateManager.createBuffer("compose").unsafeRunSync()
    val initialState = stateManager.getCurrentState.unsafeRunSync()

    // When: Compose multiple pure operations
    val operation1: AppState => AppState = state =>
      val buffer        = state.buffers(bufferId)
      val newContent    = buffer.content.insert(buffer.content.weight, " this")
      val updatedBuffer = buffer.copy(content = newContent)
      state.copy(buffers = state.buffers + (bufferId -> updatedBuffer))

    val operation2: AppState => AppState = state =>
      val buffer        = state.buffers(bufferId)
      val newContent    = buffer.content.insert(0, "Let's ")
      val updatedBuffer = buffer.copy(content = newContent)
      state.copy(buffers = state.buffers + (bufferId -> updatedBuffer))

    val operation3: AppState => AppState = state =>
      val buffer        = state.buffers(bufferId)
      val newContent    = buffer.content.insert(buffer.content.weight, " functionally!")
      val updatedBuffer = buffer.copy(content = newContent)
      state.copy(buffers = state.buffers + (bufferId -> updatedBuffer))

    // Compose operations
    val composedOperation = operation1 andThen operation2 andThen operation3
    val finalState        = composedOperation(initialState)

    // Then: Should produce expected result without side effects
    finalState.buffers(bufferId).content.collect() shouldBe "Let's compose this functionally!"
    // Original state unchanged
    initialState.buffers(bufferId).content.collect() shouldBe "compose"

  it should "handle state transitions through monadic composition" in new FunctionalFixture:
    // Given: Initial state wrapped in IO
    val bufferId = stateManager.createBuffer("monad").unsafeRunSync()
    val paneId   = stateManager.createPane(Some(bufferId)).unsafeRunSync()

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
    result.buffers(bufferId).content.collect() shouldBe "Mmonadic"

  it should "maintain state consistency through functional transformations" in new FunctionalFixture:
    // Given: Complex state with multiple components
    val buffer1 = stateManager.createBuffer("Buffer 1").unsafeRunSync()
    val buffer2 = stateManager.createBuffer("Buffer 2").unsafeRunSync()
    val pane2   = stateManager.createPane(Some(buffer2)).unsafeRunSync()

    // When: Apply functional state transformations
    val transformations = List(
      // Transform 1: Update buffer content
      (state: AppState) =>
        val updatedBuffer1 = state.buffers(buffer1).copy(isDirty = true)
        state.copy(buffers = state.buffers + (buffer1 -> updatedBuffer1))
      ,
      // Transform 2: Switch focus
      (state: AppState) => state.copy(focus = Focus.EditorPane(pane2)),
      // Transform 3: Update layout
      (state: AppState) =>
        val updatedLayout = state.layout.copy(activeEditorPaneId = Some(pane2))
        state.copy(layout = updatedLayout)
    )

    // Apply transformations functionally
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    val finalState   = transformations.foldLeft(initialState)((state, transform) => transform(state))

    // Then: All transformations should maintain consistency
    finalState.isValid shouldBe true
    finalState.focus shouldBe Focus.EditorPane(pane2)
    finalState.layout.activeEditorPaneId shouldBe Some(pane2)
    finalState.buffers(buffer1).isDirty shouldBe true

  it should "demonstrate pure functions for cursor operations" in new FunctionalFixture:
    // Given: Cursor utility functions (pure)
    def moveCursorRight(cursor: CursorPosition, lineLength: Int): CursorPosition =
      if cursor.column < lineLength then cursor.copy(column = cursor.column + 1)
      else cursor

    def moveCursorToNextLine(cursor: CursorPosition, maxLines: Int): CursorPosition =
      if cursor.line < maxLines - 1 then cursor.copy(line = cursor.line + 1, column = 0)
      else cursor

    def insertCharAtCursor(content: String, cursor: CursorPosition, char: Char): (String, CursorPosition) =
      val lines = content.split('\n')
      if cursor.line < lines.length then
        val line      = lines(cursor.line)
        val insertPos = Math.min(cursor.column, line.length)
        val newLine   = line.substring(0, insertPos) + char + line.substring(insertPos)
        val updatedLines = lines.zipWithIndex.map {
          case (l, idx) =>
            if idx == cursor.line then newLine else l
        }
        val newContent = updatedLines.mkString("\n")
        val newCursor  = cursor.copy(column = cursor.column + 1)
        (newContent, newCursor)
      else (content, cursor)

    // When: Test pure functions
    val initialCursor = CursorPosition(0, 5)
    val content       = "Hello world"

    val cursor1               = moveCursorRight(initialCursor, 11)
    val cursor2               = moveCursorToNextLine(cursor1, 2)
    val (newContent, cursor3) = insertCharAtCursor(content, initialCursor, '!')

    // Then: Functions should be pure (no side effects)
    cursor1.column shouldBe 6
    cursor2.line shouldBe 1
    cursor2.column shouldBe 0
    newContent shouldBe "Hello! world"
    cursor3.column shouldBe 6

    // Original cursor unchanged
    initialCursor.column shouldBe 5
    initialCursor.line shouldBe 0

  it should "handle error scenarios functionally with Either" in new FunctionalFixture:
    // Given: Functions that can fail
    def safeInsertAt(content: String, position: Int, text: String): Either[String, String] =
      if position < 0 || position > content.length then
        Left(s"Invalid position $position for content of length ${content.length}")
      else Right(content.substring(0, position) + text + content.substring(position))

    def safeMoveToPosition(
      cursor: CursorPosition,
      line: Int,
      column: Int,
      maxLines: Int,
      lineLength: Int
    ): Either[String, CursorPosition] =
      if line < 0 || line >= maxLines then Left(s"Invalid line $line (max: $maxLines)")
      else if column < 0 || column > lineLength then Left(s"Invalid column $column (max: $lineLength)")
      else Right(cursor.copy(line = line, column = column))

    // When: Test error handling
    val content = "Test content"
    val cursor  = CursorPosition(0, 0)

    val validInsert   = safeInsertAt(content, 4, " more")
    val invalidInsert = safeInsertAt(content, 20, " text")

    val validMove   = safeMoveToPosition(cursor, 0, 5, 1, 12)
    val invalidMove = safeMoveToPosition(cursor, 2, 0, 1, 12)

    // Then: Should handle success and failure functionally
    validInsert shouldBe Right("Test more content")
    invalidInsert shouldBe a[Left[?, ?]]

    validMove shouldBe Right(CursorPosition(0, 5))
    invalidMove shouldBe a[Left[?, ?]]

  it should "demonstrate immutable data structure benefits" in new FunctionalFixture:
    // Given: Shared state between multiple "views"
    val bufferId  = stateManager.createBuffer("Shared content").unsafeRunSync()
    val baseState = stateManager.getCurrentState.unsafeRunSync()

    // When: Create multiple derived states (simulating undo/redo or multiple views)
    val state1 = baseState // Original
    val state2 =
      val buffer        = state1.buffers(bufferId)
      val newContent    = buffer.content.insert(buffer.content.weight, " - modified")
      val updatedBuffer = buffer.copy(content = newContent)
      state1.copy(buffers = state1.buffers + (bufferId -> updatedBuffer))
    val state3 =
      val buffer        = state2.buffers(bufferId)
      val newContent    = buffer.content.replaceAll("modified", "enhanced")
      val updatedBuffer = buffer.copy(content = newContent)
      state2.copy(buffers = state2.buffers + (bufferId -> updatedBuffer))

    // Then: All states should coexist without interference
    state1.buffers(bufferId).content.collect() shouldBe "Shared content"
    state2.buffers(bufferId).content.collect() shouldBe "Shared content - modified"
    state3.buffers(bufferId).content.collect() shouldBe "Shared content - enhanced"

    // Memory sharing through structural sharing (rope benefits)
    state1.buffers(bufferId).content should not be theSameInstanceAs(state2.buffers(bufferId).content)
    state2.buffers(bufferId).content should not be theSameInstanceAs(state3.buffers(bufferId).content)

  trait FunctionalFixture:
    val stateManager: StateManager = StateManager.apply.unsafeRunSync()
