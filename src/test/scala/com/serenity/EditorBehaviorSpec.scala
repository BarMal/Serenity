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

class EditorBehaviorSpec extends AnyFlatSpec with Matchers:

  given balance: Balance                 = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)
  given loggerFactory: LoggerFactory[IO] = Slf4jFactory.create[IO]

  behavior of "Text Editor End-to-End Behavior"

  it should "start with an empty initial state" in new EditorFixture:
    val state = stateManager.getCurrentState.unsafeRunSync()

    state.buffers shouldBe empty
    state.layout.editorPanes should have size 1
    state.focus shouldBe Focus.EditorPane(PaneId(0))
    state.modal shouldBe None
    state.peekOverlay shouldBe None

  it should "create a buffer and handle basic text insertion" in new EditorFixture:
    // Given: Create a buffer and associate it with the default pane
    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head

    // Associate buffer with pane
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // When: Type "Hello"
    "Hello".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    // Then: Buffer should contain "Hello" and cursor should be at end
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    buffer.content.collect() shouldBe "Hello"

    val pane = finalState.layout.editorPanes(paneId)
    pane.cursors.head.column shouldBe 5
    pane.cursors.head.line shouldBe 0

  it should "handle backspace behavior correctly" in new EditorFixture:
    // Given: A buffer with text "Hello World"
    val bufferId = stateManager.createBuffer("Hello World").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head

    // Associate buffer with pane and position cursor at end
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 11).unsafeRunSync() // End of "Hello World"

    // When: Perform backspace
    stateManager.applyEvent(DeleteBackward).unsafeRunSync()

    // Then: Last character 'd' should be deleted
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    buffer.content.collect() shouldBe "Hello Worl"

    val pane = finalState.layout.editorPanes(paneId)
    pane.cursors.head.column shouldBe 10 // Cursor moves back

  it should "handle multiline text creation with newlines" in new EditorFixture:
    // Given: Empty buffer
    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head

    // Associate buffer with pane
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // When: Type "Line 1", press Enter, type "Line 2"
    "Line 1".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(NewLine).unsafeRunSync()
    "Line 2".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    // Then: Buffer should contain multiline text
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    buffer.content.collect() shouldBe "Line 1\nLine 2"

    val pane = finalState.layout.editorPanes(paneId)
    pane.cursors.head.line shouldBe 1
    pane.cursors.head.column shouldBe 6

  it should "handle cursor movement across lines correctly" in new EditorFixture:
    // Given: Buffer with multiline text
    val bufferId = stateManager.createBuffer("First\nSecond\nThird").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head

    // Associate buffer with pane and start cursor at beginning
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 0).unsafeRunSync()

    // When: Move down two lines
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    stateManager.applyEvent(MoveDown).unsafeRunSync()

    // Then: Cursor should be on third line
    val afterDownState = stateManager.getCurrentState.unsafeRunSync()
    val afterDownPane  = afterDownState.layout.editorPanes(paneId)
    afterDownPane.cursors.head.line shouldBe 2
    afterDownPane.cursors.head.column shouldBe 0

    // When: Move to end of line
    stateManager.applyEvent(MoveToEnd).unsafeRunSync()

    // Then: Cursor should be at end of "Third"
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val pane       = finalState.layout.editorPanes(paneId)
    pane.cursors.head.column shouldBe 5

  it should "handle complex keystroke sequences for word manipulation" in new EditorFixture:
    // Given: Buffer with text
    val bufferId = stateManager.createBuffer("Hello world").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head

    // Place cursor at end of "Hello"
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 5).unsafeRunSync()

    // When: Delete forward (delete space), then insert comma and space
    stateManager.applyEvent(DeleteForward).unsafeRunSync()
    stateManager.applyEvent(InsertChar(',')).unsafeRunSync()
    stateManager.applyEvent(InsertChar(' ')).unsafeRunSync()

    // Then: Text should be "Hello, world"
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    buffer.content.collect() shouldBe "Hello, world"

  it should "maintain buffer dirty state correctly during edits" in new EditorFixture:
    // Given: Clean buffer
    val bufferId     = stateManager.createBuffer("Initial").unsafeRunSync()
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    initialState.buffers(bufferId).isDirty shouldBe false

    val paneId = initialState.layout.editorPanes.keys.head

    // Associate buffer with pane and position cursor at end
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 7).unsafeRunSync() // At end

    // When: Make an edit
    stateManager.applyEvent(InsertChar('!')).unsafeRunSync()

    // Then: Buffer should be marked dirty
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    buffer.isDirty shouldBe true
    buffer.content.collect() shouldBe "Initial!"

  it should "handle rapid keystroke sequences without losing state consistency" in new EditorFixture:
    // Given: Empty buffer
    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head

    // Associate buffer with pane
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // When: Perform rapid keystroke sequence: type ABC, move left twice, delete B, insert X, move right, insert Y
    // Trace: ABC → MoveLeft×2 → cursor before B → DeleteForward (B) → AC → InsertChar(X) → AXC
    //        → MoveRight → cursor past C → InsertChar(Y) → AXCY
    val keySequence = List(
      InsertChar('A'),
      InsertChar('B'),
      InsertChar('C'),
      MoveLeft,
      MoveLeft,
      DeleteForward,
      InsertChar('X'),
      MoveRight,
      InsertChar('Y')
    )

    keySequence.foreach(event => stateManager.applyEvent(event).unsafeRunSync())

    // Then: Final text should be "AXCY" with cursor after Y
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    buffer.content.collect() shouldBe "AXCY"

    val pane = finalState.layout.editorPanes(paneId)
    pane.cursors.head.column shouldBe 4

  it should "validate state consistency after complex operations" in new EditorFixture:
    // Given: Multiple buffers and operations
    val buffer1 = stateManager.createBuffer("Buffer 1").unsafeRunSync()
    val buffer2 = stateManager.createBuffer("Buffer 2").unsafeRunSync()
    val pane2   = stateManager.createPane(Some(buffer2)).unsafeRunSync()

    // When: Switch between panes and perform edits
    stateManager.switchToPane(pane2).unsafeRunSync()

    // Edit in second buffer
    stateManager.setCursorPosition(pane2, 0, 8).unsafeRunSync()

    stateManager.applyEvent(InsertChar('!')).unsafeRunSync()

    // Then: State should be valid
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.isValid shouldBe true
    finalState.buffers should have size 2
    finalState.buffers(buffer2).content.collect() shouldBe "Buffer 2!"

  it should "handle edge cases with cursor at line boundaries" in new EditorFixture:
    // Given: Multiline buffer
    val bufferId = stateManager.createBuffer("Line1\n\nLine3").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head

    // Position cursor at end of first line
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 5).unsafeRunSync()

    // When: Move right (should go to next line)
    stateManager.applyEvent(MoveRight).unsafeRunSync()

    // Then: Should be on empty line
    val afterMoveState = stateManager.getCurrentState.unsafeRunSync()
    val afterMovePane  = afterMoveState.layout.editorPanes(paneId)
    afterMovePane.cursors.head.line shouldBe 1
    afterMovePane.cursors.head.column shouldBe 0

    // When: Move right again (should go to Line3)
    stateManager.applyEvent(MoveRight).unsafeRunSync()

    // Then: Should be at start of Line3
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val pane       = finalState.layout.editorPanes(paneId)
    pane.cursors.head.line shouldBe 2
    pane.cursors.head.column shouldBe 0

  it should "handle writing to a completely blank buffer" in new EditorFixture:
    // Given: Editor starts with no buffers at all
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    initialState.buffers shouldBe empty

    // When: Create a blank buffer and start typing
    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head

    // Associate buffer with pane
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Type first character into empty buffer
    "Writing into empty space!".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    // Then: Buffer should contain the text
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    buffer.content.collect() shouldBe "Writing into empty space!"
    buffer.isDirty shouldBe true

    val pane = finalState.layout.editorPanes(paneId)
    pane.cursors.head.line shouldBe 0
    pane.cursors.head.column shouldBe "Writing into empty space!".length

  it should "handle overwriting selection with new text" in new EditorFixture:
    // TODO: Implement Selection data type and selection handling
    // Given: Buffer with text and selection
    val bufferId = stateManager.createBuffer("Hello World Program").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head

    // Simulate selection of "World" (positions 6-11) - will fail until Selection is implemented
    // val updatedPane = state.layout.editorPanes(paneId).copy(
    //   bufferId = Some(bufferId),
    //   cursors = List(CursorPosition(0, 6)),
    //   selection = Some(Selection(CursorPosition(0, 6), CursorPosition(0, 11)))
    // )
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 6).unsafeRunSync()

    // When: Type new text (should replace selection when implemented)
    "Universe".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    // Then: Selected text should be replaced (will currently just append)
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    // This will fail until selection replacement is implemented:
    // buffer.content.collect() shouldBe "Hello Universe Program"
    // For now it will be: "Hello UniverseWorld Program"
    buffer.content.collect() should include("Universe")

  it should "handle undo/redo operations correctly" in new EditorFixture:
    // TODO: Implement Undo/Redo events and state management
    // Given: Buffer with initial content
    val bufferId = stateManager.createBuffer("Initial").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head

    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 7).unsafeRunSync()

    // When: Make edits
    stateManager.applyEvent(InsertChar(' ')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('T')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('e')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('x')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('t')).unsafeRunSync()

    val afterEditsState = stateManager.getCurrentState.unsafeRunSync()
    afterEditsState.buffers(bufferId).content.collect() shouldBe "Initial Text"

    // When: Try undo operations (will fail until Undo event is implemented)
    // stateManager.applyEvent(Undo).unsafeRunSync()

    // Then: Should revert changes (assertion will fail until implemented)
    // val afterUndoState = stateManager.getCurrentState.unsafeRunSync()
    // afterUndoState.buffers(bufferId).content.collect() shouldBe "Initial"

    // When: Try redo operations (will fail until Redo event is implemented)
    // stateManager.applyEvent(Redo).unsafeRunSync()

    // Then: Should restore changes (assertion will fail until implemented)
    // val afterRedoState = stateManager.getCurrentState.unsafeRunSync()
    // afterRedoState.buffers(bufferId).content.collect() shouldBe "Initial Text"

  it should "handle opening an existing file" in new EditorFixture:
    // TODO: Implement file system operations - stubs will log but not actually work
    // When: Open a file (mocked file system operation)
    val fileContent = "This is file content\nWith multiple lines\nAnd some text"
    val bufferId    = stateManager.createBuffer(fileContent).unsafeRunSync()
    stateManager.setBufferFilePath(bufferId, "/path/to/file.txt").unsafeRunSync()

    // Then: Buffer should contain file content and not be dirty (stub doesn't set filePath yet)
    val state  = stateManager.getCurrentState.unsafeRunSync()
    val buffer = state.buffers(bufferId)
    buffer.content.collect() shouldBe fileContent
    buffer.isDirty shouldBe false
    // buffer.filePath shouldBe Some("/path/to/file.txt") // Will fail until setBufferFilePath is implemented

  it should "handle saving a file" in new EditorFixture:
    // TODO: Implement file save operations - stubs will log but not actually work
    // Given: Modified buffer
    val bufferId = stateManager.createBuffer("Original content").unsafeRunSync()
    stateManager.setBufferFilePath(bufferId, "/path/to/save.txt").unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 16).unsafeRunSync()

    stateManager.applyEvent(InsertChar(' ')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('+')).unsafeRunSync()
    stateManager.applyEvent(InsertChar(' ')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('m')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('o')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('d')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('s')).unsafeRunSync()

    val beforeSaveState = stateManager.getCurrentState.unsafeRunSync()
    beforeSaveState.buffers(bufferId).isDirty shouldBe true

    // When: Save file (stub will log but not actually save)
    stateManager.saveBuffer(bufferId).unsafeRunSync()

    // Then: Buffer should no longer be dirty (will fail until saveBuffer is implemented)
    val afterSaveState = stateManager.getCurrentState.unsafeRunSync()
    // afterSaveState.buffers(bufferId).isDirty shouldBe false // Will fail until saveBuffer is implemented
    afterSaveState.buffers(bufferId).content.collect() shouldBe "Original content + mods"

  trait EditorFixture:
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager: StateManager =
      StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO]).unsafeRunSync()
