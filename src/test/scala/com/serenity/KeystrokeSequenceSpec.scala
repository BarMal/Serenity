package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class KeystrokeSequenceSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Complex Keystroke Sequences"

  it should "handle typical programming workflow: typing function definition" in new KeystrokeFixture:
    // Given: Empty buffer ready for input
    val bufferId = setupBuffer("")

    // When: Type a function definition with typical keystrokes
    val functionSequence = List(
      // Type "def hello"
      InsertChar('d'),
      InsertChar('e'),
      InsertChar('f'),
      InsertChar(' '),
      InsertChar('h'),
      InsertChar('e'),
      InsertChar('l'),
      InsertChar('l'),
      InsertChar('o'),
      // Add parentheses
      InsertChar('('),
      InsertChar(')'),
      InsertChar(':'),
      InsertChar(' '),
      // Add return type
      InsertChar('S'),
      InsertChar('t'),
      InsertChar('r'),
      InsertChar('i'),
      InsertChar('n'),
      InsertChar('g'),
      InsertChar(' '),
      InsertChar('='),
      // New line and add body
      NewLine,
      InsertChar(' '),
      InsertChar(' '), // Indentation
      InsertChar('"'),
      InsertChar('H'),
      InsertChar('e'),
      InsertChar('l'),
      InsertChar('l'),
      InsertChar('o'),
      InsertChar('"')
    )

    executeKeystrokeSequence(functionSequence)

    // Then: Should have proper function definition
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    val expected   = """def hello(): String =
  "Hello""""
    buffer.content.collect() shouldBe expected

    // Cursor should be at end
    val pane = getCurrentPane(finalState)
    pane.cursors.head.line shouldBe 1
    pane.cursors.head.column shouldBe 9 // After 2 spaces + "Hello"

  it should "handle text editing workflow: writing and correcting mistakes" in new KeystrokeFixture:
    // Given: Buffer with some text
    val bufferId = setupBuffer("The quik brown fox")

    // Position cursor at "quik" to correct spelling
    positionCursor(0, 4) // At start of "quik"

    // When: Select and correct the misspelling
    val correctionSequence = List(
      // Delete "quik"
      DeleteForward,
      DeleteForward,
      DeleteForward,
      DeleteForward,
      // Type "quick"
      InsertChar('q'),
      InsertChar('u'),
      InsertChar('i'),
      InsertChar('c'),
      InsertChar('k')
    )

    executeKeystrokeSequence(correctionSequence)

    // Then: Text should be corrected
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    buffer.content.collect() shouldBe "The quick brown fox"

  it should "handle navigation and editing in multiline text" in new KeystrokeFixture:
    // Given: Multiline buffer
    val initialText = """Line 1
Line 2
Line 3
Line 4"""
    val bufferId    = setupBuffer(initialText)

    // When: Navigate to middle and make edits
    val editSequence = List(
      // Go to Line 2 from end
      MoveUp,
      MoveUp,
      MoveToEnd,
      // Add exclamation
      InsertChar('!'),
      // Go to Line 3, insert text at beginning
      MoveDown,
      MoveToStart,
      InsertChar('*'),
      InsertChar(' '),
      // Go to Line 4 and replace content
      MoveDown,
      MoveToStart,
      // Select and replace "Line 4"
      DeleteForward,
      DeleteForward,
      DeleteForward,
      DeleteForward,
      DeleteForward,
      DeleteForward,
      InsertChar('F'),
      InsertChar('i'),
      InsertChar('n'),
      InsertChar('a'),
      InsertChar('l'),
      InsertChar(' '),
      InsertChar('l'),
      InsertChar('i'),
      InsertChar('n'),
      InsertChar('e')
    )

    executeKeystrokeSequence(editSequence)

    // Then: Should have edited multiline content
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    val expected   = """Line 1
Line 2!
* Line 3
Final line"""
    buffer.content.collect() shouldBe expected

  it should "handle word-level operations and boundary navigation" in new KeystrokeFixture:
    // Given: Text with multiple words
    val bufferId = setupBuffer("Hello world this is a test")

    // Position at start of "world"
    positionCursor(0, 6)

    // When: Perform word-level edits
    val wordEditSequence = List(
      // Delete "world "
      DeleteForward,
      DeleteForward,
      DeleteForward,
      DeleteForward,
      DeleteForward,
      DeleteForward,
      // Insert "beautiful "
      InsertChar('b'),
      InsertChar('e'),
      InsertChar('a'),
      InsertChar('u'),
      InsertChar('t'),
      InsertChar('i'),
      InsertChar('f'),
      InsertChar('u'),
      InsertChar('l'),
      InsertChar(' '),
      // Navigate to end
      MoveToEnd,
      // Add punctuation
      InsertChar('!')
    )

    executeKeystrokeSequence(wordEditSequence)

    // Then: Should have modified text
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    buffer.content.collect() shouldBe "Hello beautiful this is a test!"

  it should "handle rapid insertion and deletion sequences" in new KeystrokeFixture:
    // Given: Empty buffer
    val bufferId = setupBuffer("")

    // When: Rapid typing with corrections
    val rapidSequence = List(
      // Type some text rapidly
      InsertChar('T'),
      InsertChar('y'),
      InsertChar('p'),
      InsertChar('i'),
      InsertChar('n'),
      InsertChar('g'),
      InsertChar(' '),
      InsertChar('f'),
      InsertChar('a'),
      InsertChar('s'),
      InsertChar('t'),
      // Make some corrections
      DeleteBackward,
      DeleteBackward,
      DeleteBackward,
      DeleteBackward, // Delete "fast"
      InsertChar('q'),
      InsertChar('u'),
      InsertChar('i'),
      InsertChar('c'),
      InsertChar('k'),
      InsertChar('l'),
      InsertChar('y'),
      // Add more text
      InsertChar(' '),
      InsertChar('n'),
      InsertChar('o'),
      InsertChar('w')
    )

    executeKeystrokeSequence(rapidSequence)

    // Then: Should have final corrected text
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    buffer.content.collect() shouldBe "Typing quickly now"

  it should "handle line manipulation operations" in new KeystrokeFixture:
    // Given: Single line of text
    val bufferId = setupBuffer("Single line")

    // Position at end
    positionCursor(0, 11)

    // When: Add multiple lines and manipulate them
    val lineSequence = List(
      // Add new lines
      NewLine,
      InsertChar('S'),
      InsertChar('e'),
      InsertChar('c'),
      InsertChar('o'),
      InsertChar('n'),
      InsertChar('d'),
      NewLine,
      InsertChar('T'),
      InsertChar('h'),
      InsertChar('i'),
      InsertChar('r'),
      InsertChar('d'),
      // Go back to second line
      MoveUp,
      MoveToEnd,
      // Modify second line
      InsertChar(' '),
      InsertChar('l'),
      InsertChar('i'),
      InsertChar('n'),
      InsertChar('e'),
      // Go to first line and modify
      MoveUp,
      MoveToStart,
      InsertChar('T'),
      InsertChar('h'),
      InsertChar('e'),
      InsertChar(' ')
    )

    executeKeystrokeSequence(lineSequence)

    // Then: Should have proper multiline structure
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    val expected   = """The Single line
Second line
Third"""
    buffer.content.collect() shouldBe expected

  it should "handle edge case navigation at document boundaries" in new KeystrokeFixture:
    // Given: Small text
    val bufferId = setupBuffer("AB\nCD")

    // When: Test boundary navigation
    val boundarySequence = List(
      // Try to move up from first line (should stay)
      MoveUp,
      MoveUp,
      // Move to absolute start
      MoveToStart,
      // Try to move left from start (should stay)
      MoveLeft,
      MoveLeft,
      // Move to end of document
      MoveDown,
      MoveToEnd,
      // Try to move right from end (should stay)
      MoveRight,
      MoveRight,
      // Try to move down from last line (should stay)
      MoveDown,
      MoveDown,
      // Insert at end
      InsertChar('E')
    )

    executeKeystrokeSequence(boundarySequence)

    // Then: Should handle boundaries gracefully
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    buffer.content.collect() shouldBe "AB\nCDE"

    // Cursor should be at end
    val pane = getCurrentPane(finalState)
    pane.cursors.head.line shouldBe 1
    pane.cursors.head.column shouldBe 3

  it should "handle backspace at line boundaries" in new KeystrokeFixture:
    // Given: Multiline text
    val bufferId = setupBuffer("First\nSecond\nThird")

    // Position at start of second line
    positionCursor(1, 0)

    // When: Backspace to join lines
    val backspaceSequence = List(
      DeleteBackward, // Should join lines
      InsertChar(' '),
      InsertChar('&'),
      InsertChar(' ')
    )

    executeKeystrokeSequence(backspaceSequence)

    // Then: Lines should be joined
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    buffer.content.collect() shouldBe "First & Second\nThird"

  it should "handle state consistency during complex edit sequences" in new KeystrokeFixture:
    // Given: Buffer with content
    val bufferId = setupBuffer("Initial state")

    // When: Complex sequence that tests state consistency
    val complexSequence = List(
      MoveToEnd,
      NewLine,
      InsertChar('L'),
      InsertChar('i'),
      InsertChar('n'),
      InsertChar('e'),
      InsertChar(' '),
      InsertChar('2'),
      NewLine,
      InsertChar('L'),
      InsertChar('i'),
      InsertChar('n'),
      InsertChar('e'),
      InsertChar(' '),
      InsertChar('3'),
      // Navigate back and edit
      MoveUp,
      MoveUp,
      MoveToStart,
      InsertChar('*'),
      InsertChar(' '),
      // Navigate and edit middle line
      MoveDown,
      MoveToEnd,
      InsertChar(' '),
      InsertChar('('),
      InsertChar('m'),
      InsertChar('i'),
      InsertChar('d'),
      InsertChar('d'),
      InsertChar('l'),
      InsertChar('e'),
      InsertChar(')')
    )

    executeKeystrokeSequence(complexSequence)

    // Then: State should be consistent and valid
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.isValid shouldBe true

    val buffer   = finalState.buffers(bufferId)
    val expected = """* Initial state
Line 2 (middle)
Line 3"""
    buffer.content.collect() shouldBe expected

  trait KeystrokeFixture:
    val stateManager: StateManager = StateManager.apply.unsafeRunSync()

    def setupBuffer(content: String): BufferId =
      val bufferId = stateManager.createBuffer(content).unsafeRunSync()
      val state    = stateManager.getCurrentState.unsafeRunSync()
      val paneId   = state.layout.editorPanes.keys.head

      // Associate buffer with pane
      stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

      // Position cursor at end of initial content
      val lines      = content.split("\n", -1) // -1 to preserve empty trailing strings
      val lastLine   = lines.length - 1
      val lastColumn = if lines.nonEmpty then lines.last.length else 0
      positionCursor(lastLine, lastColumn)

      bufferId

    def positionCursor(line: Int, column: Int): Unit =
      val state  = stateManager.getCurrentState.unsafeRunSync()
      val paneId = state.layout.editorPanes.keys.head
      stateManager.setCursorPosition(paneId, line, column).unsafeRunSync()

    def executeKeystrokeSequence(events: List[Event]): Unit =
      events.foreach(event => stateManager.applyEvent(event).unsafeRunSync())

    def getCurrentPane(state: AppState): EditorPane =
      val paneId = state.layout.editorPanes.keys.head
      state.layout.editorPanes(paneId)
