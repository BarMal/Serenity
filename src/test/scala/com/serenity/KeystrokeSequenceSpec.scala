package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.traverse.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*

class KeystrokeSequenceSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Complex Keystroke Sequences"

  it should "handle typical programming workflow: typing function definition" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Empty buffer ready for input
      bufferId     <- stateManager.createBuffer("")
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      // When: Type a function definition with typical keystrokes
      functionSequence = List(
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
      _ <- functionSequence.traverse(event => stateManager.applyEvent(event))

      // Then: Should have proper function definition
      finalState <- stateManager.getCurrentState
      buffer = finalState.buffers(bufferId)
      expected = """def hello(): String =
  "Hello"""".replace("\r\n", "\n")

      // Cursor should be at end
      pane = finalState.layout.editorPanes(paneId)
      paneBuffer <- finalState.buffers
        .get(bufferId)
        .fold(IO.raiseError[Buffer](new RuntimeException("Buffer not found")))(IO.pure)
    yield
      buffer.content.collect() shouldBe expected
      paneBuffer.cursors.head.line shouldBe 1
      paneBuffer.cursors.head.column shouldBe 9 // After 2 spaces + "Hello"

    program.unsafeRunSync()
  }

  it should "handle text editing workflow: writing and correcting mistakes" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Buffer with some text
      bufferId     <- stateManager.createBuffer("The quik brown fox")
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      // Position cursor at "quik" to correct spelling
      _ <- stateManager.setCursorPosition(paneId, 0, 4) // At start of "quik"

      // When: Select and correct the misspelling
      correctionSequence = List(
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
      _ <- correctionSequence.traverse(event => stateManager.applyEvent(event))

      // Then: Text should be corrected
      finalState <- stateManager.getCurrentState
      buffer = finalState.buffers(bufferId)
    yield buffer.content.collect() shouldBe "The quick brown fox"

    program.unsafeRunSync()
  }

  it should "handle navigation and editing in multiline text" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Multiline buffer
      initialText = """Line 1
Line 2
Line 3
Line 4"""
      bufferId     <- stateManager.createBuffer(initialText)
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      // Position cursor at end of initial content
      lines      = initialText.split("\n", -1)
      lastLine   = lines.length - 1
      lastColumn = if lines.nonEmpty then lines.last.length else 0
      _ <- stateManager.setCursorPosition(paneId, lastLine, lastColumn)

      // When: Navigate to middle and make edits
      editSequence = List(
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
      _ <- editSequence.traverse(event => stateManager.applyEvent(event))

      // Then: Should have edited multiline content
      finalState <- stateManager.getCurrentState
      buffer = finalState.buffers(bufferId)
      expected = """Line 1
Line 2!
* Line 3
Final line""".replace("\r\n", "\n")
    yield buffer.content.collect() shouldBe expected

    program.unsafeRunSync()
  }

  it should "handle word-level operations and boundary navigation" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Text with multiple words
      bufferId     <- stateManager.createBuffer("Hello world this is a test")
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      // Position at start of "world"
      _ <- stateManager.setCursorPosition(paneId, 0, 6)

      // When: Perform word-level edits
      wordEditSequence = List(
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
      _ <- wordEditSequence.traverse(event => stateManager.applyEvent(event))

      // Then: Should have modified text
      finalState <- stateManager.getCurrentState
      buffer = finalState.buffers(bufferId)
    yield buffer.content.collect() shouldBe "Hello beautiful this is a test!"

    program.unsafeRunSync()
  }

  it should "handle rapid insertion and deletion sequences" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Empty buffer
      bufferId     <- stateManager.createBuffer("")
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      // When: Rapid typing with corrections
      rapidSequence = List(
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
      _ <- rapidSequence.traverse(event => stateManager.applyEvent(event))

      // Then: Should have final corrected text
      finalState <- stateManager.getCurrentState
      buffer = finalState.buffers(bufferId)
    yield buffer.content.collect() shouldBe "Typing quickly now"

    program.unsafeRunSync()
  }

  it should "handle line manipulation operations" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Single line of text
      bufferId     <- stateManager.createBuffer("Single line")
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      // Position at end
      _ <- stateManager.setCursorPosition(paneId, 0, 11)

      // When: Add multiple lines and manipulate them
      lineSequence = List(
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
      _ <- lineSequence.traverse(event => stateManager.applyEvent(event))

      // Then: Should have proper multiline structure
      finalState <- stateManager.getCurrentState
      buffer = finalState.buffers(bufferId)
      expected = """The Single line
Second line
Third""".replace("\r\n", "\n")
    yield buffer.content.collect() shouldBe expected

    program.unsafeRunSync()
  }

  it should "handle edge case navigation at document boundaries" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Small text
      bufferId     <- stateManager.createBuffer("AB\nCD")
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      // When: Test boundary navigation
      boundarySequence = List(
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
      _ <- boundarySequence.traverse(event => stateManager.applyEvent(event))

      // Then: Should handle boundaries gracefully
      finalState <- stateManager.getCurrentState
      buffer = finalState.buffers(bufferId)
      pane   = finalState.layout.editorPanes(paneId)
      paneBuffer <- finalState.buffers
        .get(bufferId)
        .fold(IO.raiseError[Buffer](new RuntimeException("Buffer not found")))(IO.pure)
    yield
      buffer.content.collect() shouldBe "AB\nCDE"
      // Cursor should be at end
      paneBuffer.cursors.head.line shouldBe 1
      paneBuffer.cursors.head.column shouldBe 3

    program.unsafeRunSync()
  }

  it should "handle backspace at line boundaries" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Multiline text
      bufferId     <- stateManager.createBuffer("First\nSecond\nThird")
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      // Position at start of second line
      _ <- stateManager.setCursorPosition(paneId, 1, 0)

      // When: Backspace to join lines
      backspaceSequence = List(
        DeleteBackward, // Should join lines
        InsertChar(' '),
        InsertChar('&'),
        InsertChar(' ')
      )
      _ <- backspaceSequence.traverse(event => stateManager.applyEvent(event))

      // Then: Lines should be joined
      finalState <- stateManager.getCurrentState
      buffer = finalState.buffers(bufferId)
    yield buffer.content.collect() shouldBe "First & Second\nThird"

    program.unsafeRunSync()
  }

  it should "handle state consistency during complex edit sequences" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Buffer with content
      bufferId     <- stateManager.createBuffer("Initial state")
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      // When: Complex sequence that tests state consistency
      complexSequence = List(
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
      _ <- complexSequence.traverse(event => stateManager.applyEvent(event))

      // Then: State should be consistent and valid
      finalState <- stateManager.getCurrentState
      _ = finalState.isValid shouldBe true

      buffer = finalState.buffers(bufferId)
      expected = """* Initial state
Line 2 (middle)
Line 3""".replace("\r\n", "\n")
    yield buffer.content.collect() shouldBe expected

    program.unsafeRunSync()
  }

  trait KeystrokeFixture:

    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))

    val stateManager: StateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

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
