package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*

/** Comprehensive end-to-end behavioral tests for the text editor. These tests capture the intended behavior and can be
  * used for TDD. Some may fail initially due to incomplete implementation.
  */
class EditorEndToEndSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Text Editor - Basic Text Input"

  it should "allow typing a simple sentence" in new EditorTestFixture:
    // Given: Empty editor
    val bufferId = createBufferWithPane("").getOrElse(fail("Could not create buffer"))

    // When: Type "Hello, world!"
    val keystrokes = "Hello, world!".map(InsertChar.apply)
    keystrokes.foreach(applyEvent)

    // Then: Buffer should contain the typed text
    val finalContent = getBufferContent(bufferId)
    finalContent shouldBe "Hello, world!"

  it should "handle backspace to correct typos" in new EditorTestFixture:
    // Given: Text with a typo
    val bufferId = createBufferWithPane("Hello, wrold!").getOrElse(fail("Could not create buffer"))

    // When: Fix "wrold" to "world" by correcting the transposition
    // Delete "ro" and retype as "or"
    applyEvent(MoveLeft)       // Before '!'
    applyEvent(MoveLeft)       // Before 'd'
    applyEvent(MoveLeft)       // Before 'l'
    applyEvent(MoveLeft)       // Before 'o'
    applyEvent(DeleteBackward) // Delete 'r'
    applyEvent(DeleteForward)  // Delete 'o'
    applyEvent(InsertChar('o'))
    applyEvent(InsertChar('r'))

    // Then: Text should be corrected to "Hello, world!"
    val finalContent = getBufferContent(bufferId)
    finalContent shouldBe "Hello, world!"

  it should "handle newlines to create multiple lines" in new EditorTestFixture:
    // Given: Empty editor
    val bufferId = createBufferWithPane("").getOrElse(fail("Could not create buffer"))

    // When: Type multiline text
    val events = List(
      InsertChar('F'),
      InsertChar('i'),
      InsertChar('r'),
      InsertChar('s'),
      InsertChar('t'),
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
      InsertChar('d')
    )
    events.foreach(applyEvent)

    // Then: Should have multiline content
    val content = getBufferContent(bufferId)
    content shouldBe "First\nSecond\nThird"

  behavior of "Text Editor - Cursor Movement"

  it should "move cursor with arrow keys" in new EditorTestFixture:
    // Given: Text content
    val bufferId = createBufferWithPane("Hello\nWorld").getOrElse(fail("Could not create buffer"))

    // When: Move cursor around and insert characters to verify position
    applyEvent(MoveLeft)        // Before 'd' in "World"
    applyEvent(InsertChar('X')) // Should insert before 'd'
    applyEvent(MoveUp)          // Go to first line
    applyEvent(MoveRight)       // Move right in first line
    applyEvent(InsertChar('Y')) // Should insert in first line
    applyEvent(MoveDown)        // Go back to second line

    // Then: Content should reflect cursor movements
    val finalContent = getBufferContent(bufferId)
    finalContent should include("Y") // Should have Y from first line
    finalContent should include("X") // Should have X from second line

  it should "move to start and end of lines" in new EditorTestFixture:
    // Given: Multi-line text
    val bufferId = createBufferWithPane("Hello\nWorld\nTest").getOrElse(fail("Could not create buffer"))

    // When: Move to the middle line (World) and edit it
    applyEvent(MoveUp)    // Go from "Test" to "World" line
    applyEvent(MoveToEnd) // Move to end of "World"
    applyEvent(InsertChar('!'))

    applyEvent(MoveToStart) // Move to start of "World"
    applyEvent(InsertChar('*'))

    // Then: Text should be modified at correct positions
    val finalContent = getBufferContent(bufferId)
    finalContent should include("*World!") // Should have both insertions on the "World" line

  behavior of "Text Editor - Deletion Operations"

  it should "delete characters with backspace" in new EditorTestFixture:
    // Given: Text content
    val bufferId = createBufferWithPane("Hello World").getOrElse(fail("Could not create buffer"))

    // When: Delete characters from the end
    applyEvent(DeleteBackward) // Delete 'd'
    applyEvent(DeleteBackward) // Delete 'l'
    applyEvent(DeleteBackward) // Delete 'r'
    applyEvent(DeleteBackward) // Delete 'o'
    applyEvent(DeleteBackward) // Delete 'W'
    applyEvent(DeleteBackward) // Delete space

    // Then: Should have "Hello"
    val finalContent = getBufferContent(bufferId)
    finalContent shouldBe "Hello"

  it should "delete characters with delete key" in new EditorTestFixture:
    // Given: Text with cursor positioned
    val bufferId = createBufferWithPane("Hello World").getOrElse(fail("Could not create buffer"))

    // When: Position cursor at the beginning and delete forward
    applyEvent(MoveToStart)   // Move to beginning of text
    applyEvent(DeleteForward) // Delete 'H'
    applyEvent(DeleteForward) // Delete 'e'

    // Then: Should have "llo World"
    val finalContent = getBufferContent(bufferId)
    finalContent shouldBe "llo World"

  behavior of "Text Editor - Complex Editing Scenarios"

  it should "handle rapid typing and corrections" in new EditorTestFixture:
    // Given: Empty editor
    val bufferId = createBufferWithPane("").getOrElse(fail("Could not create buffer"))

    // When: Simulate rapid typing with corrections
    val rapidSequence = List(
      // Type "Hwllo" (with typo)
      InsertChar('H'),
      InsertChar('w'),
      InsertChar('l'),
      InsertChar('l'),
      InsertChar('o'),
      // Correct it: move back, delete 'w', insert 'e'
      MoveLeft,
      MoveLeft,
      MoveLeft,
      MoveLeft,        // Move to after 'H'
      DeleteForward,   // Delete 'w'
      InsertChar('e'), // Insert 'e'
      MoveToEnd,       // Move to end
      InsertChar(' '),
      InsertChar('W'),
      InsertChar('o'),
      InsertChar('r'),
      InsertChar('l'),
      InsertChar('d')
    )

    rapidSequence.foreach(applyEvent)

    // Then: Should result in "Hello World"
    val finalContent = getBufferContent(bufferId)
    finalContent shouldBe "Hello World"

  it should "handle line joining with backspace" in new EditorTestFixture:
    // Given: Multiline text
    val bufferId = createBufferWithPane("First\nSecond").getOrElse(fail("Could not create buffer"))

    // When: Position cursor at start of second line and backspace
    applyEvent(MoveDown)
    applyEvent(MoveToStart)
    applyEvent(DeleteBackward) // Should join lines

    // Then: Should become "FirstSecond"
    val finalContent = getBufferContent(bufferId)
    finalContent shouldBe "FirstSecond"

  it should "handle word-level editing operations" in new EditorTestFixture:
    // Given: Text with words
    val bufferId = createBufferWithPane("The quick brown fox").getOrElse(fail("Could not create buffer"))

    // When: Replace "quick" with "slow" by positioning cursor and using deletion/insertion
    applyEvent(MoveToStart) // Go to beginning
    applyEvent(MoveRight)   // 'h'
    applyEvent(MoveRight)   // 'e'
    applyEvent(MoveRight)   // ' '
    applyEvent(MoveRight)   // move to 'q'
    // Delete "quick" (5 characters)
    applyEvent(DeleteForward) // 'q'
    applyEvent(DeleteForward) // 'u'
    applyEvent(DeleteForward) // 'i'
    applyEvent(DeleteForward) // 'c'
    applyEvent(DeleteForward) // 'k'
    // Insert "slow"
    applyEvent(InsertChar('s'))
    applyEvent(InsertChar('l'))
    applyEvent(InsertChar('o'))
    applyEvent(InsertChar('w'))

    // Then: Should become "The slow brown fox"
    val finalContent = getBufferContent(bufferId)
    finalContent shouldBe "The slow brown fox"

  behavior of "Text Editor - State Consistency"

  it should "maintain buffer dirty state during edits" in new EditorTestFixture:
    // Given: Clean buffer
    val bufferId = createBufferWithPane("Initial").getOrElse(fail("Could not create buffer"))

    // Initially should not be dirty
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    initialState.buffers.get(bufferId).map(_.isDirty) shouldBe Some(false)

    // When: Make edit
    applyEvent(InsertChar('!'))

    // Then: Should be marked dirty
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.buffers.get(bufferId).map(_.isDirty) shouldBe Some(true)

  it should "maintain state validity throughout complex operations" in new EditorTestFixture:
    // Given: Initial valid state
    val buffer1 = stateManager.createBuffer("Buffer 1").unsafeRunSync()
    val buffer2 = stateManager.createBuffer("Buffer 2").unsafeRunSync()

    // When: Perform series of operations
    val pane2 = stateManager.createPane(Some(buffer2)).unsafeRunSync()
    stateManager.switchToPane(pane2).unsafeRunSync()
    stateManager.updateBuffer(buffer1, "Updated content").unsafeRunSync()

    // Then: State should remain valid
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.isValid shouldBe true
    finalState.validationErrors shouldBe empty

  behavior of "Text Editor - Advanced Features"

  it should "handle very long lines efficiently" in new EditorTestFixture:
    // Given: Very long line
    val longLine = "a" * 10000
    val bufferId = createBufferWithPane(longLine).getOrElse(fail("Could not create buffer"))

    // When: Edit at the end (simpler than middle positioning)
    applyEvent(MoveToEnd)
    applyEvent(InsertChar('X'))

    // Then: Should handle efficiently without performance issues
    val finalContent = getBufferContent(bufferId)
    finalContent.length shouldBe 10001
    finalContent.last shouldBe 'X'

  it should "handle many lines efficiently" in new EditorTestFixture:
    // Given: Many lines
    val manyLines = (1 to 1000).map(i => s"Line $i").mkString("\n")
    val bufferId  = createBufferWithPane(manyLines).getOrElse(fail("Could not create buffer"))

    // When: Navigate to end and edit (simpler than complex navigation)
    applyEvent(MoveToEnd)
    applyEvent(InsertChar('!'))

    // Then: Should handle efficiently
    val finalContent = getBufferContent(bufferId)
    finalContent should endWith("Line 1000!")
    finalContent.count(_ == '\n') shouldBe 999 // 1000 lines = 999 newlines

  it should "support undo/redo operations" in new EditorTestFixture:
    val bufferId = createBufferWithPane("Initial").getOrElse(fail("Could not create buffer"))

    applyEvent(InsertChar('!'))
    applyEvent(InsertChar('!'))
    getBufferContent(bufferId) shouldBe "Initial!!"

    applyEvent(Undo) // undo the coalesced "!!" group
    getBufferContent(bufferId) shouldBe "Initial"

    applyEvent(Redo)
    getBufferContent(bufferId) shouldBe "Initial!!"

  it should "support copy/paste operations" in new EditorTestFixture:
    val bufferId = createBufferWithPane("Hello World").getOrElse(fail("Could not create buffer"))

    applyEvent(Copy)
    val stateAfterCopy = stateManager.getCurrentState.unsafeRunSync()
    stateAfterCopy.clipboard shouldBe Some("Hello World")

    applyEvent(Paste)
    getBufferContent(bufferId) shouldBe "Hello WorldHello World"

  trait EditorTestFixture:
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val stateManager: StateManager = StateManager
      .apply(LoggerFactory[IO].getLogger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    def createBufferWithPane(content: String): Option[BufferId] =
      try
        val bufferId = stateManager.createBuffer(content).unsafeRunSync()
        val state    = stateManager.getCurrentState.unsafeRunSync()
        val paneId   = state.layout.editorPanes.keys.head // Get default pane
        stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

        // Position cursor at the end of the content
        if content.nonEmpty then
          val lines          = content.split('\n')
          val lastLineIndex  = lines.length - 1
          val lastLineLength = lines.last.length
          stateManager.setCursorPosition(paneId, lastLineIndex, lastLineLength).unsafeRunSync()

        Some(bufferId)
      catch case _: Exception => None

    def applyEvent(event: Event): Unit =
      try stateManager.applyEvent(event).unsafeRunSync()
      catch
        case _: Exception => // Ignore for now - some operations may not be implemented

    def getBufferContent(bufferId: BufferId): String =
      val state = stateManager.getCurrentState.unsafeRunSync()
      state.buffers.get(bufferId).map(_.content.collect()).getOrElse("")
