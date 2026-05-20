package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Test viewport scrolling functionality to prevent text overflow beyond panel boundaries
  */
class ViewportScrollingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Viewport Horizontal Scrolling"

  it should "scroll right when cursor moves beyond visible columns" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Get initial viewport settings
    val initialState   = stateManager.getCurrentState.unsafeRunSync()
    val initialPane    = initialState.layout.editorPanes(paneId)
    val visibleColumns = initialPane.viewport.visibleColumns

    // Insert text longer than visible area
    val longText = "x" * (visibleColumns + 5)
    longText.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val finalPane  = finalState.layout.editorPanes(paneId)
    val cursor     = finalPane.cursors.head
    val viewport   = finalPane.viewport

    // Cursor should be at end of text
    cursor.column shouldBe longText.length

    // Viewport should have scrolled to keep cursor visible
    cursor.column should be >= viewport.leftColumn
    cursor.column should be < (viewport.leftColumn + viewport.visibleColumns)

    // Viewport left column should be > 0 since we scrolled
    viewport.leftColumn should be > 0
  }

  it should "scroll left when cursor moves back to left edge" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Create long text and move cursor to the end
    val longText = "x" * 100
    longText.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    // Now move cursor back to the beginning
    stateManager.applyEvent(MoveToStart).unsafeRunSync()

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val finalPane  = finalState.layout.editorPanes(paneId)
    val cursor     = finalPane.cursors.head
    val viewport   = finalPane.viewport

    // Cursor should be at column 0
    cursor.column shouldBe 0

    // Viewport should have scrolled back to show the beginning
    viewport.leftColumn shouldBe 0
  }

  behavior of "Viewport Vertical Scrolling"

  it should "scroll down when cursor moves beyond visible lines" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Get initial viewport settings
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    val initialPane  = initialState.layout.editorPanes(paneId)
    val visibleLines = initialPane.viewport.visibleLines

    // Create text with more lines than visible
    val numLines = visibleLines + 5
    for _ <- 0 until numLines do
      stateManager.applyEvent(InsertChar('x')).unsafeRunSync()
      stateManager.applyEvent(NewLine).unsafeRunSync()

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val finalPane  = finalState.layout.editorPanes(paneId)
    val cursor     = finalPane.cursors.head
    val viewport   = finalPane.viewport

    // Cursor should be beyond the original visible area
    cursor.line should be >= visibleLines

    // Viewport should have scrolled to keep cursor visible
    cursor.line should be >= viewport.topLine
    cursor.line should be < (viewport.topLine + viewport.visibleLines)

    // Viewport top line should be > 0 since we scrolled
    viewport.topLine should be > 0
  }

  behavior of "Buffer Content Integrity"

  it should "preserve all text content regardless of viewport position" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Create content that will require both horizontal and vertical scrolling
    val testContent = "The quick brown fox jumps over the lazy dog. " * 10 + "\n"
    val numLines    = 30

    for _ <- 0 until numLines do
      testContent.foreach { char =>
        if char == '\n' then stateManager.applyEvent(NewLine).unsafeRunSync()
        else stateManager.applyEvent(InsertChar(char)).unsafeRunSync()
      }

    val finalState      = stateManager.getCurrentState.unsafeRunSync()
    val buffer          = finalState.buffers(bufferId)
    val actualContent   = buffer.content.collect()
    val expectedContent = testContent * numLines

    // All content should be preserved exactly
    actualContent shouldBe expectedContent

    // Content should be significantly larger than any viewport
    actualContent.length should be > 1000
    actualContent.count(_ == '\n') shouldBe numLines
  }

  it should "handle rapid scrolling movements without losing content" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("Initial text").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Perform rapid movements that would trigger viewport changes
    val movements = List(
      MoveToEnd, // Move to end of initial text
      InsertChar('!'),
      InsertChar('!'),
      InsertChar('!'), // Add more text
      NewLine,         // New line
      InsertChar('N'),
      InsertChar('e'),
      InsertChar('w'), // Add "New line"
      MoveToStart,     // Back to beginning
      InsertChar('S'),
      InsertChar('t'),
      InsertChar('a'),
      InsertChar('r'),
      InsertChar('t'), // Add "Start"
      MoveToEnd        // Back to end
    )

    movements.foreach(event => stateManager.applyEvent(event).unsafeRunSync())

    val finalState   = stateManager.getCurrentState.unsafeRunSync()
    val buffer       = finalState.buffers(bufferId)
    val finalContent = buffer.content.collect()

    // Content should contain all our additions
    finalContent should include("Initial text")
    finalContent should include("Start")
    finalContent should include("New")
    finalContent should include("!!!")

    // Should have exactly one newline
    finalContent.count(_ == '\n') shouldBe 1
  }
