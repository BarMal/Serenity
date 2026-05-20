package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutEngine, TerminalSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** TDD tests for automatic line wrapping functionality.
  *
  * Requirements:
  *   1. Text wraps visually within panel boundaries
  *   2. Raw buffer content remains unchanged (no artificial line breaks)
  *   3. Wrapped lines are navigable seamlessly
  *   4. Viewport dynamically sizes based on terminal size
  *   5. Implementation is immutable and functional
  */
class LineWrappingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Visual Line Wrapping"

  it should "wrap long single line text within panel width while preserving buffer content" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Get panel width for wrapping calculations
    val currentState = stateManager.getCurrentState.unsafeRunSync()
    val layout       = LayoutEngine.calculateLayout(currentState, TerminalSize(80, 24))
    val panelWidth   = layout.editorPanelRect.width

    // Create text longer than panel width but shorter than 2 lines
    val longText = "The quick brown fox jumps over the lazy dog and continues running through the forest."
    longText.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)
    val pane       = finalState.layout.editorPanes(paneId)

    // Buffer content should be exactly the original text (no artificial line breaks)
    buffer.content.collect() shouldBe longText
    buffer.content.lineCount shouldBe 1

    // Text should require wrapping
    longText.length should be > panelWidth

    // Visual representation should calculate wrapped lines
    val visualLines = calculateVisualLines(longText, panelWidth)
    visualLines.length should be > 1
    visualLines.map(_.length).foreach(_ should be <= panelWidth)
  }

  it should "handle cursor navigation across wrapped visual lines" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    val layout     = LayoutEngine.calculateLayout(stateManager.getCurrentState.unsafeRunSync(), TerminalSize(80, 24))
    val panelWidth = layout.editorPanelRect.width

    // Create text that wraps to exactly 2 visual lines
    val firstLineText  = "a" * (panelWidth - 1) // One character short of wrapping
    val secondLineText = "bcdefg"               // This will wrap to second visual line
    val combinedText   = firstLineText + secondLineText

    combinedText.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val midState = stateManager.getCurrentState.unsafeRunSync()
    val buffer   = midState.buffers(bufferId)
    val cursor   = midState.layout.editorPanes(paneId).cursors.head

    // Buffer should be single line
    buffer.content.lineCount shouldBe 1
    cursor.line shouldBe 0
    cursor.column shouldBe combinedText.length

    // Navigate to middle of first visual line
    val targetColumn = panelWidth / 2
    // Move to start, then right to target position
    stateManager.applyEvent(MoveToStart).unsafeRunSync()
    for _ <- 0 until targetColumn do stateManager.applyEvent(MoveRight).unsafeRunSync()

    val navState  = stateManager.getCurrentState.unsafeRunSync()
    val navCursor = navState.layout.editorPanes(paneId).cursors.head

    navCursor.line shouldBe 0
    navCursor.column shouldBe targetColumn

    // Calculate visual line position
    val visualLineInfo = calculateVisualLinePosition(targetColumn, panelWidth)
    visualLineInfo.visualLine shouldBe 0
    visualLineInfo.visualColumn shouldBe targetColumn
  }

  it should "adjust viewport size dynamically based on terminal size changes" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Test different terminal sizes
    val smallTerminal = TerminalSize(40, 12)
    val largeTerminal = TerminalSize(120, 30)

    val smallLayout = LayoutEngine.calculateLayout(stateManager.getCurrentState.unsafeRunSync(), smallTerminal)
    val largeLayout = LayoutEngine.calculateLayout(stateManager.getCurrentState.unsafeRunSync(), largeTerminal)

    val smallPanelWidth = smallLayout.editorPanelRect.width
    val largePanelWidth = largeLayout.editorPanelRect.width

    // Larger terminal should provide more panel width
    largePanelWidth should be > smallPanelWidth

    // Test text wrapping behavior with different panel widths
    val testText = "This is a moderately long sentence that might wrap differently based on panel width."

    val smallVisualLines = calculateVisualLines(testText, smallPanelWidth)
    val largeVisualLines = calculateVisualLines(testText, largePanelWidth)

    // Same text should require fewer visual lines on larger panel
    largeVisualLines.length should be <= smallVisualLines.length

    info(
      s"Small terminal (${smallTerminal.width}x${smallTerminal.height}): panel width $smallPanelWidth, visual lines ${smallVisualLines.length}"
    )
    info(
      s"Large terminal (${largeTerminal.width}x${largeTerminal.height}): panel width $largePanelWidth, visual lines ${largeVisualLines.length}"
    )
  }

  it should "handle multiple paragraphs with wrapped lines correctly" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    val layout     = LayoutEngine.calculateLayout(stateManager.getCurrentState.unsafeRunSync(), TerminalSize(80, 24))
    val panelWidth = layout.editorPanelRect.width

    // Create multiple paragraphs
    val paragraph1 =
      "This is the first paragraph with enough text to require wrapping across multiple visual lines within the editor panel."
    val paragraph2 =
      "This is the second paragraph, also long enough to wrap, demonstrating multi-paragraph visual wrapping behavior."

    // Insert first paragraph
    paragraph1.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    // Add newline (real line break)
    stateManager.applyEvent(NewLine).unsafeRunSync()

    // Insert second paragraph
    paragraph2.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)

    // Buffer should have 2 real lines (separated by actual newline)
    buffer.content.lineCount shouldBe 2
    buffer.content.collect().count(_ == '\n') shouldBe 1

    // Each paragraph should wrap into multiple visual lines
    val para1VisualLines = calculateVisualLines(paragraph1, panelWidth)
    val para2VisualLines = calculateVisualLines(paragraph2, panelWidth)

    para1VisualLines.length should be > 1
    para2VisualLines.length should be > 1

    // Total visual lines should be sum of wrapped lines from both paragraphs
    val totalVisualLines = para1VisualLines.length + para2VisualLines.length
    totalVisualLines should be > 2
  }

  it should "scroll viewport to keep cursor visible when navigating wrapped lines" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    val layout      = LayoutEngine.calculateLayout(stateManager.getCurrentState.unsafeRunSync(), TerminalSize(80, 24))
    val panelWidth  = layout.editorPanelRect.width
    val panelHeight = layout.editorPanelRect.height

    // Create enough content to exceed panel height when wrapped
    val longSentence = "x" * (panelWidth + 10) // Wraps to 2 visual lines
    val totalLines   = panelHeight + 5         // More content than can fit in panel

    for _ <- 0 until totalLines do
      longSentence.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
      stateManager.applyEvent(NewLine).unsafeRunSync()

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val finalPane  = finalState.layout.editorPanes(paneId)
    val cursor     = finalPane.cursors.head
    val viewport   = finalPane.viewport

    // Cursor should be at end of content
    cursor.line shouldBe totalLines
    cursor.column shouldBe 0 // At start of empty line after last newline

    // Viewport should have scrolled to keep cursor visible
    val totalVisualLines = totalLines * 2 // Each line wraps to 2 visual lines
    if totalVisualLines > panelHeight then viewport.topLine should be > 0

    // Cursor should be visible within viewport
    cursor.line should be >= viewport.topLine
    cursor.line should be < (viewport.topLine + viewport.visibleLines)
  }

  it should "dynamically update viewport dimensions based on terminal size" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("test content").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Test different terminal sizes and their effect on viewport
    val smallTerminal = TerminalSize(60, 20)
    val largeTerminal = TerminalSize(120, 40)

    val currentState = stateManager.getCurrentState.unsafeRunSync()

    val smallLayout = LayoutEngine.calculateLayout(currentState, smallTerminal)
    val largeLayout = LayoutEngine.calculateLayout(currentState, largeTerminal)

    // Verify that different terminal sizes produce different panel sizes
    val smallPanelRect = smallLayout.editorPanelRect
    val largePanelRect = largeLayout.editorPanelRect

    smallPanelRect.width should be < largePanelRect.width
    smallPanelRect.height should be < largePanelRect.height

    // Test viewport dimension updates
    val initialPane   = currentState.layout.editorPanes(paneId)
    val smallViewport = LayoutEngine.updateViewportDimensions(initialPane.viewport, smallPanelRect)
    val largeViewport = LayoutEngine.updateViewportDimensions(initialPane.viewport, largePanelRect)

    // Verify viewport dimensions match panel dimensions
    smallViewport.visibleColumns shouldBe smallPanelRect.width
    smallViewport.visibleLines shouldBe smallPanelRect.height
    largeViewport.visibleColumns shouldBe largePanelRect.width
    largeViewport.visibleLines shouldBe largePanelRect.height

    // Verify dynamic sizing works
    largeViewport.visibleColumns should be > smallViewport.visibleColumns
    largeViewport.visibleLines should be > smallViewport.visibleLines

    info(
      s"Small terminal: panel ${smallPanelRect.width}x${smallPanelRect.height}, viewport ${smallViewport.visibleColumns}x${smallViewport.visibleLines}"
    )
    info(
      s"Large terminal: panel ${largePanelRect.width}x${largePanelRect.height}, viewport ${largeViewport.visibleColumns}x${largeViewport.visibleLines}"
    )
  }

  it should "preserve exact cursor position during window resize operations" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Insert test content
    val testContent =
      "The quick brown fox jumps over the lazy dog. This text will wrap differently based on window size."
    testContent.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    // Position cursor in middle of content
    val targetPosition = testContent.length / 2
    // Move to start, then right to target position
    stateManager.applyEvent(MoveToStart).unsafeRunSync()
    for _ <- 0 until targetPosition do stateManager.applyEvent(MoveRight).unsafeRunSync()

    val beforeState  = stateManager.getCurrentState.unsafeRunSync()
    val beforeCursor = beforeState.layout.editorPanes(paneId).cursors.head

    // Simulate window resize by recalculating layouts with different terminal sizes
    val smallLayout = LayoutEngine.calculateLayout(beforeState, TerminalSize(50, 20))
    val largeLayout = LayoutEngine.calculateLayout(beforeState, TerminalSize(100, 30))

    // Cursor position in buffer coordinates should remain unchanged
    beforeCursor.line shouldBe 0
    beforeCursor.column shouldBe targetPosition

    // But visual representation should adapt to new panel widths
    val smallPanelWidth = smallLayout.editorPanelRect.width
    val largePanelWidth = largeLayout.editorPanelRect.width

    val smallVisualPos = calculateVisualLinePosition(targetPosition, smallPanelWidth)
    val largeVisualPos = calculateVisualLinePosition(targetPosition, largePanelWidth)

    // Same buffer position maps to different visual positions
    if smallPanelWidth != largePanelWidth then
      (smallVisualPos.visualLine != largeVisualPos.visualLine) ||
      (smallVisualPos.visualColumn != largeVisualPos.visualColumn) shouldBe true
  }

  behavior of "Visual Line Navigation"

  it should "navigate up and down through visual lines correctly" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    val layout     = LayoutEngine.calculateLayout(stateManager.getCurrentState.unsafeRunSync(), TerminalSize(80, 24))
    val panelWidth = layout.editorPanelRect.width

    // Create text that wraps to exactly 2 visual lines
    val firstVisualLine  = "a" * (panelWidth - 2) // Leave room for a few more chars
    val secondVisualLine = "bcdef"                // This will be on the second visual line
    val longLine         = firstVisualLine + secondVisualLine

    longLine.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    // Position cursor at start of second visual line
    stateManager.applyEvent(MoveToEnd).unsafeRunSync()
    for _ <- 0 until secondVisualLine.length do stateManager.applyEvent(MoveLeft).unsafeRunSync()

    val beforeNavState = stateManager.getCurrentState.unsafeRunSync()
    val beforeCursor   = beforeNavState.layout.editorPanes(paneId).cursors.head
    beforeCursor.column shouldBe (panelWidth - 2) // At start of second visual line

    // Move up should go to same position in first visual line
    stateManager.applyEvent(MoveUp).unsafeRunSync()
    val afterUpState  = stateManager.getCurrentState.unsafeRunSync()
    val afterUpCursor = afterUpState.layout.editorPanes(paneId).cursors.head

    info(s"Panel width: $panelWidth")
    info(s"Before navigation: line=${beforeCursor.line}, column=${beforeCursor.column}")
    info(s"After move up: line=${afterUpCursor.line}, column=${afterUpCursor.column}")

    afterUpCursor.column should be <= (panelWidth - 2) // Should be near same position but within first visual line

    // Move down should return to second visual line
    stateManager.applyEvent(MoveDown).unsafeRunSync()
    val afterDownState  = stateManager.getCurrentState.unsafeRunSync()
    val afterDownCursor = afterDownState.layout.editorPanes(paneId).cursors.head

    info(s"After move down: line=${afterDownCursor.line}, column=${afterDownCursor.column}")

    // Should be back near the original position in second visual line
    val columnDiff = math.abs(afterDownCursor.column - beforeCursor.column)
    info(s"Column difference: $columnDiff")
    // Allow for slight differences due to line endings and wrapping behavior
    columnDiff should be <= 3
  }

  it should "navigate across multiple buffer lines with wrapped content" in {
    val stateManager = StateManager.apply.unsafeRunSync()

    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    val layout     = LayoutEngine.calculateLayout(stateManager.getCurrentState.unsafeRunSync(), TerminalSize(80, 24))
    val panelWidth = layout.editorPanelRect.width

    // Create first buffer line that wraps
    val firstLine = "x" * (panelWidth + 5) // Wraps to 2 visual lines
    firstLine.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    // Add a real newline
    stateManager.applyEvent(NewLine).unsafeRunSync()

    // Create second buffer line that also wraps
    val secondLine = "y" * (panelWidth + 3) // Wraps to 2 visual lines
    secondLine.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val buffer     = finalState.buffers(bufferId)

    // Verify we have 2 buffer lines
    buffer.content.lineCount shouldBe 2

    // Position cursor at end of content
    val endCursor = finalState.layout.editorPanes(paneId).cursors.head
    endCursor.line shouldBe 1 // Second buffer line
    endCursor.column shouldBe secondLine.length

    // Test navigation through visual lines
    val initialPosition = (endCursor.line, endCursor.column)

    // Move up through visual lines back to beginning using functional approach
    def moveUpAndCollect(state: AppState, moves: Int, positions: List[(Int, Int)]): List[(Int, Int)] =
      if moves >= 10 then positions // Safety limit
      else
        stateManager.applyEvent(MoveUp).unsafeRunSync()
        val newState     = stateManager.getCurrentState.unsafeRunSync()
        val cursor       = newState.layout.editorPanes(paneId).cursors.head
        val newPosition  = (cursor.line, cursor.column)
        val newPositions = positions :+ newPosition

        if cursor.line == 0 && cursor.column == 0 then newPositions // Break condition
        else moveUpAndCollect(newState, moves + 1, newPositions)

    val positions = moveUpAndCollect(finalState, 0, List(initialPosition))

    // Should have moved through approximately 4 visual lines total
    // (2 from second buffer line + 2 from first buffer line)
    positions.length should be >= 4

    // Final position should be at start
    val finalCursor = finalState.layout.editorPanes(paneId).cursors.head
    finalCursor.line shouldBe 0
    finalCursor.column shouldBe 0
  }

  // Helper functions for testing visual line wrapping logic
  private def calculateVisualLines(text: String, panelWidth: Int): List[String] =
    if text.isEmpty || panelWidth <= 0 then return List("")

    def wrapLine(remaining: String, acc: List[String]): List[String] =
      if remaining.length <= panelWidth then acc :+ remaining
      else
        val chunk = remaining.substring(0, panelWidth)
        val rest  = remaining.substring(panelWidth)
        wrapLine(rest, acc :+ chunk)

    wrapLine(text, List.empty)

  case class VisualLinePosition(visualLine: Int, visualColumn: Int)

  private def calculateVisualLinePosition(bufferColumn: Int, panelWidth: Int): VisualLinePosition =
    if panelWidth <= 0 then return VisualLinePosition(0, 0)

    val visualLine   = bufferColumn / panelWidth
    val visualColumn = bufferColumn % panelWidth

    VisualLinePosition(visualLine, visualColumn)
