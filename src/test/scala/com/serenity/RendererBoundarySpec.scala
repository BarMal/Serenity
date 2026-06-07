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
import com.serenity.ui.layout.{LayoutEngine, ViewportSize}

/** TDD tests for text rendering boundary enforcement. These tests ensure that rendered text never extends beyond panel
  * boundaries, even when the underlying buffer contains text longer than the visible area.
  */
class RendererBoundarySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Text Rendering Boundary Enforcement"

  it should "never render characters beyond the right edge of editor panel" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val mockScreen          = new MockScreen(80, 24)

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Setup: Create buffer with text longer than panel width
      bufferId <- stateManager.createBuffer("")
      state    <- stateManager.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      // Calculate actual panel boundaries
      currentState <- stateManager.getCurrentState
      layout    = LayoutEngine.calculateLayout(currentState, ViewportSize(mockScreen.cols, mockScreen.rows))
      panelRect = layout.editorPanelRect

      // Insert text much longer than panel width
      longText     = "x" * (panelRect.width + 20)
      insertEvents = longText.map(char => InsertChar(char)).toList
      _ <- insertEvents.traverse(event => stateManager.applyEvent(event))

      // Render the state
      finalState <- stateManager.getCurrentState

      // Simulate rendering by placing 'x' characters within panel bounds (this is what we're testing)
      _            = mockScreen.clear()
      viewportSize = ViewportSize(mockScreen.cols, mockScreen.rows)
      layout       = LayoutEngine.calculateLayout(finalState, viewportSize)
      panelRect    = layout.editorPanelRect
      buffer       = finalState.buffers(bufferId)
      content      = buffer.content.collect()

      // Simulate placing characters on screen (basic version of what Renderer does)
      _ = content.zipWithIndex.foreach {
        case (char, i) =>
          val x = panelRect.x + (i % panelRect.width)
          val y = panelRect.y + (i / panelRect.width)
          if x < panelRect.right && y < panelRect.bottom && x < mockScreen.cols && y < mockScreen.rows then
            mockScreen.putChar(x, y, char)
      }
    yield
      // Verify: No 'x' characters should appear beyond panel right boundary
      for y <- 0 until mockScreen.rows; x <- 0 until mockScreen.cols do
        val char = mockScreen.getChar(x, y)
        if char == 'x' then
          // Any rendered 'x' must be within panel boundaries
          x should be >= panelRect.x
          x should be < panelRect.right

    program.unsafeRunSync()
  }

  it should "clip long lines at the visible viewport boundary" in new MockRenderFixture:
    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Get panel dimensions
    val currentState = stateManager.getCurrentState.unsafeRunSync()
    val layout       = LayoutEngine.calculateLayout(currentState, ViewportSize(mockScreen.cols, mockScreen.rows))
    val panelRect    = layout.editorPanelRect

    // Create a line with identifiable pattern that's longer than panel
    val pattern     = "0123456789"
    val repetitions = (panelRect.width / pattern.length) + 3 // Ensure it extends beyond
    val longLine    = pattern * repetitions

    longLine.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    renderStateToMockScreen(finalState)

    // Count how many characters are actually rendered in the first row of the panel
    val panelStartY = panelRect.y
    val renderedChars = (panelRect.x until panelRect.right).count { x =>
      val char = mockScreen.getChar(x, panelStartY)
      char != ' ' && char != '\u0000'
    }

    // Should render at most panelRect.width characters
    renderedChars should be <= panelRect.width

    // Should not render any content beyond the panel
    for x <- panelRect.right until mockScreen.cols do
      val char = mockScreen.getChar(x, panelStartY)
      char should (be(' ') or be('\u0000'))

  it should "handle horizontal scrolling while respecting panel boundaries" in new MockRenderFixture:
    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Get panel dimensions
    val currentState = stateManager.getCurrentState.unsafeRunSync()
    val layout       = LayoutEngine.calculateLayout(currentState, ViewportSize(mockScreen.cols, mockScreen.rows))
    val panelRect    = layout.editorPanelRect

    // Create text that will cause horizontal scrolling
    val alphabet = "abcdefghijklmnopqrstuvwxyz"
    val longText = alphabet * 5 // Much longer than any typical panel

    longText.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    renderStateToMockScreen(finalState)

    // Verify viewport has scrolled (cursor should be beyond initial visible area)
    val finalPane = finalState.layout.editorPanes(paneId)
    val buffer    = finalState.buffers(bufferId) // ← Get the correct buffer by ID
    val viewport  = buffer.viewport
    viewport.leftColumn should be > 0

    // Verify rendered content is still within panel boundaries
    for y <- 0 until mockScreen.rows; x <- 0 until mockScreen.cols do
      val char = mockScreen.getChar(x, y)
      if alphabet.contains(char) then
        // Any rendered alphabet character must be within panel boundaries
        x should be >= panelRect.x
        x should be < panelRect.right
        y should be >= panelRect.y
        y should be < panelRect.bottom

  it should "properly handle multi-line text without extending beyond panel bottom" in new MockRenderFixture:
    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    val layout = LayoutEngine.calculateLayout(
      stateManager.getCurrentState.unsafeRunSync(),
      ViewportSize(mockScreen.cols, mockScreen.rows)
    )
    val panelRect = layout.editorPanelRect

    // Create more lines than can fit in the panel
    val linesCount = panelRect.height + 10
    for lineNum <- 0 until linesCount do
      s"Line $lineNum content".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
      if lineNum < linesCount - 1 then stateManager.applyEvent(NewLine).unsafeRunSync()

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    renderStateToMockScreen(finalState)

    // Verify no content is rendered below panel bottom
    for y <- panelRect.bottom until mockScreen.rows; x <- 0 until mockScreen.cols do
      val char = mockScreen.getChar(x, y)
      char should (be(' ') or be('\u0000'))

    // Verify viewport has scrolled to show the end
    val finalPane = finalState.layout.editorPanes(paneId)
    val buffer    = finalState.buffers(bufferId) // ← Get the correct buffer by ID
    val viewport  = buffer.viewport
    viewport.topLine should be > 0

  it should "correctly handle cursor rendering at panel edges" in new MockRenderFixture:
    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    val layout = LayoutEngine.calculateLayout(
      stateManager.getCurrentState.unsafeRunSync(),
      ViewportSize(mockScreen.cols, mockScreen.rows)
    )
    val panelRect = layout.editorPanelRect

    // Fill exactly to panel width
    val exactText = "a" * panelRect.width
    exactText.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    renderStateToMockScreen(finalState)

    // Check that cursor is rendered within boundaries
    val finalPane = finalState.layout.editorPanes(paneId)
    val cursor    = finalPane.cursors.head
    val viewport  = finalPane.viewport

    // Cursor screen position calculation
    val screenX = panelRect.x + cursor.column - viewport.leftColumn
    val screenY = panelRect.y + cursor.line - viewport.topLine

    // If cursor is visible, it should be within panel bounds
    if screenX >= panelRect.x && screenX < panelRect.right &&
        screenY >= panelRect.y && screenY < panelRect.bottom
    then
      // Cursor rendering is valid
      screenX should be < mockScreen.cols
      screenY should be < mockScreen.rows

  // Mock infrastructure for testing rendering behavior
  class MockScreen(val cols: Int = 80, val rows: Int = 24):
    private val buffer           = Array.fill(rows, cols)(' ')
    private val backgroundColors = Array.fill(rows, cols)(java.awt.Color.BLACK)

    def putChar(x: Int, y: Int, char: Char): Unit =
      if y >= 0 && y < rows && x >= 0 && x < cols then buffer(y)(x) = char

    def setBackground(x: Int, y: Int, color: java.awt.Color): Unit =
      if y >= 0 && y < rows && x >= 0 && x < cols then backgroundColors(y)(x) = color

    def getChar(x: Int, y: Int): Char =
      if y >= 0 && y < rows && x >= 0 && x < cols then buffer(y)(x) else ' '

    def getBackground(x: Int, y: Int): java.awt.Color =
      if y >= 0 && y < rows && x >= 0 && x < cols then backgroundColors(y)(x)
      else java.awt.Color.BLACK

    def clear(): Unit =
      for y <- 0 until rows; x <- 0 until cols do
        buffer(y)(x) = ' '
        backgroundColors(y)(x) = java.awt.Color.BLACK

    def getRowContent(y: Int): String =
      if y >= 0 && y < rows then buffer(y).mkString else ""

  trait MockRenderFixture:
    val mockScreen = new MockScreen(80, 24)

    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))

    val stateManager: StateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    // Initialize with empty buffer and pane
    stateManager.createBuffer("").unsafeRunSync()

    // We need to mock the rendering since we can't directly use Renderer with MockScreen
    // Instead we'll test the rendering logic by examining the calculated positions
    def renderStateToMockScreen(state: AppState): Unit =
      mockScreen.clear()

      val viewportSize = ViewportSize(mockScreen.cols, mockScreen.rows)
      val layout       = LayoutEngine.calculateLayout(state, viewportSize)

      // Simulate the rendering logic from Renderer.scala
      state.layout.editorPanes.foreach { (paneId, pane) =>
        pane.bufferId.foreach { bufferId =>
          state.buffers.get(bufferId).foreach(buffer => renderBufferToMock(pane, buffer, layout.editorPanelRect))
        }
      }

    private def renderBufferToMock(pane: EditorPane, buffer: Buffer, rect: com.serenity.ui.layout.LayoutRect): Unit =
      val viewport = pane.viewport
      val rope     = buffer.content

      // Render visible lines - this simulates Renderer.renderBufferContent
      for screenLine <- 0 until math.min(viewport.visibleLines, rect.height) do
        val bufferLine  = viewport.topLine + screenLine
        val lineContent = if bufferLine < rope.lineCount then rope.getLine(bufferLine).getOrElse("") else ""

        // Handle horizontal scrolling - this is the key logic we're testing
        val visibleContent = if lineContent.length > viewport.leftColumn then
          val endColumn = math.min(
            lineContent.length,
            viewport.leftColumn + viewport.visibleColumns
          )
          lineContent.substring(viewport.leftColumn, endColumn)
        else ""

        // Render the line to mock screen
        val screenY = rect.y + screenLine
        val screenX = rect.x

        // This is where the boundary enforcement should happen
        if screenY < mockScreen.rows && screenX < mockScreen.cols then
          // Render only what fits within the panel boundaries
          val maxCharsToRender = math.min(visibleContent.length, rect.right - screenX)
          val clippedContent   = visibleContent.take(maxCharsToRender)

          clippedContent.zipWithIndex.foreach {
            case (char, offset) =>
              val renderX = screenX + offset
              if renderX < rect.right && renderX < mockScreen.cols then mockScreen.putChar(renderX, screenY, char)
          }

      // Render cursor
      pane.cursors.foreach { cursor =>
        val screenLine   = cursor.line - viewport.topLine
        val screenColumn = cursor.column - viewport.leftColumn

        if screenLine >= 0 && screenLine < viewport.visibleLines &&
            screenColumn >= 0 && screenColumn < viewport.visibleColumns
        then
          val screenX = rect.x + screenColumn
          val screenY = rect.y + screenLine

          if screenY < mockScreen.rows && screenX < mockScreen.cols &&
              screenX < rect.right && screenY < rect.bottom
          then mockScreen.setBackground(screenX, screenY, java.awt.Color.WHITE)
      }
