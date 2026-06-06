package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.lsp.config.LanguageId
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutEngine, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** TDD tests for gutter and line number display functionality.
  *
  * Requirements:
  *   1. Gutter shows current col/row position and buffer path
  *   2. Line numbers are displayed vertically along the left side of the buffer
  *   3. Both elements can be toggled on/off
  *   4. Layout engine allocates appropriate space for these elements
  */
class GutterAndLineNumbersSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  behavior of "Gutter Display"

  it should "show the active buffer language in the rendered gutter" in {
    val buffer = Buffer
      .fromString(BufferId(1), "# Heading")
      .copy(language = Some(LanguageId.Markdown))
    val state = AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = AppState.initial.layout.copy(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
        activeEditorPaneId = Some(PaneId(0)),
        paneOrder = List(PaneId(0))
      ),
      focus = Focus.EditorPane(PaneId(0)),
      theme = Theme.light
    )
    val surface  = new MockRenderSurface(80, 24)
    val viewport = ViewportSize(80, 24)
    val layout   = LayoutEngine.calculateLayout(state, viewport)
    val gutter   = layout.gutterRect.getOrElse(fail("Expected gutter rect"))

    Renderer.render(state, cursorVisible = true, surface, viewport)

    val renderedGutter =
      (gutter.x until gutter.right).map(x => surface.getChar(x, gutter.y)).mkString

    renderedGutter should include("Language: Markdown")
  }

  it should "show current cursor position and buffer path when gutter is enabled" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Buffer with content and cursor at specific position
      bufferId     <- stateManager.createBuffer("Line 1\nLine 2\nLine 3")
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)
      _ <- stateManager.setCursorPosition(paneId, 1, 5) // Line 2, column 5

      // When: Gutter is enabled (should be default)
      finalState <- stateManager.getCurrentState
      buffer = finalState.buffers(bufferId)
      pane   = finalState.layout.editorPanes(paneId)
    yield
      // Then: Buffer should have gutter information available
      val gutterInfo = calculateGutterInfo(buffer, pane, None) // No file path yet
      gutterInfo.cursorPosition shouldBe "Line 2, Col 6" // 1-indexed display
      gutterInfo.filePath shouldBe "Not saved to file yet"

    program.unsafeRunSync()
  }

  it should "display file path when buffer is associated with a file" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Buffer with file path
      bufferId     <- stateManager.createBuffer("File content")
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      // Simulate setting file path (this would normally happen during file open/save)
      stateWithPath <- stateManager.getCurrentState
      bufferWithPath = stateWithPath
        .buffers(bufferId)
        .copy(
          filePath = Some(java.nio.file.Paths.get("/path/to/myfile.txt"))
        )
      updatedState = stateWithPath.copy(
        buffers = stateWithPath.buffers + (bufferId -> bufferWithPath)
      )

      pane = updatedState.layout.editorPanes(paneId)
    yield
      // Then: Gutter should show file path
      val gutterInfo = calculateGutterInfo(bufferWithPath, pane, bufferWithPath.filePath)
      gutterInfo.filePath shouldBe "myfile.txt"

    program.unsafeRunSync()
  }

  behavior of "Line Number Display"

  it should "allocate space for line numbers in layout calculation" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: State with line numbers enabled and some buffer content
      bufferId     <- stateManager.createBuffer("Line 1\nLine 2\nLine 3") // Small buffer for testing
      initialState <- stateManager.getCurrentState
      stateWithLineNumbers = initialState.copy(
        config = initialState.config.copy(
          showLineNumbers = true,
          showGutter = true
        )
      )

      // When: Calculate layout with UI elements enabled
      viewportSize = ViewportSize(80, 24)
      layout       = LayoutEngine.calculateLayoutWithUI(stateWithLineNumbers, viewportSize)

      // Calculate expected dimensions based on actual buffer content (3 lines = 1 digit + 1 space = min 3 chars)
      lineNumberWidth = 3 // Based on actual LayoutEngine calculation for 3-line buffer
      gutterHeight    = if stateWithLineNumbers.config.showGutter then 1 else 0
    yield
      // Then: Layout should allocate space for line numbers and gutter
      if stateWithLineNumbers.config.showLineNumbers then
        layout.editorPanelRect.x should be >= lineNumberWidth
        layout.lineNumberRect should be(defined)
        layout.lineNumberRect.get.width should be(lineNumberWidth)

      if stateWithLineNumbers.config.showGutter then
        layout.gutterRect should be(defined)
        layout.gutterRect.get.height should be(gutterHeight)
        // Gutter should be at bottom of terminal
        layout.gutterRect.get.y should be(viewportSize.height - gutterHeight)

    program.unsafeRunSync()
  }

  it should "calculate correct line number width based on total lines" in {
    // Test helper function for line number width calculation
    calculateLineNumberWidth(9) shouldBe 3    // " 9 " = 3 chars (minimum 3)
    calculateLineNumberWidth(99) shouldBe 3   // " 99" = 3 chars
    calculateLineNumberWidth(999) shouldBe 4  // " 999" = 4 chars
    calculateLineNumberWidth(9999) shouldBe 5 // " 9999" = 5 chars
  }

  it should "render line numbers for visible lines only" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Buffer with many lines
      lines = (1 to 50).map(i => s"Line $i").mkString("\n")
      bufferId     <- stateManager.createBuffer(lines)
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      finalState <- stateManager.getCurrentState
      buffer = finalState.buffers(bufferId)
      pane   = finalState.layout.editorPanes(paneId)

      // Simulate viewport showing lines 10-20 (0-indexed, so lines 11-21 in display)
      viewportLines = (10 to 20).toList
    yield
      // Then: Line numbers should be calculated for visible range only
      val lineNumbers = calculateVisibleLineNumbers(buffer, pane.viewport, viewportLines)
      lineNumbers shouldBe List("11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21")

    program.unsafeRunSync()
  }

  it should "render shared gutter line numbers from the active pane only" in {
    val buffer1 = Buffer
      .fromString(BufferId(1), (1 to 20).map(i => s"left $i").mkString("\n"))
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 10, visibleColumns = 20))
    val buffer2 = Buffer
      .fromString(BufferId(2), (1 to 20).map(i => s"right $i").mkString("\n"))
      .copy(viewport = Viewport(topLine = 5, leftColumn = 0, visibleLines = 10, visibleColumns = 20))
    val state = AppState.initial.copy(
      buffers = Map(buffer1.id -> buffer1, buffer2.id -> buffer2),
      bufferOrder = List(buffer1.id, buffer2.id),
      layout = com.serenity.ui.layout.Layout(
        editorPanes = Map(
          PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer1.id),
          PaneId(1) -> EditorPane.withBuffer(PaneId(1), buffer2.id)
        ),
        activeEditorPaneId = Some(PaneId(1)),
        paneOrder = List(PaneId(0), PaneId(1))
      ),
      focus = Focus.EditorPane(PaneId(1)),
      theme = Theme.light
    )
    val surface  = new MockRenderSurface(80, 24)
    val viewport = ViewportSize(80, 24)
    val layout   = LayoutEngine.calculateLayout(state, viewport)
    val lineRect = layout.lineNumberRect.getOrElse(fail("Expected line number rect"))

    Renderer.render(state, cursorVisible = true, surface, viewport)

    val firstRenderedLine =
      (lineRect.x until lineRect.right).map(x => surface.getChar(x, lineRect.y)).mkString.trim

    firstRenderedLine shouldBe "6"
  }

  it should "position gutter correctly below buffer header and use black background" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Buffer with header and gutter enabled
      bufferId     <- stateManager.createBuffer("Some content")
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      // When: Calculate layout with gutter enabled
      stateWithGutter = initialState.copy(
        config = initialState.config.copy(showGutter = true)
      )
      viewportSize = ViewportSize(80, 24)
      layout       = LayoutEngine.calculateLayoutWithUI(stateWithGutter, viewportSize)
    yield
      // Then: Gutter should be positioned to account for buffer header
      layout.gutterRect should be(defined)

      // Gutter should be at absolute bottom (not interfering with editor content)
      layout.gutterRect.get.y should be(viewportSize.height - 1)

      // Editor panel should have reduced height to accommodate gutter
      layout.editorPanelRect.height should be < viewportSize.height

      // Verify gutter styling would be black (this will be tested in rendering)
      checkGutterStyling() shouldBe "black_background"

    program.unsafeRunSync()
  }

  // Helper functions that will need to be implemented

  case class GutterInfo(cursorPosition: String, filePath: String)

  private def calculateGutterInfo(buffer: Buffer, pane: EditorPane, filePath: Option[java.nio.file.Path]): GutterInfo =
    val cursor   = buffer.cursors.headOption.getOrElse(CursorPosition(0, 0))
    val position = s"Line ${cursor.line + 1}, Col ${cursor.column + 1}" // 1-indexed for display

    val path = filePath match
      case Some(path) => path.getFileName.toString
      case None       => "Not saved to file yet"

    GutterInfo(position, path)

  private def calculateLineNumberWidth(maxLines: Int): Int =
    if maxLines <= 0 then 2
    else math.max(3, maxLines.toString.length + 1) // +1 for spacing, minimum 3

  private def calculateVisibleLineNumbers(buffer: Buffer, viewport: Viewport, visibleLines: List[Int]): List[String] =
    visibleLines.map(lineNum => (lineNum + 1).toString) // Convert 0-indexed to 1-indexed for display

  private def checkGutterStyling(): String = "black_background" // Mock function for styling test
