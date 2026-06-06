package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class ScrollingNavigationSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager.apply(logger)(using balance, LoggerFactory[IO]).unsafeRunSync()

  behavior of "Scrolling and Navigation in Editor Panes"

  it should "handle vertical scrolling in large files" in {
    val program = for
      sm <- IO(makeStateManager())
      // Given: Large file with many lines
      largeContent = (1 to 1000).map(i => s"Line $i with some content").mkString("\n")
      bufferId <- sm.createBuffer(largeContent)
      state    <- sm.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _ <- sm.setBufferForPane(paneId, bufferId)
      _ <- sm.setCursorPosition(paneId, 0, 0)
      _ <- sm.setViewport(paneId, Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80))

      // When: Scroll down with Page Down
      _                  <- sm.applyEvent(PageDown)
      afterPageDownState <- sm.getCurrentState

      // When: Scroll down more with Ctrl+End (go to end of file)
      _             <- sm.applyEvent(MoveToEndOfFile)
      afterEndState <- sm.getCurrentState
    yield
      // Then: Viewport should move down after PageDown
      val pane1   = afterPageDownState.layout.editorPanes(paneId)
      val buffer1 = pane1.bufferId.flatMap(afterPageDownState.buffers.get).get
      buffer1.viewport.topLine should be > 0

      // Then: Should be at end of file after MoveToEndOfFile
      val pane2   = afterEndState.layout.editorPanes(paneId)
      val buffer2 = pane2.bufferId.flatMap(afterEndState.buffers.get).get
      buffer2.cursors.head.line shouldBe 999            // Last line (0-indexed)
      buffer2.viewport.topLine should be >= (1000 - 25) // Viewport shows last lines

    program.unsafeRunSync()
  }

  it should "handle horizontal scrolling in wide lines" in {
    val program = for
      sm <- IO(makeStateManager())
      // Given: File with very long lines
      wideContent = List(
        "A" * 200, // 200 character line
        "B" * 150,
        "C" * 300
      ).mkString("\n")
      bufferId <- sm.createBuffer(wideContent)
      state    <- sm.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _ <- sm.setBufferForPane(paneId, bufferId)
      _ <- sm.updateState { current =>
        current.copy(
          buffers = current.buffers.updated(
            bufferId,
            current.buffers(bufferId).copy(language = Some(LanguageId.Scala))
          )
        )
      }
      _ <- sm.setCursorPosition(paneId, 0, 150)
      _ <- sm.setViewport(paneId, Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80))

      // When: Scroll horizontally to bring cursor into view
      _                <- sm.ensureCursorVisible(paneId)
      afterScrollState <- sm.getCurrentState
    yield
      // Then: Viewport should scroll horizontally
      val pane   = afterScrollState.layout.editorPanes(paneId)
      val buffer = pane.bufferId.flatMap(afterScrollState.buffers.get).get
      buffer.viewport.leftColumn should be >= (150 - 80) // Cursor should be visible

    program.unsafeRunSync()
  }

  it should "use measured horizontal scrolling for proportional markdown lines" in {
    val program = for
      sm       <- IO(makeStateManager())
      _        <- sm.updateState(_.copy(config = AppConfig.default.withLineNumbers(false).withGutter(false)))
      bufferId <- sm.createBuffer("iiiiiiiiWW")
      state    <- sm.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _ <- sm.setBufferForPane(paneId, bufferId)
      _ <- sm.updateState { current =>
        current.copy(
          buffers = current.buffers.updated(
            bufferId,
            current.buffers(bufferId).copy(language = Some(LanguageId.Markdown))
          )
        )
      }
      _ <- sm.setViewport(paneId, Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 4))
      _ <- sm.setCursorPosition(paneId, 0, 10)
      _ <- sm.ensureCursorVisible(paneId)
      afterScrollState <- sm.getCurrentState
    yield
      val pane   = afterScrollState.layout.editorPanes(paneId)
      val buffer = pane.bufferId.flatMap(afterScrollState.buffers.get).get
      buffer.viewport.leftColumn should be < 7
      buffer.viewport.leftColumn should be >= 0

    program.unsafeRunSync()
  }

  it should "use measured horizontal scrolling after deleting a proportional selection" in {
    val program = for
      sm       <- IO(makeStateManager())
      _        <- sm.updateState(_.copy(config = AppConfig.default.withLineNumbers(false).withGutter(false)))
      bufferId <- sm.createBuffer("iiiiiiiiWW")
      state    <- sm.getCurrentState
      paneId = state.layout.editorPanes.keys.head
      _ <- sm.setBufferForPane(paneId, bufferId)
      _ <- sm.updateState { current =>
        current.copy(
          buffers = current.buffers.updated(
            bufferId,
            current
              .buffers(bufferId)
              .copy(
                language = Some(LanguageId.Markdown),
                viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 4),
                cursors = List(CursorPosition(0, 10)),
                selection = Some(Selection(CursorPosition(0, 8), CursorPosition(0, 10)))
              )
          )
        )
      }
      _                <- sm.applyEvent(DeleteBackward)
      afterDeleteState <- sm.getCurrentState
    yield
      val buffer = afterDeleteState.buffers(bufferId)
      val font = FontLoader.previewTextFont(
        FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f, enableLigatures = true)
      )
      val expectedLeftColumn = TextLayoutSnapshot.leftColumnForCursorVisibility(
        lineText = buffer.content.getLine(0).getOrElse(""),
        cursorColumn = 8,
        visibleWidthPx = CellMetrics.fromFont(font).charWidth * 4,
        font = font
      )

      buffer.content.collect() shouldBe "iiiiiiii"
      buffer.cursors.head shouldBe CursorPosition(0, 8)
      buffer.viewport.leftColumn shouldBe expectedLeftColumn

    program.unsafeRunSync()
  }

  it should "handle mouse wheel scrolling" in new ScrollFixture:
    // Given: File with content
    val content  = (1 to 100).map(i => s"Line $i").mkString("\n")
    val bufferId = stateManager.createBuffer(content).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 0).unsafeRunSync()
    stateManager
      .setViewport(paneId, Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80))
      .unsafeRunSync()

    // When: Mouse wheel scroll down (3 lines)
    stateManager.applyEvent(ScrollDown(3)).unsafeRunSync()

    // Then: Viewport should scroll down 3 lines
    val afterScrollDownState = stateManager.getCurrentState.unsafeRunSync()
    val pane1                = afterScrollDownState.layout.editorPanes(paneId)
    val buffer1              = pane1.bufferId.flatMap(afterScrollDownState.buffers.get).get
    buffer1.viewport.topLine shouldBe 3

    // When: Mouse wheel scroll up (2 lines)
    stateManager.applyEvent(ScrollUp(2)).unsafeRunSync()

    // Then: Viewport should scroll up
    val afterScrollUpState = stateManager.getCurrentState.unsafeRunSync()
    val pane2              = afterScrollUpState.layout.editorPanes(paneId)
    val buffer2            = pane2.bufferId.flatMap(afterScrollUpState.buffers.get).get
    buffer2.viewport.topLine shouldBe 1

  it should "handle smooth scrolling animations" in new ScrollFixture:
    // Given: File with content
    val content  = (1 to 50).map(i => s"Line $i").mkString("\n")
    val bufferId = stateManager.createBuffer(content).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 0).unsafeRunSync()
    stateManager
      .setViewport(paneId, Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80))
      .unsafeRunSync()

    // When: Initiate smooth scroll to line 30
    stateManager.smoothScrollTo(paneId, 30).unsafeRunSync()

    // Then: Should start smooth scrolling animation
    val duringScrollState = stateManager.getCurrentState.unsafeRunSync()
    val pane              = duringScrollState.layout.editorPanes(paneId)
    pane.smoothScrolling shouldBe Some(SmoothScrollState(targetTopLine = 30, progress = 0.0))

    // When: Progress smooth scroll animation
    stateManager.progressSmoothScroll(paneId, 0.5).unsafeRunSync()

    // Then: Should be partially scrolled
    val halfwayState = stateManager.getCurrentState.unsafeRunSync()
    val pane2        = halfwayState.layout.editorPanes(paneId)
    val buffer2      = pane2.bufferId.flatMap(halfwayState.buffers.get).get
    buffer2.viewport.topLine should be > 0
    buffer2.viewport.topLine should be < 30

    // When: Complete smooth scroll
    stateManager.progressSmoothScroll(paneId, 1.0).unsafeRunSync()

    // Then: Should reach target
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val pane3      = finalState.layout.editorPanes(paneId)
    val buffer3    = pane3.bufferId.flatMap(finalState.buffers.get).get
    buffer3.viewport.topLine shouldBe 30
    pane3.smoothScrolling shouldBe None

  it should "handle goto line functionality" in new ScrollFixture:
    // Given: Large file
    val content  = (1 to 500).map(i => s"Line $i content").mkString("\n")
    val bufferId = stateManager.createBuffer(content).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 0).unsafeRunSync()
    stateManager
      .setViewport(paneId, Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80))
      .unsafeRunSync()

    // When: Open goto line dialog (Ctrl+G)
    stateManager.applyEvent(OpenGotoLine).unsafeRunSync()

    // Then: Modal should be open
    val modalState   = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = modalState.modalSurface
    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("")))
    modalState.focus shouldBe Focus.Surface(modalSurface.get.id)

    // When: Type line number
    "250".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    // Then: Should jump to line 250
    val afterGotoState = stateManager.getCurrentState.unsafeRunSync()
    afterGotoState.modalSurface shouldBe None
    val pane   = afterGotoState.layout.editorPanes(paneId)
    val buffer = pane.bufferId.flatMap(afterGotoState.buffers.get).get
    buffer.cursors.head.line shouldBe 249           // 0-indexed, so line 250 = index 249
    buffer.viewport.topLine should be >= (249 - 12) // Center line in viewport
    buffer.viewport.topLine should be <= 249

  it should "handle find and scroll to search results" in new ScrollFixture:
    // Given: File with searchable content
    val content = (1 to 200)
      .map { i =>
        if i % 50 == 0 then s"Line $i SPECIAL_MARKER content"
        else s"Line $i normal content"
      }
      .mkString("\n")
    val bufferId = stateManager.createBuffer(content).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 0).unsafeRunSync()
    stateManager
      .setViewport(paneId, Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80))
      .unsafeRunSync()

    // When: Open find dialog (Ctrl+F)
    stateManager.applyEvent(OpenFind).unsafeRunSync()

    // Type search term
    "SPECIAL_MARKER".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    // Then: Should scroll to first occurrence (line 50)
    val afterFindState = stateManager.getCurrentState.unsafeRunSync()
    val pane1          = afterFindState.layout.editorPanes(paneId)
    val buffer1        = pane1.bufferId.flatMap(afterFindState.buffers.get).get
    buffer1.cursors.head.line shouldBe 49           // Line 50 (0-indexed)
    buffer1.viewport.topLine should be >= (49 - 12) // Should be visible
    buffer1.viewport.topLine should be <= 49

    // When: Find next (F3)
    stateManager.applyEvent(FindNext).unsafeRunSync()

    // Then: Should scroll to next occurrence (line 100)
    val afterNextState = stateManager.getCurrentState.unsafeRunSync()
    val pane2          = afterNextState.layout.editorPanes(paneId)
    val buffer2        = pane2.bufferId.flatMap(afterNextState.buffers.get).get
    buffer2.cursors.head.line shouldBe 99 // Line 100 (0-indexed)

    // When: Find next again
    stateManager.applyEvent(FindNext).unsafeRunSync()

    // Then: Should scroll to line 150
    val afterNext2State = stateManager.getCurrentState.unsafeRunSync()
    val pane3           = afterNext2State.layout.editorPanes(paneId)
    val buffer3         = pane3.bufferId.flatMap(afterNext2State.buffers.get).get
    buffer3.cursors.head.line shouldBe 149 // Line 150 (0-indexed)

  it should "handle viewport synchronization across split panes" in new ScrollFixture:
    // Given: Same file in multiple panes
    val content  = (1 to 100).map(i => s"Line $i").mkString("\n")
    val bufferId = stateManager.createBuffer(content).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    val pane1 = state.layout.editorPanes.keys.head

    // Associate the buffer with the first pane
    stateManager.setBufferForPane(pane1, bufferId).unsafeRunSync()

    // Create split pane with same buffer
    val pane2 = stateManager.splitPaneHorizontal(pane1, Some(bufferId)).unsafeRunSync()

    val defaultViewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80)
    stateManager.setViewport(pane1, defaultViewport).unsafeRunSync()
    stateManager.setViewport(pane2, defaultViewport).unsafeRunSync() // ← Set buffer viewport correctly
    stateManager.setPaneProperties(pane2, _.copy(syncedScrolling = true)).unsafeRunSync() // ← Only set syncedScrolling

    // When: Scroll in first pane
    stateManager.switchToPane(pane1).unsafeRunSync()
    stateManager.applyEvent(ScrollDown(10)).unsafeRunSync()

    // Then: Both panes should scroll if synchronized
    val afterScrollState = stateManager.getCurrentState.unsafeRunSync()
    val finalPane1       = afterScrollState.layout.editorPanes(pane1)
    val finalPane2       = afterScrollState.layout.editorPanes(pane2)
    val finalBuffer1     = finalPane1.bufferId.flatMap(afterScrollState.buffers.get).get
    val finalBuffer2     = finalPane2.bufferId.flatMap(afterScrollState.buffers.get).get

    finalBuffer1.viewport.topLine shouldBe 10
    if finalPane2.syncedScrolling then finalBuffer2.viewport.topLine shouldBe 10

  it should "handle minimap scrolling and navigation" in new ScrollFixture:
    // Given: Large file with minimap enabled
    val content  = (1 to 1000).map(i => s"Line $i").mkString("\n")
    val bufferId = stateManager.createBuffer(content).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 0).unsafeRunSync()
    stateManager
      .setPaneProperties(
        paneId,
        _.copy(
          viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80),
          minimapVisible = true
        )
      )
      .unsafeRunSync()

    // When: Click on minimap (simulate click at 50% down)
    val targetLine = 500 // Middle of file
    stateManager.clickMinimap(paneId, targetLine).unsafeRunSync()

    // Then: Should scroll to clicked location
    val afterClickState = stateManager.getCurrentState.unsafeRunSync()
    val pane            = afterClickState.layout.editorPanes(paneId)
    val buffer          = pane.bufferId.flatMap(afterClickState.buffers.get).get
    buffer.viewport.topLine should be >= (targetLine - 12)
    buffer.viewport.topLine should be <= (targetLine + 12)

  it should "handle edge cases with scrolling bounds" in new ScrollFixture:
    // Given: Small file
    val content  = "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"
    val bufferId = stateManager.createBuffer(content).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, 0).unsafeRunSync()
    stateManager
      .setViewport(paneId, Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80))
      .unsafeRunSync()

    // When: Try to scroll beyond file bounds
    stateManager.applyEvent(ScrollDown(100)).unsafeRunSync() // Way more than file has

    // Then: Should clamp to file bounds
    val afterScrollState = stateManager.getCurrentState.unsafeRunSync()
    val pane             = afterScrollState.layout.editorPanes(paneId)
    val buffer           = pane.bufferId.flatMap(afterScrollState.buffers.get).get
    buffer.viewport.topLine shouldBe 0 // Can't scroll down in small file

    // When: Try to scroll up beyond beginning
    stateManager.applyEvent(ScrollUp(100)).unsafeRunSync()

    // Then: Should stay at beginning
    val afterScrollUpState = stateManager.getCurrentState.unsafeRunSync()
    val pane2              = afterScrollUpState.layout.editorPanes(paneId)
    val buffer2            = pane2.bufferId.flatMap(afterScrollUpState.buffers.get).get
    buffer2.viewport.topLine shouldBe 0

  trait ScrollFixture:

    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))

    val stateManager: StateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()
