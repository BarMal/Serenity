package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class ScrollingNavigationSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Scrolling and Navigation in Editor Panes"

  it should "handle vertical scrolling in large files" in new ScrollFixture:
    // Given: Large file with many lines
    val largeContent = (1 to 1000).map(i => s"Line $i with some content").mkString("\n")
    val bufferId     = stateManager.createBuffer(largeContent).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    val updatedPane = state.layout
      .editorPanes(paneId)
      .copy(
        bufferId = Some(bufferId),
        cursors = List(CursorPosition(0, 0)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80)
      )
    val updatedLayout = state.layout.copy(
      editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
    )
    stateManager.getCurrentState
      .flatMap(currentState => IO.pure(currentState.copy(layout = updatedLayout)))
      .unsafeRunSync()

    // When: Scroll down with Page Down
    stateManager.applyEvent(PageDown).unsafeRunSync()

    // Then: Viewport should move down
    val afterPageDownState = stateManager.getCurrentState.unsafeRunSync()
    val pane1              = afterPageDownState.layout.editorPanes(paneId)
    pane1.viewport.topLine should be > 0

    // When: Scroll down more with Ctrl+End (go to end of file)
    stateManager.applyEvent(MoveToEndOfFile).unsafeRunSync()

    // Then: Should be at end of file
    val afterEndState = stateManager.getCurrentState.unsafeRunSync()
    val pane2         = afterEndState.layout.editorPanes(paneId)
    pane2.cursors.head.line shouldBe 999            // Last line (0-indexed)
    pane2.viewport.topLine should be >= (1000 - 25) // Viewport shows last lines

  it should "handle horizontal scrolling in wide lines" in new ScrollFixture:
    // Given: File with very long lines
    val wideContent = List(
      "A" * 200, // 200 character line
      "B" * 150,
      "C" * 300
    ).mkString("\n")
    val bufferId = stateManager.createBuffer(wideContent).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    val updatedPane = state.layout
      .editorPanes(paneId)
      .copy(
        bufferId = Some(bufferId),
        cursors = List(CursorPosition(0, 150)), // Far right on first line
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80)
      )
    val updatedLayout = state.layout.copy(
      editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
    )
    stateManager.getCurrentState
      .flatMap(currentState => IO.pure(currentState.copy(layout = updatedLayout)))
      .unsafeRunSync()

    // When: Scroll horizontally to bring cursor into view
    stateManager.ensureCursorVisible(paneId).unsafeRunSync()

    // Then: Viewport should scroll horizontally
    val afterScrollState = stateManager.getCurrentState.unsafeRunSync()
    val pane             = afterScrollState.layout.editorPanes(paneId)
    pane.viewport.leftColumn should be >= (150 - 80) // Cursor should be visible

  it should "handle mouse wheel scrolling" in new ScrollFixture:
    // Given: File with content
    val content  = (1 to 100).map(i => s"Line $i").mkString("\n")
    val bufferId = stateManager.createBuffer(content).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    val updatedPane = state.layout
      .editorPanes(paneId)
      .copy(
        bufferId = Some(bufferId),
        cursors = List(CursorPosition(0, 0)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80)
      )
    val updatedLayout = state.layout.copy(
      editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
    )
    stateManager.getCurrentState
      .flatMap(currentState => IO.pure(currentState.copy(layout = updatedLayout)))
      .unsafeRunSync()

    // When: Mouse wheel scroll down (3 lines)
    stateManager.applyEvent(ScrollDown(3)).unsafeRunSync()

    // Then: Viewport should scroll down 3 lines
    val afterScrollDownState = stateManager.getCurrentState.unsafeRunSync()
    val pane1                = afterScrollDownState.layout.editorPanes(paneId)
    pane1.viewport.topLine shouldBe 3

    // When: Mouse wheel scroll up (2 lines)
    stateManager.applyEvent(ScrollUp(2)).unsafeRunSync()

    // Then: Viewport should scroll up
    val afterScrollUpState = stateManager.getCurrentState.unsafeRunSync()
    val pane2              = afterScrollUpState.layout.editorPanes(paneId)
    pane2.viewport.topLine shouldBe 1

  it should "handle smooth scrolling animations" in new ScrollFixture:
    // Given: File with content
    val content  = (1 to 50).map(i => s"Line $i").mkString("\n")
    val bufferId = stateManager.createBuffer(content).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    val updatedPane = state.layout
      .editorPanes(paneId)
      .copy(
        bufferId = Some(bufferId),
        cursors = List(CursorPosition(0, 0)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80)
      )
    val updatedLayout = state.layout.copy(
      editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
    )
    stateManager.getCurrentState
      .flatMap(currentState => IO.pure(currentState.copy(layout = updatedLayout)))
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
    pane2.viewport.topLine should be > 0
    pane2.viewport.topLine should be < 30

    // When: Complete smooth scroll
    stateManager.progressSmoothScroll(paneId, 1.0).unsafeRunSync()

    // Then: Should reach target
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    val pane3      = finalState.layout.editorPanes(paneId)
    pane3.viewport.topLine shouldBe 30
    pane3.smoothScrolling shouldBe None

  it should "handle goto line functionality" in new ScrollFixture:
    // Given: Large file
    val content  = (1 to 500).map(i => s"Line $i content").mkString("\n")
    val bufferId = stateManager.createBuffer(content).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    val updatedPane = state.layout
      .editorPanes(paneId)
      .copy(
        bufferId = Some(bufferId),
        cursors = List(CursorPosition(0, 0)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80)
      )
    val updatedLayout = state.layout.copy(
      editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
    )
    stateManager.getCurrentState
      .flatMap(currentState => IO.pure(currentState.copy(layout = updatedLayout)))
      .unsafeRunSync()

    // When: Open goto line dialog (Ctrl+G)
    stateManager.applyEvent(OpenGotoLine).unsafeRunSync()

    // Then: Modal should be open
    val modalState = stateManager.getCurrentState.unsafeRunSync()
    modalState.modal shouldBe Some(Modal.GotoLine(""))
    modalState.focus shouldBe Focus.Modal(ModalType.GotoLine)

    // When: Type line number
    "250".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    // Then: Should jump to line 250
    val afterGotoState = stateManager.getCurrentState.unsafeRunSync()
    afterGotoState.modal shouldBe None
    val pane = afterGotoState.layout.editorPanes(paneId)
    pane.cursors.head.line shouldBe 249           // 0-indexed, so line 250 = index 249
    pane.viewport.topLine should be >= (249 - 12) // Center line in viewport
    pane.viewport.topLine should be <= 249

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
    val updatedPane = state.layout
      .editorPanes(paneId)
      .copy(
        bufferId = Some(bufferId),
        cursors = List(CursorPosition(0, 0)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80)
      )
    val updatedLayout = state.layout.copy(
      editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
    )
    stateManager.getCurrentState
      .flatMap(currentState => IO.pure(currentState.copy(layout = updatedLayout)))
      .unsafeRunSync()

    // When: Open find dialog (Ctrl+F)
    stateManager.applyEvent(OpenFind).unsafeRunSync()

    // Type search term
    "SPECIAL_MARKER".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    // Then: Should scroll to first occurrence (line 50)
    val afterFindState = stateManager.getCurrentState.unsafeRunSync()
    val pane1          = afterFindState.layout.editorPanes(paneId)
    pane1.cursors.head.line shouldBe 49           // Line 50 (0-indexed)
    pane1.viewport.topLine should be >= (49 - 12) // Should be visible
    pane1.viewport.topLine should be <= 49

    // When: Find next (F3)
    stateManager.applyEvent(FindNext).unsafeRunSync()

    // Then: Should scroll to next occurrence (line 100)
    val afterNextState = stateManager.getCurrentState.unsafeRunSync()
    val pane2          = afterNextState.layout.editorPanes(paneId)
    pane2.cursors.head.line shouldBe 99 // Line 100 (0-indexed)

    // When: Find next again
    stateManager.applyEvent(FindNext).unsafeRunSync()

    // Then: Should scroll to line 150
    val afterNext2State = stateManager.getCurrentState.unsafeRunSync()
    val pane3           = afterNext2State.layout.editorPanes(paneId)
    pane3.cursors.head.line shouldBe 149 // Line 150 (0-indexed)

  it should "handle viewport synchronization across split panes" in new ScrollFixture:
    // Given: Same file in multiple panes
    val content  = (1 to 100).map(i => s"Line $i").mkString("\n")
    val bufferId = stateManager.createBuffer(content).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    val pane1 = state.layout.editorPanes.keys.head

    // Create split pane with same buffer
    val pane2 = stateManager.splitPaneHorizontal(pane1, Some(bufferId)).unsafeRunSync()

    val afterSplitState = stateManager.getCurrentState.unsafeRunSync()
    val updatedPane1 = afterSplitState.layout
      .editorPanes(pane1)
      .copy(
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80)
      )
    val updatedPane2 = afterSplitState.layout
      .editorPanes(pane2)
      .copy(
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80),
        syncedScrolling = true
      )
    val updatedLayout = afterSplitState.layout.copy(
      editorPanes = afterSplitState.layout.editorPanes + (pane1 -> updatedPane1) + (pane2 -> updatedPane2)
    )
    stateManager.getCurrentState
      .flatMap(currentState => IO.pure(currentState.copy(layout = updatedLayout)))
      .unsafeRunSync()

    // When: Scroll in first pane
    stateManager.switchToPane(pane1).unsafeRunSync()
    stateManager.applyEvent(ScrollDown(10)).unsafeRunSync()

    // Then: Both panes should scroll if synchronized
    val afterScrollState = stateManager.getCurrentState.unsafeRunSync()
    val finalPane1       = afterScrollState.layout.editorPanes(pane1)
    val finalPane2       = afterScrollState.layout.editorPanes(pane2)

    finalPane1.viewport.topLine shouldBe 10
    if finalPane2.syncedScrolling then finalPane2.viewport.topLine shouldBe 10

  it should "handle minimap scrolling and navigation" in new ScrollFixture:
    // Given: Large file with minimap enabled
    val content  = (1 to 1000).map(i => s"Line $i").mkString("\n")
    val bufferId = stateManager.createBuffer(content).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    val updatedPane = state.layout
      .editorPanes(paneId)
      .copy(
        bufferId = Some(bufferId),
        cursors = List(CursorPosition(0, 0)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80),
        minimapVisible = true
      )
    val updatedLayout = state.layout.copy(
      editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
    )
    stateManager.getCurrentState
      .flatMap(currentState => IO.pure(currentState.copy(layout = updatedLayout)))
      .unsafeRunSync()

    // When: Click on minimap (simulate click at 50% down)
    val targetLine = 500 // Middle of file
    stateManager.clickMinimap(paneId, targetLine).unsafeRunSync()

    // Then: Should scroll to clicked location
    val afterClickState = stateManager.getCurrentState.unsafeRunSync()
    val pane            = afterClickState.layout.editorPanes(paneId)
    pane.viewport.topLine should be >= (targetLine - 12)
    pane.viewport.topLine should be <= (targetLine + 12)

  it should "handle edge cases with scrolling bounds" in new ScrollFixture:
    // Given: Small file
    val content  = "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"
    val bufferId = stateManager.createBuffer(content).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val paneId = state.layout.editorPanes.keys.head
    val updatedPane = state.layout
      .editorPanes(paneId)
      .copy(
        bufferId = Some(bufferId),
        cursors = List(CursorPosition(0, 0)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 25, visibleColumns = 80)
      )
    val updatedLayout = state.layout.copy(
      editorPanes = state.layout.editorPanes + (paneId -> updatedPane)
    )
    stateManager.getCurrentState
      .flatMap(currentState => IO.pure(currentState.copy(layout = updatedLayout)))
      .unsafeRunSync()

    // When: Try to scroll beyond file bounds
    stateManager.applyEvent(ScrollDown(100)).unsafeRunSync() // Way more than file has

    // Then: Should clamp to file bounds
    val afterScrollState = stateManager.getCurrentState.unsafeRunSync()
    val pane             = afterScrollState.layout.editorPanes(paneId)
    pane.viewport.topLine shouldBe 0 // Can't scroll down in small file

    // When: Try to scroll up beyond beginning
    stateManager.applyEvent(ScrollUp(100)).unsafeRunSync()

    // Then: Should stay at beginning
    val afterScrollUpState = stateManager.getCurrentState.unsafeRunSync()
    val pane2              = afterScrollUpState.layout.editorPanes(paneId)
    pane2.viewport.topLine shouldBe 0

  trait ScrollFixture:

    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager: StateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()
