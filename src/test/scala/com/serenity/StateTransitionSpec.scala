package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.traverse.*
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.{DirEntry, PanelPosition, PeekContent}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateTransitionSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "State Machine Transitions"

  it should "transition focus between editor panes correctly" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      
      // Given: Multiple panes
      buffer1 <- stateManager.createBuffer("First buffer")
      buffer2 <- stateManager.createBuffer("Second buffer")
      pane2 <- stateManager.createPane(Some(buffer2))

      // When: Switch focus between panes
      _ <- stateManager.switchToPane(pane2)

      // Then: Focus should be on second pane
      state1 <- stateManager.getCurrentState
      _ = state1.focus shouldBe Focus.EditorPane(pane2)
      _ = state1.layout.activeEditorPaneId shouldBe Some(pane2)

      // When: Switch back to first pane
      firstPaneId = state1.layout.editorPanes.keys.find(_ != pane2).get
      _ <- stateManager.switchToPane(firstPaneId)

      // Then: Focus should return to first pane
      state2 <- stateManager.getCurrentState
    yield
      state2.focus shouldBe Focus.EditorPane(firstPaneId)
      state2.layout.activeEditorPaneId shouldBe Some(firstPaneId)

    program.unsafeRunSync()
  }

  it should "handle modal state transitions" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      
      // Given: Initial state with active pane
      bufferId <- stateManager.createBuffer("Some content")
      initialState <- stateManager.getCurrentState
      paneId = initialState.layout.editorPanes.keys.head

      // When: Show modal
      modal = Modal.CommandRunner("test", List.empty, 0)
      _ <- stateManager.showModal(modal)

      // Then: Focus should be on modal
      modalState <- stateManager.getCurrentState
      _ = modalState.focus shouldBe Focus.Modal(ModalType.CommandPalette)
      _ = modalState.modal shouldBe Some(modal)

      // When: Dismiss modal
      _ <- stateManager.dismissModal()

      // Then: Focus should return to pane
      finalState <- stateManager.getCurrentState
    yield
      finalState.focus shouldBe Focus.EditorPane(paneId)
      finalState.modal shouldBe None

    program.unsafeRunSync()
  }

  it should "handle peek overlay state transitions" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    
    val program = for
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      
      // Given: Active pane with cursor position
      bufferId <- stateManager.createBuffer("Content")
      cursor = CursorPosition(0, 3)
      content = PeekContent.DirectoryListing(
        java.nio.file.Paths.get("/test"),
        List(
          DirEntry(java.nio.file.Paths.get("/test/file1.txt"), "file1.txt", false),
          DirEntry(java.nio.file.Paths.get("/test/file2.txt"), "file2.txt", false)
        )
      )

      // When: Show peek overlay
      _ <- stateManager.showPeek(content, cursor)

      // Then: Focus should be on peek overlay
      peekState <- stateManager.getCurrentState
      _ = peekState.focus shouldBe Focus.PeekOverlay
      _ = peekState.peekOverlay shouldBe defined
      _ = peekState.peekOverlay.get.position shouldBe cursor

      // When: Dismiss peek overlay
      _ <- stateManager.dismissPeek()

      // Then: Focus should return to editor pane
      finalState <- stateManager.getCurrentState
    yield
      finalState.focus match
        case Focus.EditorPane(_) => finalState.peekOverlay shouldBe None
        case other               => fail(s"Expected EditorPane focus, got $other")

    program.unsafeRunSync()
  }

  it should "maintain state consistency during rapid focus transitions" in new StateFixture:
    // Given: Multiple UI components
    val buffer1 = stateManager.createBuffer("Buffer 1").unsafeRunSync()
    val buffer2 = stateManager.createBuffer("Buffer 2").unsafeRunSync()
    val pane1   = stateManager.getCurrentState.unsafeRunSync().layout.editorPanes.keys.head
    val pane2   = stateManager.createPane(Some(buffer2)).unsafeRunSync()

    val modal = Modal.FileSearch("*.scala", List.empty, 0)
    val peekContent = PeekContent.DirectoryListing(
      java.nio.file.Paths.get("/src"),
      List(
        DirEntry(java.nio.file.Paths.get("/src/main"), "main", true),
        DirEntry(java.nio.file.Paths.get("/src/test"), "test", true)
      )
    )

    // When: Rapid state transitions
    stateManager.switchToPane(pane2).unsafeRunSync()
    stateManager.showModal(modal).unsafeRunSync()
    stateManager.dismissModal().unsafeRunSync()
    stateManager.showPeek(peekContent, CursorPosition(0, 0)).unsafeRunSync()
    stateManager.dismissPeek().unsafeRunSync()
    stateManager.switchToPane(pane1).unsafeRunSync()

    // Then: Final state should be valid
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.isValid shouldBe true
    finalState.focus shouldBe Focus.EditorPane(pane1)
    finalState.modal shouldBe None
    finalState.peekOverlay shouldBe None

  it should "handle buffer lifecycle correctly" in new StateFixture:
    // Given: Create buffer and associate with pane
    val bufferId     = stateManager.createBuffer("Initial content").unsafeRunSync()
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    val paneId       = initialState.layout.editorPanes.keys.head

    // Associate buffer with pane
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Verify buffer exists and is associated
    val stateWithBuffer = stateManager.getCurrentState.unsafeRunSync()
    stateWithBuffer.buffers should contain key bufferId

    // When: Close buffer
    stateManager.closeBuffer(bufferId).unsafeRunSync()

    // Then: Buffer should be removed and pane should have no buffer
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.buffers should not contain key(bufferId)
    finalState.layout.editorPanes(paneId).bufferId shouldBe None
    finalState.isValid shouldBe true

  it should "handle pane lifecycle correctly" in new StateFixture:
    // Given: Multiple panes
    val buffer1 = stateManager.createBuffer("Buffer 1").unsafeRunSync()
    val buffer2 = stateManager.createBuffer("Buffer 2").unsafeRunSync()
    val pane1   = stateManager.getCurrentState.unsafeRunSync().layout.editorPanes.keys.head
    val pane2   = stateManager.createPane(Some(buffer2)).unsafeRunSync()

    val stateWithTwoPanes = stateManager.getCurrentState.unsafeRunSync()
    stateWithTwoPanes.layout.editorPanes should have size 2

    // When: Close one pane
    stateManager.closePane(pane2).unsafeRunSync()

    // Then: Pane should be removed and focus should adjust
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.layout.editorPanes should have size 1
    finalState.layout.editorPanes should contain key pane1
    finalState.layout.editorPanes should not contain key(pane2)
    finalState.isValid shouldBe true

    // Focus should be on remaining pane
    finalState.focus match
      case Focus.EditorPane(id) if id == pane1 => succeed
      case other                               => fail(s"Expected focus on pane1 ($pane1), got $other")

  it should "handle complex state validation scenarios" in new StateFixture:
    // Given: Create a complex state
    val buffer1 = stateManager.createBuffer("Buffer 1").unsafeRunSync()
    val buffer2 = stateManager.createBuffer("Buffer 2").unsafeRunSync()
    val pane1   = stateManager.getCurrentState.unsafeRunSync().layout.editorPanes.keys.head
    val pane2   = stateManager.createPane(Some(buffer2)).unsafeRunSync()

    // Verify valid state
    val complexState = stateManager.getCurrentState.unsafeRunSync()
    complexState.isValid shouldBe true
    complexState.validationErrors shouldBe empty

    // When: Test that state remains valid during operations
    stateManager.switchToPane(pane2).unsafeRunSync()
    val afterSwitchState = stateManager.getCurrentState.unsafeRunSync()
    afterSwitchState.isValid shouldBe true

    stateManager.createBuffer("Buffer 3").unsafeRunSync()
    val afterCreateState = stateManager.getCurrentState.unsafeRunSync()
    afterCreateState.isValid shouldBe true


  it should "handle state recovery from invalid transitions gracefully" in new StateFixture:
    // Given: Valid initial state
    val bufferId   = stateManager.createBuffer("Content").unsafeRunSync()
    val validState = stateManager.getCurrentState.unsafeRunSync()
    validState.isValid shouldBe true

    // When: Attempt invalid operations (these should be handled gracefully)
    // Switch to non-existent pane (should be ignored)
    stateManager.switchToPane(PaneId(999)).unsafeRunSync()
    val afterInvalidSwitch = stateManager.getCurrentState.unsafeRunSync()
    afterInvalidSwitch.isValid shouldBe true

    // Close non-existent buffer (should be ignored)
    stateManager.closeBuffer(BufferId(999)).unsafeRunSync()
    val afterInvalidClose = stateManager.getCurrentState.unsafeRunSync()
    afterInvalidClose.isValid shouldBe true

  trait StateFixture:

    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager: StateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()
