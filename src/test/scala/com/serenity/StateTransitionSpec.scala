package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.{DirEntry, PeekContent, SplitAxis, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
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
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Multiple panes
      buffer1 <- stateManager.createBuffer("First buffer")
      buffer2 <- stateManager.createBuffer("Second buffer")
      pane2   <- stateManager.createPane(Some(buffer2))

      // When: Switch focus between panes
      _ <- stateManager.switchToPane(pane2)

      // Then: Focus should be on second pane
      state1 <- stateManager.getCurrentState
      _ = state1.persisted.focus shouldBe Focus.EditorPane(pane2)
      _ = state1.persisted.layout.activeEditorPaneId shouldBe Some(pane2)

      // When: Switch back to first pane
      firstPaneId = state1.persisted.layout.editorPanes.keys.find(_ != pane2).get
      _ <- stateManager.switchToPane(firstPaneId)

      // Then: Focus should return to first pane
      state2 <- stateManager.getCurrentState
    yield
      state2.persisted.focus shouldBe Focus.EditorPane(firstPaneId)
      state2.persisted.layout.activeEditorPaneId shouldBe Some(firstPaneId)

    program.unsafeRunSync()
  }

  it should "handle modal state transitions" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      // Given: Initial state with active pane
      bufferId     <- stateManager.createBuffer("Some content")
      initialState <- stateManager.getCurrentState
      paneId = initialState.persisted.layout.editorPanes.keys.head

      // When: Show modal
      modal = Modal.Custom("test-modal", "test")
      _ <- stateManager.showModal(modal)

      // Then: Focus should be on the modal surface
      modalState <- stateManager.getCurrentState
      modalSurface = modalState.runtime.uiSurfaces.find(_.content == SurfaceContent.ModalWorkflow(modal))
      _            = modalSurface shouldBe defined
      _            = modalState.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)

      // When: Dismiss modal
      _ <- stateManager.dismissModal()

      // Then: Focus should return to pane
      finalState <- stateManager.getCurrentState
    yield
      finalState.persisted.focus shouldBe Focus.EditorPane(paneId)
      finalState.runtime.uiSurfaces.exists(_.content == SurfaceContent.ModalWorkflow(modal)) shouldBe false

    program.unsafeRunSync()
  }

  it should "handle peek overlay state transitions" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
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

      // Then: Focus should be on the peek surface
      peekState <- stateManager.getCurrentState
      peekSurface = peekState.runtime.uiSurfaces.find(
        _.content == SurfaceContent.DirectoryListing(
          java.nio.file.Paths.get("/test"),
          List(
            DirEntry(java.nio.file.Paths.get("/test/file1.txt"), "file1.txt", false),
            DirEntry(java.nio.file.Paths.get("/test/file2.txt"), "file2.txt", false)
          ),
          None
        )
      )
      _ = peekSurface shouldBe defined
      _ = peekState.persisted.focus shouldBe Focus.Surface(peekSurface.get.id)
      _ = peekSurface.get.presentation shouldBe SurfacePresentation.Floating(Some(cursor), SurfacePlacement.AboveCursor)

      // When: Dismiss peek overlay
      _ <- stateManager.dismissPeek()

      // Then: Focus should return to editor pane
      finalState <- stateManager.getCurrentState
    yield finalState.persisted.focus match
      case Focus.EditorPane(_) =>
        finalState.runtime.uiSurfaces.exists {
          _.content == SurfaceContent.DirectoryListing(
            java.nio.file.Paths.get("/test"),
            List(
              DirEntry(java.nio.file.Paths.get("/test/file1.txt"), "file1.txt", false),
              DirEntry(java.nio.file.Paths.get("/test/file2.txt"), "file2.txt", false)
            ),
            None
          )
        } shouldBe false
      case other => fail(s"Expected EditorPane focus, got $other")

    program.unsafeRunSync()
  }

  it should "maintain state consistency during rapid focus transitions" in new StateFixture:
    // Given: Multiple UI components
    stateManager.createBuffer("Buffer 1").unsafeRunSync()
    val buffer2 = stateManager.createBuffer("Buffer 2").unsafeRunSync()
    val pane1   = stateManager.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keys.head
    val pane2   = stateManager.createPane(Some(buffer2)).unsafeRunSync()

    val modal = Modal.Custom("search-panel", "*.scala")
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
    finalState.persisted.focus shouldBe Focus.EditorPane(pane1)
    finalState.runtime.uiSurfaces.exists(_.content == SurfaceContent.ModalWorkflow(modal)) shouldBe false
    finalState.runtime.uiSurfaces.exists {
      _.content == SurfaceContent.DirectoryListing(
        java.nio.file.Paths.get("/src"),
        List(
          DirEntry(java.nio.file.Paths.get("/src/main"), "main", true),
          DirEntry(java.nio.file.Paths.get("/src/test"), "test", true)
        ),
        None
      )
    } shouldBe false

  it should "handle buffer lifecycle correctly" in new StateFixture:
    // Given: Create buffer and associate with pane
    val bufferId     = stateManager.createBuffer("Initial content").unsafeRunSync()
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    val paneId       = initialState.persisted.layout.editorPanes.keys.head

    // Associate buffer with pane
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()

    // Verify buffer exists and is associated
    val stateWithBuffer = stateManager.getCurrentState.unsafeRunSync()
    stateWithBuffer.persisted.buffers should contain key bufferId

    // When: Close buffer
    stateManager.closeBuffer(bufferId).unsafeRunSync()

    // Then: Buffer should be removed and pane should have no buffer
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.buffers should not contain key(bufferId)
    finalState.persisted.layout.editorPanes(paneId).bufferId shouldBe None
    finalState.isValid shouldBe true

  it should "handle pane lifecycle correctly" in new StateFixture:
    // Given: Multiple panes
    stateManager.createBuffer("Buffer 1").unsafeRunSync()
    val buffer2 = stateManager.createBuffer("Buffer 2").unsafeRunSync()
    val pane1   = stateManager.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keys.head
    val pane2   = stateManager.createPane(Some(buffer2)).unsafeRunSync()

    val stateWithTwoPanes = stateManager.getCurrentState.unsafeRunSync()
    stateWithTwoPanes.persisted.layout.editorPanes should have size 2

    // When: Close one pane
    stateManager.closePane(pane2).unsafeRunSync()

    // Then: Pane should be removed and focus should adjust
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.layout.editorPanes should have size 1
    finalState.persisted.layout.editorPanes should contain key pane1
    finalState.persisted.layout.editorPanes should not contain key(pane2)
    finalState.isValid shouldBe true

    // Focus should be on remaining pane
    finalState.persisted.focus match
      case Focus.EditorPane(id) if id == pane1 => succeed
      case other                               => fail(s"Expected focus on pane1 ($pane1), got $other")

  it should "handle complex state validation scenarios" in new StateFixture:
    // Given: Create a complex state
    stateManager.createBuffer("Buffer 1").unsafeRunSync()
    val buffer2 = stateManager.createBuffer("Buffer 2").unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keys.head
    val pane2 = stateManager.createPane(Some(buffer2)).unsafeRunSync()

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
    stateManager.createBuffer("Content").unsafeRunSync()
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

  it should "reject workspace trees with duplicate or missing pane leaves" in:
    val base = AppState.initial
    val invalidTree = WorkspaceTree(
      WorkspaceNode.Split(
        WorkspaceNodeId("duplicate"),
        SplitAxis.Horizontal,
        0.5,
        WorkspaceNode.Leaf(WorkspaceNodeId("duplicate"), PaneId(0)),
        WorkspaceNode.Leaf(WorkspaceNodeId("other"), PaneId(0))
      )
    )
    val invalid =
      base.copy(persisted = base.persisted.copy(layout = base.persisted.layout.copy(workspaceTree = Some(invalidTree))))

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Workspace tree contains duplicate node IDs: duplicate")
    invalid.validationErrors should contain("Workspace tree contains duplicate pane leaves: 0")

  it should "reject editor focus outside the workspace tree" in:
    val base        = AppState.initial
    val outsideId   = PaneId(99)
    val outsidePane = EditorPane.empty(outsideId)
    val invalid = base.copy(
      persisted = base.persisted.copy(
        layout = base.persisted.layout.copy(
          editorPanes = base.persisted.layout.editorPanes.updated(outsideId, outsidePane),
          workspaceTree = base.persisted.layout.effectiveWorkspaceTree
        ),
        focus = Focus.EditorPane(outsideId)
      )
    )

    invalid.isValid shouldBe false
    invalid.validationErrors should contain("Workspace tree is missing editor panes: 99")
    invalid.validationErrors should contain(s"Focus points outside workspace tree: $outsideId")

  trait StateFixture:

    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))

    val stateManager: StateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()
