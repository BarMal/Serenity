package com.serenity

import java.nio.file.Path

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.FileDialog
import com.serenity.keystroke.events.*
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CommandRunnerFindReplaceCommandsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(
    sessionRootOverride: Option[Path] = None,
    configPersistencePath: Option[Path] = None,
    fileDialog: Option[FileDialog] = None
  ): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerFindReplaceCommandsSpec"))
    StateManager
      .apply(
        logger,
        sessionRootOverride = sessionRootOverride,
        configPersistencePath = configPersistencePath,
        fileDialog = fileDialog
      )
      .unsafeRunSync()

  private def executeCommandThroughRunner(
    stateManager: StateManager,
    searchTerm: String,
    expectedCommandName: String
  ): Unit =
    val beforeOpen = stateManager.getCurrentState.unsafeRunSync()
    if beforeOpen.commandRunnerSurface
          .flatMap {
            _.content match
              case SurfaceContent.CommandPalette(runner) => Some(runner.isActive)
              case _                                     => None
          }
          .getOrElse(false) == false
    then stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    searchTerm.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => runner.selectedCommand.map(_.name)
        case _                                     => None
    } shouldBe Some(expectedCommandName)

    stateManager.applyEvent(Enter).unsafeRunSync()

  "Command runner" should "open the goto-line modal for the goto-line command" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "goto-line", "goto-line")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = updatedState.modalSurface

    updatedState.commandRunnerSurface shouldBe None
    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("")))
    updatedState.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "open the find modal for the find command" in {
    val stateManager = createStateManager()
    val cursor       = CursorPosition(0, 0)

    executeCommandThroughRunner(stateManager, "find", "find")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = updatedState.modalSurface

    updatedState.commandRunnerSurface shouldBe None
    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.Find("", Nil, 0)))
    modalSurface.map(_.presentation) shouldBe Some(
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
    )
    updatedState.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "open the find modal from the command runner with the active buffer's existing query" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document =
              state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("alpha\nbeta\nalpha")),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(2, 0))),
            findState = Some(FindState("alpha", List(FindResult(0, 0), FindResult(2, 0)), 1))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "find", "find")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = updatedState.modalSurface

    updatedState.commandRunnerSurface shouldBe None
    modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(Modal.Find("alpha", List(FindResult(0, 0), FindResult(2, 0)), 1))
    )
    modalSurface.map(_.presentation) shouldBe Some(
      SurfacePresentation.Floating(Some(CursorPosition(2, 0)), SurfacePlacement.BelowCursor)
    )
    updatedState.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "open the find modal for the find-all command" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "find-all", "find-all")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = updatedState.modalSurface

    updatedState.commandRunnerSurface shouldBe None
    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.Find("", Nil, 0)))
    updatedState.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "open the replace modal for the replace command" in {
    val stateManager = createStateManager()
    val cursor       = CursorPosition(0, 0)

    executeCommandThroughRunner(stateManager, "replace", "replace")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = updatedState.modalSurface

    updatedState.commandRunnerSurface shouldBe None
    modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          com.serenity.state.models.ReplaceWorkflowState()
        )
      )
    )
    modalSurface.map(_.presentation) shouldBe Some(
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
    )
    updatedState.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "open the replace-all workflow with the bulk action selected" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "replace-all", "replace-all")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = updatedState.modalSurface

    updatedState.commandRunnerSurface shouldBe None
    modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          com.serenity.state.models.ReplaceWorkflowState(selectedAction = ReplaceWorkflowAction.ReplaceAll)
        )
      )
    )
    updatedState.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }
