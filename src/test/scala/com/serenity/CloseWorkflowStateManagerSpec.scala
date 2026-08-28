package com.serenity

import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.FileDialog
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CloseWorkflowStateManagerSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  final private case class TestFileDialog(saveSelection: Option[java.nio.file.Path]) extends FileDialog:
    override def chooseOpenFile(initialDirectory: Option[java.nio.file.Path]): IO[Option[java.nio.file.Path]] =
      IO.pure(None)

    override def chooseSaveFile(
      initialDirectory: Option[java.nio.file.Path],
      suggestedFileName: Option[String]
    ): IO[Option[java.nio.file.Path]] =
      IO.pure(saveSelection)

  private def createStateManager(fileDialog: Option[FileDialog] = None): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("CloseWorkflowStateManagerSpec"))
    StateManager.apply(logger, fileDialog = fileDialog).unsafeRunSync()

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

    searchTerm.foreach(char => stateManager.applyEvent(com.serenity.keystroke.events.InsertChar(char)).unsafeRunSync())

    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => runner.selectedCommand.map(_.name)
        case _                                     => None
    } shouldBe Some(expectedCommandName)

    stateManager.applyEvent(Enter).unsafeRunSync()

  private def currentCloseWorkflow(stateManager: StateManager) =
    stateManager.getCurrentState
      .unsafeRunSync()
      .modalSurface
      .flatMap {
        _.content match
          case SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)) => Some(workflow)
          case _                                                           => None
      }
      .getOrElse(fail("Expected active close workflow modal"))

  "Close workflow" should "discard and close the current dirty buffer" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document = state.persisted.buffers(bufferId).document.copy(isDirty = true))
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "close", "close")
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface shouldBe None
    updatedState.persisted.buffers should not contain key(bufferId)
  }

  it should "close an untouched empty buffer without prompting" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager.applyEvent(CloseTab).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface shouldBe None
    updatedState.persisted.buffers should not contain key(bufferId)
  }

  it should "warn before closing an untitled content buffer that has not been saved" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document =
            state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("draft"),
                filePath = None,
                isDirty = false,
                isNewEmpty = false
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(CloseTab).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.persisted.buffers should contain key bufferId
    currentCloseWorkflow(stateManager).currentBufferId shouldBe bufferId
  }

  it should "warn before closing an untitled buffer that was edited back to empty" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document =
            state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope.empty,
                filePath = None,
                isDirty = false,
                isNewEmpty = false
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(CloseTab).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.persisted.buffers should contain key bufferId
    currentCloseWorkflow(stateManager).currentBufferId shouldBe bufferId
  }

  it should "warn before closing the active editor buffer when a surface has focus" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document =
            state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope.empty,
                filePath = None,
                isDirty = false,
                isNewEmpty = false
              )
          )
        val (stateWithId, surfaceId) =
          state
            .copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
            .allocateSurfaceId
        val surface = UiSurface(
          id = surfaceId,
          content = SurfaceContent.QuickInfo("hint"),
          presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
        stateWithId.copy(
          persisted = stateWithId.persisted.copy(focus = Focus.Surface(surfaceId)),
          runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces :+ surface)
        )
      }
      .unsafeRunSync()

    stateManager.applyEvent(CloseTab).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.persisted.buffers should contain key bufferId
    currentCloseWorkflow(stateManager).currentBufferId shouldBe bufferId
  }

  it should "close an untitled content buffer when close anyway is selected" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document =
            state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("draft"),
                filePath = None,
                isDirty = false,
                isNewEmpty = false
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(CloseTab).unsafeRunSync()
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface shouldBe None
    updatedState.persisted.buffers should not contain key(bufferId)
  }

  it should "report untitled content buffers as unsaved changes" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document =
            state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("draft"),
                filePath = None,
                isDirty = false,
                isNewEmpty = false
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager.checkUnsavedChanges(Some(bufferId)).unsafeRunSync() shouldBe true
    stateManager.checkUnsavedChanges().unsafeRunSync() shouldBe true
  }

  it should "cancel the close workflow without closing the dirty buffer" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document = state.persisted.buffers(bufferId).document.copy(isDirty = true))
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "close", "close")
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface shouldBe None
    updatedState.persisted.buffers should contain key bufferId
    updatedState.persisted.buffers(bufferId).document.isDirty shouldBe true
    updatedState.persisted.focus shouldBe Focus.EditorPane(updatedState.persisted.layout.activeEditorPaneId.get)
  }

  it should "save and close a dirty path-backed buffer" in {
    val tempRoot   = Files.createTempDirectory("close-workflow-save")
    val targetFile = tempRoot.resolve("notes.scala")
    val bufferId   = BufferId(0)

    try
      val stateManager = createStateManager()
      stateManager
        .updateState { state =>
          val buffer = state.persisted
            .buffers(bufferId)
            .copy(document =
              state.persisted
                .buffers(bufferId)
                .document
                .copy(
                  content = com.serenity.rope.Rope("object Notes"),
                  filePath = Some(targetFile),
                  isDirty = true
                )
            )
          state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
        }
        .unsafeRunSync()

      executeCommandThroughRunner(stateManager, "close", "close")
      stateManager.applyEvent(Enter).unsafeRunSync()

      val updatedState = stateManager.getCurrentState.unsafeRunSync()
      updatedState.modalSurface shouldBe None
      updatedState.persisted.buffers should not contain key(bufferId)
      Files.readString(targetFile) shouldBe "object Notes"
    finally
      Files.deleteIfExists(targetFile)
      Files.deleteIfExists(tempRoot)
  }

  it should "route save for an unsaved buffer through the native save-as dialog and resume closure" in {
    val tempRoot   = Files.createTempDirectory("close-workflow-save-as")
    val targetDir  = tempRoot.resolve("nested")
    val targetFile = targetDir.resolve("notes.scala")
    val bufferId   = BufferId(0)

    try
      val stateManager = createStateManager(Some(TestFileDialog(Some(targetFile))))
      stateManager
        .updateState { state =>
          val buffer = state.persisted
            .buffers(bufferId)
            .copy(document =
              state.persisted
                .buffers(bufferId)
                .document
                .copy(
                  content = com.serenity.rope.Rope("object Notes"),
                  isDirty = true
                )
            )
          state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
        }
        .unsafeRunSync()

      executeCommandThroughRunner(stateManager, "close", "close")
      stateManager.applyEvent(Enter).unsafeRunSync()

      val updatedState = stateManager.getCurrentState.unsafeRunSync()
      updatedState.modalSurface shouldBe None
      updatedState.persisted.buffers should not contain key(bufferId)
      Files.readString(targetFile) shouldBe "object Notes"
    finally
      Files.deleteIfExists(targetFile)
      Files.deleteIfExists(targetDir)
      Files.deleteIfExists(tempRoot)
  }

  it should "keep the close workflow open when native save-as is cancelled" in {
    val stateManager = createStateManager(Some(TestFileDialog(None)))
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document =
            state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("object Notes"),
                isDirty = true
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "close", "close")
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    currentCloseWorkflow(stateManager).currentBufferId shouldBe bufferId
    updatedState.persisted.buffers should contain key bufferId
    updatedState.persisted.buffers(bufferId).document.isDirty shouldBe true
  }

  it should "open sequential unsaved-changes prompts for close-all" in {
    val stateManager   = createStateManager()
    val secondBufferId = stateManager.createBuffer("second").unsafeRunSync()

    stateManager
      .updateState { state =>
        val first =
          state.persisted
            .buffers(BufferId(0))
            .copy(document = state.persisted.buffers(BufferId(0)).document.copy(isDirty = true))
        val second =
          state.persisted
            .buffers(secondBufferId)
            .copy(document = state.persisted.buffers(secondBufferId).document.copy(isDirty = true))
        state.copy(persisted =
          state.persisted.copy(
            buffers = state.persisted.buffers + (BufferId(0) -> first) + (secondBufferId -> second)
          )
        )
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "close-all", "close-all")
    currentCloseWorkflow(stateManager).scope shouldBe CloseScope.All

    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val nextWorkflow = currentCloseWorkflow(stateManager)
    nextWorkflow.scope shouldBe CloseScope.All
    nextWorkflow.currentBufferId shouldBe secondBufferId
  }

  it should "open the unsaved-changes workflow from the close-tab hotkey" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document = state.persisted.buffers(bufferId).document.copy(isDirty = true))
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(CloseTab).unsafeRunSync()

    currentCloseWorkflow(stateManager).scope shouldBe CloseScope.Current
  }

  it should "open the unsaved-changes workflow from the quit hotkey when any buffer is dirty" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document = state.persisted.buffers(bufferId).document.copy(isDirty = true))
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(Quit).unsafeRunSync()

    currentCloseWorkflow(stateManager).scope shouldBe CloseScope.Quit
  }

  it should "discard the current dirty buffer and complete quit when close anyway is selected" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document = state.persisted.buffers(bufferId).document.copy(isDirty = true))
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    stateManager.applyEvent(Quit).unsafeRunSync()
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface shouldBe None
    updatedState.persisted.buffers should not contain key(bufferId)
    stateManager.awaitQuit.timeout(1.second).unsafeRunSync()
  }
end CloseWorkflowStateManagerSpec
