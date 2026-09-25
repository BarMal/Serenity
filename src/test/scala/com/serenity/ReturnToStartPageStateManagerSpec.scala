package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{CommandIntent, CommandRegistry, SessionIntent}
import com.serenity.keystroke.events.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** #6: a command-runner option to return to the start page from an editor. The Save / Close Anyway / Cancel dialog
  * appears for unsaved buffers, but -- unlike Quit/Close -- no buffer is dropped: the whole session (including any
  * still-unsaved buffers) is snapshotted so [Tab] Quick-resume restores the editor exactly as it was left.
  */
class ReturnToStartPageStateManagerSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger  = LoggerFactory[IO].getLogger(using LoggerName("ReturnToStartPageStateManagerSpec"))
    val tempDir = Files.createTempDirectory("serenity-return-to-start-page")
    StateManager.apply(logger, sessionRootOverride = Some(tempDir)).unsafeRunSync()

  private def returnCommand =
    CommandRegistry.withToggleUI
      .findCommand("return-to-start-page")
      .getOrElse(fail("expected a return-to-start-page command to be registered"))

  private def startPageOf(state: AppState): StartupPage =
    state.startPageSurface
      .flatMap {
        _.content match
          case SurfaceContent.StartPage(page) => Some(page)
          case _                              => None
      }
      .getOrElse(fail("expected a start-page surface"))

  private def currentCloseWorkflow(stateManager: StateManager): CloseWorkflowState =
    stateManager.getCurrentState
      .unsafeRunSync()
      .topModal
      .flatMap {
        _.modal match
          case Modal.CloseWorkflow(workflow) => Some(workflow)
          case _                             => None
      }
      .getOrElse(fail("expected an active close-workflow modal"))

  private def markBufferDirty(stateManager: StateManager, bufferId: BufferId, content: String): Unit =
    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document =
            state.persisted
              .buffers(bufferId)
              .document
              .copy(content = Rope(content), filePath = None, isDirty = true, isNewEmpty = false)
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

  "return-to-start-page command" should "be registered with a return-to-start-page session intent" in {
    returnCommand.intent shouldBe CommandIntent.Session(SessionIntent.ReturnToStartPage)
  }

  it should "show the start page immediately when no buffer has unsaved changes" in {
    val stateManager = createStateManager()

    stateManager.commandExecutor.executeCommand(returnCommand).unsafeRunSync()

    val updated = stateManager.getCurrentState.unsafeRunSync()
    updated.topModal shouldBe None
    updated.startPageSurface.isDefined shouldBe true
    startPageOf(updated).resume.isDefined shouldBe true
  }

  it should "prompt with the close dialog scoped to the return-to-start-page flow for a dirty buffer" in {
    val stateManager = createStateManager()
    markBufferDirty(stateManager, BufferId(0), "draft")

    stateManager.commandExecutor.executeCommand(returnCommand).unsafeRunSync()

    currentCloseWorkflow(stateManager).scope shouldBe CloseScope.ReturnToStartPage
  }

  it should "keep the editor when the dialog is cancelled" in {
    val stateManager = createStateManager()
    markBufferDirty(stateManager, BufferId(0), "draft")

    stateManager.commandExecutor.executeCommand(returnCommand).unsafeRunSync()
    // Save -> Close Anyway -> Cancel, then submit.
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updated = stateManager.getCurrentState.unsafeRunSync()
    updated.topModal shouldBe None
    updated.startPageSurface shouldBe None
    updated.persisted.buffers.get(BufferId(0)).exists(_.document.isDirty) shouldBe true
  }

  it should "snapshot the unsaved buffer on Close Anyway so Tab-resume restores it" in {
    val stateManager = createStateManager()
    markBufferDirty(stateManager, BufferId(0), "draft")

    stateManager.commandExecutor.executeCommand(returnCommand).unsafeRunSync()
    // Save -> Close Anyway, then submit the Close Anyway choice.
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val afterDiscard = stateManager.getCurrentState.unsafeRunSync()
    afterDiscard.startPageSurface.isDefined shouldBe true

    val resumeCommand =
      startPageOf(afterDiscard).resume.map(_.command).getOrElse(fail("expected a resume hint"))
    stateManager.commandExecutor.executeCommand(resumeCommand).unsafeRunSync()

    val restored = stateManager.getCurrentState.unsafeRunSync()
    restored.startPageSurface shouldBe None
    restored.persisted.buffers.values.exists(_.document.content.collect().contains("draft")) shouldBe true
  }
