package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.{Enter, TabKey}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** #858 part 2: direct `Ref[IO, AppState]` mutation paths that bypass `validateAndUpdateState` can commit a
  * structurally invalid `AppState`. These tests drive a corrupted-but-realistic precondition (a `nextBufferId` counter
  * that has drifted out of sync with the live buffers -- the same class of drift #1181 found already happening by hand
  * in test fixtures) through the public file-loading API and assert the checked commit path rejects the resulting
  * invalid state instead of silently persisting it.
  */
class StateMutationValidationSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateMutationValidationSpec"))
    StateManager.apply(logger).unsafeRunSync()

  /** Forces `nextBufferId` to collide with the buffer that `AppState.initial` already ships (`BufferId(0)`), which is
    * also the focused buffer -- the precondition under which the bypassed commit produces a duplicate `bufferOrder`
    * entry and silently overwrites the live buffer.
    */
  private def corruptNextBufferIdToCollideWithLiveBuffer(stateManager: StateManager): Unit =
    stateManager
      .updateState(state => state.copy(runtime = state.runtime.copy(nextBufferId = BufferId(0))))
      .unsafeRunSync()

  "StateManager.openFile" should "not commit a duplicate buffer-order entry when nextBufferId has drifted" in {
    val stateManager = createStateManager()
    corruptNextBufferIdToCollideWithLiveBuffer(stateManager)

    val tempFile = Files.createTempFile("state-mutation-validation", ".txt")
    try
      Files.writeString(tempFile, "loaded from disk")

      val before = stateManager.getCurrentState.unsafeRunSync()
      stateManager.fileOpener.openFile(tempFile).unsafeRunSync()
      val after = stateManager.getCurrentState.unsafeRunSync()

      after.isValid shouldBe true
      after.persisted.bufferOrder should contain theSameElementsAs after.persisted.bufferOrder.distinct
      after.persisted.buffers(BufferId(0)).document.filePath shouldBe before.persisted
        .buffers(
          BufferId(0)
        )
        .document
        .filePath
    finally Files.deleteIfExists(tempFile)
  }

  "The open-file workflow modal" should
    "not commit a duplicate buffer-order entry when nextBufferId has drifted" in {
      val stateManager = createStateManager()

      val tempRoot   = Files.createTempDirectory("state-mutation-validation-workflow")
      val targetFile = tempRoot.resolve("notes.scala")
      Files.writeString(targetFile, "val answer = 42")

      try
        val before = stateManager.getCurrentState.unsafeRunSync()

        // Open the modal on an untouched, valid state -- the drift is introduced only after the modal is showing,
        // mirroring the many other unchecked `Ref.update` paths elsewhere in this codebase that could plausibly
        // desync `nextBufferId` between a validated commit and this workflow's own completion.
        stateManager.modalService
          .showModal(
            Modal.FileWorkflow(
              FileWorkflowState(mode = FileWorkflowMode.Open, filename = "notes.scala", path = tempRoot.toString)
            )
          )
          .unsafeRunSync()
        corruptNextBufferIdToCollideWithLiveBuffer(stateManager)
        stateManager.applyEvent(Enter).unsafeRunSync()

        val after = stateManager.getCurrentState.unsafeRunSync()

        after.isValid shouldBe true
        after.persisted.bufferOrder should contain theSameElementsAs after.persisted.bufferOrder.distinct
        after.persisted.buffers(BufferId(0)).document.filePath shouldBe before.persisted
          .buffers(
            BufferId(0)
          )
          .document
          .filePath
      finally
        Files.deleteIfExists(targetFile)
        Files.deleteIfExists(tempRoot)
    }

  /** #1183 item 1: the close-workflow family (`beginCloseAction`, `promptCloseWorkflow`, `submitCloseWorkflowEffect`,
    * `continueCloseWorkflow`) commits every step via direct `stateRef.set`, bypassing `validateAndUpdateState`
    * entirely. A close-all/close-others/quit sequence queues each dirty buffer's id in `CloseWorkflowState
    * .remainingBufferIds`, captured once when the sequence begins; if a queued buffer is no longer live by the time
    * `continueCloseWorkflow` resumes for it (some other unchecked mutation path removed it in the meantime -- exactly
    * the class of drift this suite already proves for `nextBufferId`), `promptCloseWorkflow`'s call to
    * `EditorState.rebalancePanes`/`assignBuffersToPanes` (see `EditorState.scala`'s `case None` branch, which assigns
    * `pane.copy(bufferId = Some(focusedBufferId))` without checking the buffer still exists) commits a pane pointing at
    * a nonexistent buffer -- with no `closeFocusedTab`/`removeBuffer` call afterward to clean it up, unlike the
    * discard/save branches that immediately close the buffer they just focused.
    */
  "A close-all workflow" should
    "not commit a pane referencing a buffer removed from under a still-queued close step" in {
      val stateManager   = createStateManager()
      val secondBufferId = stateManager.bufferManager.createBuffer("second", None).unsafeRunSync()
      val closeAllCommand = CommandRegistry.default
        .findCommand("close-all")
        .getOrElse(fail("\"close-all\" command not registered in CommandRegistry.default"))

      stateManager
        .updateState { state =>
          val dirtyBuffers =
            state.persisted.buffers.view.mapValues(b => b.copy(document = b.document.copy(isDirty = true))).toMap
          state.copy(persisted = state.persisted.copy(buffers = dirtyBuffers))
        }
        .unsafeRunSync()

      stateManager.commandExecutor.executeCommand(closeAllCommand).unsafeRunSync()

      val afterFirstPrompt = stateManager.getCurrentState.unsafeRunSync()
      afterFirstPrompt.modalSurface.flatMap {
        _.content match
          case SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)) => Some(workflow)
          case _                                                           => None
      } match
        case Some(workflow) =>
          workflow.currentBufferId shouldBe BufferId(0)
          workflow.remainingBufferIds shouldBe List(secondBufferId)
        case None => fail("Expected a close-workflow modal for the first dirty buffer")

      // Drift: the second buffer -- still queued in `remainingBufferIds` -- disappears from `persisted.buffers` out
      // from under the in-flight close-all sequence, simulating some other unchecked mutation path having removed it.
      stateManager
        .updateState { state =>
          state.copy(persisted =
            state.persisted.copy(
              buffers = state.persisted.buffers - secondBufferId,
              bufferOrder = state.persisted.bufferOrder.filterNot(_ == secondBufferId)
            )
          )
        }
        .unsafeRunSync()

      // Discard the (still-live) first buffer, which resumes the close-all sequence onto the now-stale second one.
      stateManager.applyEvent(TabKey).unsafeRunSync()
      stateManager.applyEvent(Enter).unsafeRunSync()

      val after = stateManager.getCurrentState.unsafeRunSync()
      after.isValid shouldBe true
      AppStateValidation.validationErrors(after) shouldBe empty
      after.persisted.layout.editorPanes.values.foreach { pane =>
        pane.bufferId.foreach(bufferId => after.persisted.buffers should contain key bufferId)
      }
    }
