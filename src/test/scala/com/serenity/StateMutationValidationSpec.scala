package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.Enter
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
      stateManager.openFile(tempFile).unsafeRunSync()
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
        stateManager
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
