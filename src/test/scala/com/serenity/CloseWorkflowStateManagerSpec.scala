package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{CloseTab, Enter, Quit, TabKey, ToggleCommandRunner}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{BufferId, CloseScope, Focus, Modal, SurfaceContent}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CloseWorkflowStateManagerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("CloseWorkflowStateManagerSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def executeCommandThroughRunner(
    stateManager: StateManager,
    searchTerm: String,
    expectedCommandName: String
  ): Unit =
    val beforeOpen = stateManager.getCurrentState.unsafeRunSync()
    if beforeOpen.commandRunnerSurface.flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner.isActive)
          case _                                     => None
      }.getOrElse(false) == false
    then
      stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    searchTerm.foreach(char => stateManager.applyEvent(com.serenity.keystroke.events.InsertChar(char)).unsafeRunSync())

    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => runner.selectedCommand.map(_.name)
        case _                                     => None
    } shouldBe Some(expectedCommandName)

    stateManager.applyEvent(Enter).unsafeRunSync()

  private def currentCloseWorkflow(stateManager: StateManager) =
    stateManager.getCurrentState.unsafeRunSync().modalSurface.flatMap {
      _.content match
        case SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)) => Some(workflow)
        case _                                                           => None
    }.getOrElse(fail("Expected active close workflow modal"))

  "Close workflow" should "discard and close the current dirty buffer" in {
    val stateManager = createStateManager()
    val bufferId = BufferId(0)

    stateManager.updateState { state =>
      val buffer = state.buffers(bufferId).copy(isDirty = true)
      state.copy(buffers = state.buffers + (bufferId -> buffer))
    }.unsafeRunSync()

    executeCommandThroughRunner(stateManager, "close", "close")
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface shouldBe None
    updatedState.buffers should not contain key(bufferId)
  }

  it should "cancel the close workflow without closing the dirty buffer" in {
    val stateManager = createStateManager()
    val bufferId = BufferId(0)

    stateManager.updateState { state =>
      val buffer = state.buffers(bufferId).copy(isDirty = true)
      state.copy(buffers = state.buffers + (bufferId -> buffer))
    }.unsafeRunSync()

    executeCommandThroughRunner(stateManager, "close", "close")
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface shouldBe None
    updatedState.buffers should contain key(bufferId)
    updatedState.buffers(bufferId).isDirty shouldBe true
    updatedState.focus shouldBe Focus.EditorPane(updatedState.layout.activeEditorPaneId.get)
  }

  it should "save and close a dirty path-backed buffer" in {
    val tempRoot   = Files.createTempDirectory("close-workflow-save")
    val targetFile = tempRoot.resolve("notes.scala")
    val bufferId   = BufferId(0)

    try
      val stateManager = createStateManager()
      stateManager.updateState { state =>
        val buffer = state.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("object Notes"),
          filePath = Some(targetFile),
          isDirty = true
        )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }.unsafeRunSync()

      executeCommandThroughRunner(stateManager, "close", "close")
      stateManager.applyEvent(Enter).unsafeRunSync()

      val updatedState = stateManager.getCurrentState.unsafeRunSync()
      updatedState.modalSurface shouldBe None
      updatedState.buffers should not contain key(bufferId)
      Files.readString(targetFile) shouldBe "object Notes"
    finally
      Files.deleteIfExists(targetFile)
      Files.deleteIfExists(tempRoot)
  }

  it should "route save for an unsaved buffer into the save-as workflow and resume closure after submit" in {
    val tempRoot   = Files.createTempDirectory("close-workflow-save-as")
    val targetDir  = tempRoot.resolve("nested")
    val targetFile = targetDir.resolve("notes.scala")
    val bufferId   = BufferId(0)

    try
      val stateManager = createStateManager()
      stateManager.updateState { state =>
        val buffer = state.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("object Notes"),
          isDirty = true
        )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }.unsafeRunSync()

      executeCommandThroughRunner(stateManager, "close", "close")
      stateManager.applyEvent(Enter).unsafeRunSync()

      stateManager.getCurrentState.unsafeRunSync().modalSurface.flatMap(_.content match
        case SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)) => Some(workflow.mode)
        case _                                                          => None
      ) shouldBe Some(com.serenity.state.models.FileWorkflowMode.SaveAs)

      stateManager.updateState { state =>
        state.modalSurface match
          case Some(surface) =>
            state.copy(
              uiSurfaces = state.uiSurfaces.filterNot(_.id == surface.id) :+ surface.copy(
                content = SurfaceContent.ModalWorkflow(
                  Modal.FileWorkflow(
                    com.serenity.state.models.FileWorkflowState(
                      mode = com.serenity.state.models.FileWorkflowMode.SaveAs,
                      filename = "notes.scala",
                      path = targetDir.toString
                    )
                  )
                )
              )
            )
          case None => state
      }.unsafeRunSync()

      stateManager.applyEvent(Enter).unsafeRunSync()
      stateManager.applyEvent(Enter).unsafeRunSync()

      val updatedState = stateManager.getCurrentState.unsafeRunSync()
      updatedState.modalSurface shouldBe None
      updatedState.buffers should not contain key(bufferId)
      Files.readString(targetFile) shouldBe "object Notes"
    finally
      Files.deleteIfExists(targetFile)
      Files.deleteIfExists(targetDir)
      Files.deleteIfExists(tempRoot)
  }

  it should "open sequential unsaved-changes prompts for close-all" in {
    val stateManager = createStateManager()
    val secondBufferId = stateManager.createBuffer("second").unsafeRunSync()

    stateManager.updateState { state =>
      val first = state.buffers(BufferId(0)).copy(isDirty = true)
      val second = state.buffers(secondBufferId).copy(isDirty = true)
      state.copy(
        buffers = state.buffers + (BufferId(0) -> first) + (secondBufferId -> second),
        bufferOrder = state.bufferOrder :+ secondBufferId
      )
    }.unsafeRunSync()

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
    val bufferId = BufferId(0)

    stateManager.updateState { state =>
      val buffer = state.buffers(bufferId).copy(isDirty = true)
      state.copy(buffers = state.buffers + (bufferId -> buffer))
    }.unsafeRunSync()

    stateManager.applyEvent(CloseTab).unsafeRunSync()

    currentCloseWorkflow(stateManager).scope shouldBe CloseScope.Current
  }

  it should "open the unsaved-changes workflow from the quit hotkey when any buffer is dirty" in {
    val stateManager = createStateManager()
    val bufferId = BufferId(0)

    stateManager.updateState { state =>
      val buffer = state.buffers(bufferId).copy(isDirty = true)
      state.copy(buffers = state.buffers + (bufferId -> buffer))
    }.unsafeRunSync()

    stateManager.applyEvent(Quit).unsafeRunSync()

    currentCloseWorkflow(stateManager).scope shouldBe CloseScope.Quit
  }
end CloseWorkflowStateManagerSpec
