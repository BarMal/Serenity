package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Direction, Enter, InsertChar, ModalNavigate, MoveLeft, TabKey, ToggleCommandRunner}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{
  BufferId,
  CursorPosition,
  Focus,
  Modal,
  ReplaceWorkflowScope,
  ReplaceWorkflowState,
  Selection,
  SurfaceContent
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class ReplaceWorkflowStateManagerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def createStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger = LoggerFactory[IO].getLogger(using LoggerName("ReplaceWorkflowStateManagerSpec"))
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

    searchTerm.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => runner.selectedCommand.map(_.name)
        case _                                     => None
    } shouldBe Some(expectedCommandName)

    stateManager.applyEvent(Enter).unsafeRunSync()

  "Replace workflow" should "replace all matches in the focused buffer and dismiss the modal" in {
    val stateManager = createStateManager()
    val bufferId = BufferId(0)

    stateManager.updateState { state =>
      val buffer = state.buffers(bufferId).copy(
        content = com.serenity.rope.Rope("needle one\nneedle two\nkeep")
      )
      state.copy(buffers = state.buffers + (bufferId -> buffer))
    }.unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace", "replace")

    stateManager.applyEvent(InsertChar('n')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('e')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('e')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('d')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('l')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('e')).unsafeRunSync()
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(InsertChar('t')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('h')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('r')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('e')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('a')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('d')).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface shouldBe None
    updatedState.focus shouldBe Focus.EditorPane(com.serenity.state.models.PaneId(0))
    updatedState.buffers(bufferId).content.collect() shouldBe "thread one\nthread two\nkeep"
    updatedState.buffers(bufferId).isDirty shouldBe true
  }

  it should "replace the next match and keep the modal open for repeated replacement" in {
    val stateManager = createStateManager()
    val bufferId = BufferId(0)

    stateManager.updateState { state =>
      val buffer = state.buffers(bufferId).copy(
        content = com.serenity.rope.Rope("needle one\nneedle two\nkeep")
      )
      state.copy(buffers = state.buffers + (bufferId -> buffer))
    }.unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace", "replace")

    "needle".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(TabKey).unsafeRunSync()
    "thread".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(MoveLeft).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val afterFirstReplace = stateManager.getCurrentState.unsafeRunSync()
    afterFirstReplace.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          ReplaceWorkflowState(
            findText = "needle",
            replacementText = "thread",
            activeField = com.serenity.state.models.ReplaceWorkflowField.ReplaceWith,
            selectedAction = com.serenity.state.models.ReplaceWorkflowAction.ReplaceNext,
            statusMessage = Some("Replaced next match")
          )
        )
      )
    )
    afterFirstReplace.buffers(bufferId).content.collect() shouldBe "thread one\nneedle two\nkeep"

    stateManager.applyEvent(Enter).unsafeRunSync()

    val afterSecondReplace = stateManager.getCurrentState.unsafeRunSync()
    afterSecondReplace.modalSurface shouldBe defined
    afterSecondReplace.buffers(bufferId).content.collect() shouldBe "thread one\nthread two\nkeep"
  }

  it should "keep the modal open with a status message when find text is empty" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "replace", "replace")
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          ReplaceWorkflowState(
            statusMessage = Some("Enter text to find")
          )
        )
      )
    )
  }

  it should "replace only inside the active selection when selection scope is chosen" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager.updateState { state =>
      val buffer = state.buffers(bufferId).copy(
        content = com.serenity.rope.Rope("needle one\nneedle two\nneedle three"),
        selection = Some(
          Selection(
            CursorPosition(1, 0),
            CursorPosition(1, "needle two".length)
          )
        )
      )
      state.copy(buffers = state.buffers + (bufferId -> buffer))
    }.unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace", "replace")

    "needle".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(TabKey).unsafeRunSync()
    "thread".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(MoveLeft).unsafeRunSync()
    stateManager.applyEvent(ModalNavigate(Direction.Down)).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface shouldBe defined
    updatedState.buffers(bufferId).content.collect() shouldBe "needle one\nthread two\nneedle three"

    updatedState.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          ReplaceWorkflowState(
            findText = "needle",
            replacementText = "thread",
            activeField = com.serenity.state.models.ReplaceWorkflowField.ReplaceWith,
            selectedAction = com.serenity.state.models.ReplaceWorkflowAction.ReplaceNext,
            selectedScope = ReplaceWorkflowScope.Selection,
            statusMessage = Some("Replaced next match")
          )
        )
      )
    )
  }

  it should "replace all matches only inside the active selection when selection scope is chosen" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager.updateState { state =>
      val buffer = state.buffers(bufferId).copy(
        content = com.serenity.rope.Rope("needle one\nneedle two\nneedle three"),
        selection = Some(
          Selection(
            CursorPosition(1, 0),
            CursorPosition(2, "needle three".length)
          )
        )
      )
      state.copy(buffers = state.buffers + (bufferId -> buffer))
    }.unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace", "replace")

    "needle".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(TabKey).unsafeRunSync()
    "thread".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(ModalNavigate(Direction.Down)).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface shouldBe None
    updatedState.buffers(bufferId).content.collect() shouldBe "needle one\nthread two\nthread three"
    updatedState.focus shouldBe Focus.EditorPane(com.serenity.state.models.PaneId(0))
  }

  it should "keep the replace modal open with a status message when selection scope is chosen without a selection" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "replace", "replace")

    "needle".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(TabKey).unsafeRunSync()
    "thread".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(MoveLeft).unsafeRunSync()
    stateManager.applyEvent(ModalNavigate(Direction.Down)).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          ReplaceWorkflowState(
            findText = "needle",
            replacementText = "thread",
            activeField = com.serenity.state.models.ReplaceWorkflowField.ReplaceWith,
            selectedAction = com.serenity.state.models.ReplaceWorkflowAction.ReplaceNext,
            selectedScope = ReplaceWorkflowScope.Selection,
            statusMessage = Some("Select text to limit replacement")
          )
        )
      )
    )
  }
end ReplaceWorkflowStateManagerSpec
