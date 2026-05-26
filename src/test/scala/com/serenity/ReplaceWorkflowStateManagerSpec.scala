package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Enter, InsertChar, TabKey, ToggleCommandRunner}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{BufferId, Focus, Modal, ReplaceWorkflowState, SurfaceContent}
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
end ReplaceWorkflowStateManagerSpec
