package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class ReplaceWorkflowStateManagerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def createStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("ReplaceWorkflowStateManagerSpec"))
    StateManager.apply(logger).unsafeRunSync()

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

  "Replace workflow" should "replace all matches in the focused buffer and dismiss the modal" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("needle one\nneedle two\nkeep")
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

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
    updatedState.persisted.focus shouldBe Focus.EditorPane(com.serenity.state.models.PaneId(0))
    updatedState.persisted.buffers(bufferId).document.content.collect() shouldBe "thread one\nthread two\nkeep"
    updatedState.persisted.buffers(bufferId).document.isDirty shouldBe true
  }

  it should "replace the next match and keep the modal open for repeated replacement" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("needle one\nneedle two\nkeep")
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

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
    afterFirstReplace.persisted.buffers(bufferId).document.content.collect() shouldBe "thread one\nneedle two\nkeep"

    stateManager.applyEvent(Enter).unsafeRunSync()

    val afterSecondReplace = stateManager.getCurrentState.unsafeRunSync()
    afterSecondReplace.modalSurface shouldBe defined
    afterSecondReplace.persisted.buffers(bufferId).document.content.collect() shouldBe "thread one\nthread two\nkeep"
  }

  it should "make replace next undoable while leaving the workflow state available" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)
    val original     = "needle one\nneedle two\nkeep"

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope(original)
              ),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(
                cursors = List(CursorPosition(0, 0)),
                selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, "needle".length)))
              ),
            findState = Some(FindState("needle", List(FindResult(0, 0), FindResult(1, 0)), 0))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace", "replace")

    "needle".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(TabKey).unsafeRunSync()
    "thread".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(MoveLeft).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val replaced = stateManager.getCurrentState.unsafeRunSync()
    replaced.modalSurface shouldBe defined
    replaced.persisted.buffers(bufferId).document.content.collect() shouldBe "thread one\nneedle two\nkeep"
    replaced.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, "thread".length))
    replaced.persisted.buffers(bufferId).editing.selection shouldBe None
    replaced.persisted.buffers(bufferId).findState shouldBe Some(FindState("needle", List(FindResult(1, 0)), 0))

    stateManager.applyEvent(Undo).unsafeRunSync()

    val undone = stateManager.getCurrentState.unsafeRunSync()
    undone.modalSurface shouldBe defined
    undone.persisted.focus shouldBe Focus.EditorPane(com.serenity.state.models.PaneId(0))
    undone.persisted.buffers(bufferId).document.content.collect() shouldBe original
    undone.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, 0))
    undone.persisted.buffers(bufferId).editing.selection shouldBe Some(
      Selection(CursorPosition(0, 0), CursorPosition(0, "needle".length))
    )
    undone.persisted.buffers(bufferId).findState shouldBe Some(
      FindState("needle", List(FindResult(0, 0), FindResult(1, 0)), 0)
    )
  }

  it should "drop split-grapheme find results after replace next" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("e cafe\u0301")
              ),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(
                cursors = List(CursorPosition(0, 0))
              ),
            findState = Some(FindState("e", List(FindResult(0, 0)), 0))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace", "replace")

    stateManager.applyEvent(InsertChar('e')).unsafeRunSync()
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(InsertChar('x')).unsafeRunSync()
    stateManager.applyEvent(MoveLeft).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.persisted.buffers(bufferId).document.content.collect() shouldBe "x cafe\u0301"
    updatedState.persisted.buffers(bufferId).findState shouldBe None
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

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("needle one\nneedle two\nneedle three")
              ),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(
                selection = Some(
                  Selection(
                    CursorPosition(1, 0),
                    CursorPosition(1, "needle two".length)
                  )
                )
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace", "replace")

    "needle".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(TabKey).unsafeRunSync()
    "thread".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(MoveLeft).unsafeRunSync()
    stateManager.applyEvent(ModalNavigate(Direction.Down)).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface shouldBe defined
    updatedState.persisted.buffers(bufferId).document.content.collect() shouldBe "needle one\nthread two\nneedle three"

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

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("needle one\nneedle two\nneedle three")
              ),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(
                selection = Some(
                  Selection(
                    CursorPosition(1, 0),
                    CursorPosition(2, "needle three".length)
                  )
                )
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace", "replace")

    "needle".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(TabKey).unsafeRunSync()
    "thread".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(ModalNavigate(Direction.Down)).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface shouldBe None
    updatedState.persisted.buffers(bufferId).document.content.collect() shouldBe "needle one\nthread two\nthread three"
    updatedState.persisted.focus shouldBe Focus.EditorPane(com.serenity.state.models.PaneId(0))
  }

  it should "replace next repeatedly within the original selection scope" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("needle one\nneedle two\nneedle three\noutside needle")
              ),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(
                selection = Some(
                  Selection(
                    CursorPosition(0, 0),
                    CursorPosition(2, "needle three".length)
                  )
                )
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace", "replace")

    "needle".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(TabKey).unsafeRunSync()
    "thread".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(MoveLeft).unsafeRunSync()
    stateManager.applyEvent(ModalNavigate(Direction.Down)).unsafeRunSync()

    stateManager.applyEvent(Enter).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).document.content.collect() shouldBe
      "thread one\nneedle two\nneedle three\noutside needle"

    stateManager.applyEvent(Enter).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).document.content.collect() shouldBe
      "thread one\nthread two\nneedle three\noutside needle"

    stateManager.applyEvent(Enter).unsafeRunSync()
    val afterThird = stateManager.getCurrentState.unsafeRunSync()
    afterThird.persisted.buffers(bufferId).document.content.collect() shouldBe
      "thread one\nthread two\nthread three\noutside needle"
    afterThird.modalSurface shouldBe defined

    stateManager.applyEvent(Enter).unsafeRunSync()
    val afterExhausted = stateManager.getCurrentState.unsafeRunSync()
    afterExhausted.persisted.buffers(bufferId).document.content.collect() shouldBe
      "thread one\nthread two\nthread three\noutside needle"
    afterExhausted.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          ReplaceWorkflowState(
            findText = "needle",
            replacementText = "thread",
            activeField = com.serenity.state.models.ReplaceWorkflowField.ReplaceWith,
            selectedAction = com.serenity.state.models.ReplaceWorkflowAction.ReplaceNext,
            selectedScope = ReplaceWorkflowScope.Selection,
            statusMessage = Some("No matches found")
          )
        )
      )
    )
  }

  it should "shrink the active selection scope after a shorter replace-next result" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("needle one\nneedle two\noutside needle")
              ),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(
                selection = Some(
                  Selection(
                    CursorPosition(0, 0),
                    CursorPosition(1, "needle two".length)
                  )
                )
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace", "replace")

    "needle".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(InsertChar('n')).unsafeRunSync()
    stateManager.applyEvent(MoveLeft).unsafeRunSync()
    stateManager.applyEvent(ModalNavigate(Direction.Down)).unsafeRunSync()

    stateManager.applyEvent(Enter).unsafeRunSync()

    val afterFirst = stateManager.getCurrentState.unsafeRunSync()
    afterFirst.persisted.buffers(bufferId).document.content.collect() shouldBe "n one\nneedle two\noutside needle"
    afterFirst.persisted.buffers(bufferId).editing.selection shouldBe Some(
      Selection(CursorPosition(0, 0), CursorPosition(1, "needle two".length))
    )

    stateManager.applyEvent(Enter).unsafeRunSync()

    val afterSecond = stateManager.getCurrentState.unsafeRunSync()
    afterSecond.persisted.buffers(bufferId).document.content.collect() shouldBe "n one\nn two\noutside needle"
    afterSecond.persisted.buffers(bufferId).editing.selection shouldBe Some(
      Selection(CursorPosition(0, 0), CursorPosition(1, "n two".length))
    )

    stateManager.applyEvent(Enter).unsafeRunSync()

    val afterExhausted = stateManager.getCurrentState.unsafeRunSync()
    afterExhausted.persisted.buffers(bufferId).document.content.collect() shouldBe "n one\nn two\noutside needle"
    afterExhausted.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          ReplaceWorkflowState(
            findText = "needle",
            replacementText = "n",
            activeField = com.serenity.state.models.ReplaceWorkflowField.ReplaceWith,
            selectedAction = com.serenity.state.models.ReplaceWorkflowAction.ReplaceNext,
            selectedScope = ReplaceWorkflowScope.Selection,
            statusMessage = Some("No matches found")
          )
        )
      )
    )
  }

  it should "place the cursor at the final replacement and make replace-all undoable" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)
    val original     = "needle one\nmiddle\nneedle two"

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope(original)
              ),
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(
                cursors = List(CursorPosition(0, 0)),
                selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, "needle".length)))
              ),
            findState = Some(FindState("needle", List(FindResult(0, 0), FindResult(2, 0)), 0))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace", "replace")

    "needle".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(TabKey).unsafeRunSync()
    "thread".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val replaced = stateManager.getCurrentState.unsafeRunSync()
    replaced.modalSurface shouldBe None
    replaced.persisted.buffers(bufferId).document.content.collect() shouldBe "thread one\nmiddle\nthread two"
    replaced.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(2, "thread".length))
    replaced.persisted.buffers(bufferId).editing.selection shouldBe None
    replaced.persisted.buffers(bufferId).editing.selections shouldBe Nil
    replaced.persisted.buffers(bufferId).findState shouldBe None

    stateManager.applyEvent(Undo).unsafeRunSync()

    val undone = stateManager.getCurrentState.unsafeRunSync()
    undone.persisted.buffers(bufferId).document.content.collect() shouldBe original
    undone.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, 0))
    undone.persisted.buffers(bufferId).editing.selection shouldBe Some(
      Selection(CursorPosition(0, 0), CursorPosition(0, "needle".length))
    )
    undone.persisted.buffers(bufferId).findState shouldBe Some(
      FindState("needle", List(FindResult(0, 0), FindResult(2, 0)), 0)
    )
  }

  it should "refresh find-all results after replace-all when the replacement still matches" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("needle one\nneedle two")
              ),
            findState = Some(FindState("needle", List(FindResult(0, 0), FindResult(1, 0)), 0))
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace-all", "replace-all")

    "needle".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(TabKey).unsafeRunSync()
    "needle!".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface shouldBe None
    updatedState.persisted.buffers(bufferId).document.content.collect() shouldBe "needle! one\nneedle! two"
    updatedState.persisted.buffers(bufferId).findState shouldBe Some(
      FindState("needle", List(FindResult(0, 0), FindResult(1, 0)), 0)
    )
  }

  it should "leave the buffer clean and keep the modal open when replace-all has no matches" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("plain text"),
                isDirty = false
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace", "replace")

    "needle".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(TabKey).unsafeRunSync()
    "thread".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.persisted.buffers(bufferId).document.content.collect() shouldBe "plain text"
    updatedState.persisted.buffers(bufferId).document.isDirty shouldBe false
    updatedState.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          ReplaceWorkflowState(
            findText = "needle",
            replacementText = "thread",
            activeField = com.serenity.state.models.ReplaceWorkflowField.ReplaceWith,
            statusMessage = Some("No matches found")
          )
        )
      )
    )
  }

  it should "not replace matches that split a grapheme cluster" in {
    val stateManager   = createStateManager()
    val bufferId       = BufferId(0)
    val flag           = "\uD83C\uDDFA\uD83C\uDDF8"
    val firstIndicator = flag.substring(0, 2)
    val original       = s"a$flag one\na$flag two"

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope(original),
                isDirty = false
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "replace-all", "replace-all")

    firstIndicator.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager.applyEvent(TabKey).unsafeRunSync()
    stateManager.applyEvent(InsertChar('!')).unsafeRunSync()
    stateManager.applyEvent(Enter).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.persisted.buffers(bufferId).document.content.collect() shouldBe original
    updatedState.persisted.buffers(bufferId).document.isDirty shouldBe false
    updatedState.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          ReplaceWorkflowState(
            findText = firstIndicator,
            replacementText = "!",
            activeField = com.serenity.state.models.ReplaceWorkflowField.ReplaceWith,
            selectedAction = com.serenity.state.models.ReplaceWorkflowAction.ReplaceAll,
            statusMessage = Some("No matches found")
          )
        )
      )
    )
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
