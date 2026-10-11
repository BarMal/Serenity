package com.serenity.state.manager

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{RunCommand, Undo}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.setBufferForPane
import com.serenity.state.manager.StateManagerTestFacade.{createBuffer, updateState}
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** Commenting a selection out the way a user does: by running the registry command, then undoing it. */
class CommentToggleScenarioSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = NoOpFactory[IO]

  private val source = "def f =\n  val a = 1\n\n  a"

  private trait ScalaBuffer:
    val stateManager: StateManager = StateManager(NoOpLogger[IO]).unsafeRunSync()
    val bufferId: BufferId         = stateManager.createBuffer(source, None).unsafeRunSync()
    val paneId: PaneId             = stateManager.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keys.head

    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager
      .updateState { state =>
        val buffer = state.persisted.buffers(bufferId)
        val prepared = buffer.copy(
          document = buffer.document.copy(language = Some(LanguageId.Scala)),
          editing = EditingStateFixtures(
            selection = Some(Selection(CursorPosition(0, 0), CursorPosition(3, 3)))
          )
        )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> prepared)))
      }
      .unsafeRunSync()

    def run(commandId: String): Unit = stateManager.applyEvent(RunCommand(commandId)).unsafeRunSync()

    def undo(): Unit = stateManager.applyEvent(Undo).unsafeRunSync()

    def text: String =
      stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).document.content.collect()

  "toggle-line-comment" should "comment the selected lines and uncomment them again" in new ScalaBuffer:
    run("toggle-line-comment")
    text shouldBe "// def f =\n//   val a = 1\n\n//   a"

    run("toggle-line-comment")
    text shouldBe source

  it should "be undone in a single step" in new ScalaBuffer:
    run("toggle-line-comment")
    undo()

    text shouldBe source

  "toggle-block-comment" should "wrap the selection in one comment and be undone in a single step" in new ScalaBuffer:
    run("toggle-block-comment")
    text shouldBe "/* def f =\n  val a = 1\n\n  a */"

    undo()
    text shouldBe source
