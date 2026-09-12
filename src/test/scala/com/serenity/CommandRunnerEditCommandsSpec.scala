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

class CommandRunnerEditCommandsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(
    sessionRootOverride: Option[Path] = None,
    configPersistencePath: Option[Path] = None,
    fileDialog: Option[FileDialog] = None
  ): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerEditCommandsSpec"))
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

  "Command runner" should "execute clipboard and select-all editor commands" in {
    val stateManager = createStateManager()
    val bufferId     = stateManager.bufferManager.createBuffer("Hello World", None).unsafeRunSync()
    stateManager.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    stateManager
      .updateState { state =>
        val selected = state.persisted
          .buffers(bufferId)
          .copy(
            editing = state.persisted
              .buffers(bufferId)
              .editing
              .copy(
                selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 5)))
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> selected)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "copy", "copy")

    stateManager.getCurrentState.unsafeRunSync().runtime.clipboard shouldBe Some("Hello")

    executeCommandThroughRunner(stateManager, "select-all", "select-all")

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.selection shouldBe Some(
      Selection(CursorPosition(0, 0), CursorPosition(0, "Hello World".length))
    )

    stateManager
      .updateState(state => state.copy(runtime = state.runtime.copy(clipboard = Some("Draft"))))
      .unsafeRunSync()
    executeCommandThroughRunner(stateManager, "paste", "paste")

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).document.content.collect() shouldBe "Draft"

    executeCommandThroughRunner(stateManager, "select-all", "select-all")
    executeCommandThroughRunner(stateManager, "cut", "cut")

    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.runtime.clipboard shouldBe Some("Draft")
    finalState.persisted.buffers(bufferId).document.content.collect() shouldBe ""
  }

  it should "execute undo and redo editor commands" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager.applyEvent(InsertChar('!')).unsafeRunSync()
    stateManager.applyEvent(InsertChar('!')).unsafeRunSync()

    executeCommandThroughRunner(stateManager, "undo", "undo")

    val undone = stateManager.getCurrentState.unsafeRunSync()
    undone.commandRunnerSurface shouldBe None
    undone.persisted.buffers(bufferId).document.content.collect() shouldBe ""
    undone.persisted.focus shouldBe Focus.EditorPane(PaneId(0))

    executeCommandThroughRunner(stateManager, "redo", "redo")

    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).document.content.collect() shouldBe "!!"
  }
