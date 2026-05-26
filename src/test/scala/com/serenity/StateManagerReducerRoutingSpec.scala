package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Quit, SaveFile, ToggleCommandRunner}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{CursorPosition, Focus, Modal, SurfaceContent, UiSurface}
import com.serenity.ui.layout.PeekContent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerReducerRoutingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerReducerRoutingSpec"))
    StateManager.apply(logger).unsafeRunSync()

  "StateManager.applyEvent" should "toggle the command runner through the application event path" in {
    val stateManager = createStateManager()

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val openedState = stateManager.getCurrentState.unsafeRunSync()
    val commandSurface = openedState.uiSurfaces.collectFirst {
      case surface @ UiSurface(_, SurfaceContent.CommandPalette(_), _, _) => surface
    }

    commandSurface shouldBe defined
    openedState.focus shouldBe Focus.Surface(commandSurface.get.id)
    commandSurface.get.content match
      case SurfaceContent.CommandPalette(runner) => runner.isActive shouldBe true
      case other                                 => fail(s"Expected command palette, got $other")

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val closedState = stateManager.getCurrentState.unsafeRunSync()

    closedState.focus shouldBe Focus.EditorPane(com.serenity.state.models.PaneId(0))
    closedState.commandRunnerSurface shouldBe None
  }

  it should "complete the quit signal through the application event path" in {
    val stateManager = createStateManager()

    stateManager.applyEvent(Quit).unsafeRunSync()
    stateManager.awaitQuit.unsafeRunSync()

    succeed
  }

  it should "save the focused buffer through the file event path" in {
    val tempFile       = Files.createTempFile("state-manager-save", ".scala")
    val initialContent = "val x = 42"

    try
      Files.writeString(tempFile, initialContent)

      val stateManager = createStateManager()
      val bufferId     = com.serenity.state.models.BufferId(99)
      val paneId       = com.serenity.state.models.PaneId(0)
      val buffer = com.serenity.state.models.Buffer
        .fromString(bufferId, "val x = 100")
        .copy(filePath = Some(tempFile), isDirty = true)

      stateManager.updateState { state =>
        state.copy(
          buffers = state.buffers + (bufferId -> buffer),
          layout = state.layout.copy(
            editorPanes = state.layout.editorPanes.updated(
              paneId,
              state.layout.editorPanes(paneId).copy(bufferId = Some(bufferId))
            )
          ),
          focus = Focus.EditorPane(paneId)
        )
      }.unsafeRunSync()

      stateManager.applyEvent(SaveFile).unsafeRunSync()

      Files.readString(tempFile) shouldBe "val x = 100"
      val updatedState = stateManager.getCurrentState.unsafeRunSync()
      updatedState.buffers(bufferId).isDirty shouldBe false
    finally Files.deleteIfExists(tempFile)
  }

  it should "route modal and peek lifecycle through the state manager without losing focus invariants" in {
    val stateManager = createStateManager()

    stateManager.showModal(Modal.GotoLine("7")).unsafeRunSync()
    val modalState = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = modalState.uiSurfaces.find(_.content == SurfaceContent.ModalWorkflow(Modal.GotoLine("7")))
    modalSurface shouldBe defined
    modalState.focus shouldBe Focus.Surface(modalSurface.get.id)

    stateManager.dismissModal().unsafeRunSync()
    val afterDismiss = stateManager.getCurrentState.unsafeRunSync()
    afterDismiss.focus shouldBe Focus.EditorPane(com.serenity.state.models.PaneId(0))
    afterDismiss.uiSurfaces.exists(_.content == SurfaceContent.ModalWorkflow(Modal.GotoLine("7"))) shouldBe false

    stateManager.showPeek(PeekContent.QuickInfo("hint"), CursorPosition(1, 2)).unsafeRunSync()
    val peekState = stateManager.getCurrentState.unsafeRunSync()
    val peekSurface = peekState.uiSurfaces.find(_.content == SurfaceContent.QuickInfo("hint"))
    peekSurface shouldBe defined
    peekState.focus shouldBe Focus.Surface(peekSurface.get.id)

    stateManager.dismissPeek().unsafeRunSync()
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.focus shouldBe Focus.EditorPane(com.serenity.state.models.PaneId(0))
    finalState.uiSurfaces.exists(_.content == SurfaceContent.QuickInfo("hint")) shouldBe false
  }
