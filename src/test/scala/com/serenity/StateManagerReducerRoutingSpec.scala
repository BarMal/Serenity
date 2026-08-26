package com.serenity

import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerReducerRoutingSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
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

  it should "reject background mutations before dispatch while a blocking modal is active" in {
    val tempFile = Files.createTempFile("state-manager-modal-gate", ".scala")

    try
      val stateManager = createStateManager()
      val bufferId     = stateManager.createBuffer("unsaved").unsafeRunSync()

      stateManager
        .updateState { state =>
          state.copy(
            buffers = state.buffers.updated(
              bufferId,
              state.buffers(bufferId).copy(document = state.buffers(bufferId).document.copy(filePath = Some(tempFile)))
            ),
            layout = state.layout.copy(
              editorPanes = state.layout.editorPanes.updated(
                PaneId(0),
                state.layout.editorPanes(PaneId(0)).copy(bufferId = Some(bufferId))
              )
            ),
            focus = Focus.EditorPane(PaneId(0))
          )
        }
        .unsafeRunSync()
      stateManager.applyEvent(InsertChar('!')).unsafeRunSync()
      stateManager.applyEvent(DeleteBackward).unsafeRunSync()
      stateManager.applyEvent(Undo).unsafeRunSync()

      val modal = UiSurface(
        SurfaceId("close-confirmation"),
        SurfaceContent.ModalWorkflow(
          Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, bufferId, "notes.scala"))
        ),
        SurfacePresentation.Modal
      )
      stateManager
        .updateState(state => state.copy(uiSurfaces = state.uiSurfaces :+ modal, focus = Focus.Surface(modal.id)))
        .unsafeRunSync()
      val before = stateManager.getCurrentState.unsafeRunSync()

      List[Event](Undo, Redo, SaveFile, SwitchTheme("light"), ToggleCommandRunner, CloseTab, Quit).foreach { event =>
        stateManager.applyEvent(event).unsafeRunSync()
        stateManager.getCurrentState.unsafeRunSync() shouldBe before
      }

      Files.readString(tempFile) shouldBe ""
    finally Files.deleteIfExists(tempFile)
  }

  it should "allow modal and system input through the blocking modal gate" in {
    val stateManager = createStateManager()
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    val bufferId     = initialState.focusedBufferId.get

    stateManager
      .showModal(Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, bufferId, "notes.scala")))
      .unsafeRunSync()
    stateManager.applyEvent(ModalNavigate(Direction.Right)).unsafeRunSync()
    stateManager.applyEvent(ResizeEvent(ViewportSize(120, 40))).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface.flatMap(_.content match
      case SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)) => Some(workflow.selectedChoice)
      case _ => None) shouldBe Some(CloseWorkflowChoice.Discard)
    updatedState.viewportSize shouldBe Some(ViewportSize(120, 40))
  }

  it should "save the focused buffer through the file event path" in {
    val tempFile       = Files.createTempFile("state-manager-save", ".scala")
    val initialContent = "val x = 42"

    try
      Files.writeString(tempFile, initialContent)

      val stateManager = createStateManager()
      val bufferId     = com.serenity.state.models.BufferId(99)
      val paneId       = com.serenity.state.models.PaneId(0)
      val baseBuffer   = com.serenity.state.models.Buffer.fromString(bufferId, "val x = 100")
      val buffer       = baseBuffer.copy(document = baseBuffer.document.copy(filePath = Some(tempFile), isDirty = true))

      stateManager
        .updateState { state =>
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
        }
        .unsafeRunSync()

      stateManager.applyEvent(SaveFile).unsafeRunSync()

      Files.readString(tempFile) shouldBe "val x = 100"
      val updatedState = stateManager.getCurrentState.unsafeRunSync()
      updatedState.buffers(bufferId).document.isDirty shouldBe false
    finally Files.deleteIfExists(tempFile)
  }

  it should "route modal and peek lifecycle through the state manager without losing focus invariants" in {
    val stateManager = createStateManager()

    stateManager.showModal(Modal.GotoLine("7")).unsafeRunSync()
    val modalState   = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = modalState.uiSurfaces.find(_.content == SurfaceContent.ModalWorkflow(Modal.GotoLine("7")))
    modalSurface shouldBe defined
    modalState.focus shouldBe Focus.Surface(modalSurface.get.id)

    stateManager.dismissModal().unsafeRunSync()
    val afterDismiss = stateManager.getCurrentState.unsafeRunSync()
    afterDismiss.focus shouldBe Focus.EditorPane(com.serenity.state.models.PaneId(0))
    afterDismiss.uiSurfaces.exists(_.content == SurfaceContent.ModalWorkflow(Modal.GotoLine("7"))) shouldBe false

    stateManager.showPeek(PeekContent.QuickInfo("hint"), CursorPosition(1, 2)).unsafeRunSync()
    val peekState   = stateManager.getCurrentState.unsafeRunSync()
    val peekSurface = peekState.uiSurfaces.find(_.content == SurfaceContent.QuickInfo("hint"))
    peekSurface shouldBe defined
    peekState.focus shouldBe Focus.Surface(peekSurface.get.id)

    stateManager.dismissPeek().unsafeRunSync()
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.focus shouldBe Focus.EditorPane(com.serenity.state.models.PaneId(0))
    finalState.uiSurfaces.exists(_.content == SurfaceContent.QuickInfo("hint")) shouldBe false
  }

  it should "route focused editor events through the typed local handler path" in {
    val stateManager    = createStateManager()
    val initialState    = stateManager.getCurrentState.unsafeRunSync()
    val initialBufferId = initialState.focusedBufferId.get

    stateManager.applyEvent(InsertChar('x')).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.buffers(initialBufferId).document.content.collect() shouldBe "x"
    updatedState.focus shouldBe Focus.EditorPane(com.serenity.state.models.PaneId(0))
  }

  it should "route focused modal events through the typed local handler path" in {
    val stateManager = createStateManager()

    stateManager
      .showModal(
        Modal.FileWorkflow(
          com.serenity.state.models.FileWorkflowState(mode = FileWorkflowMode.Open)
        )
      )
      .unsafeRunSync()

    stateManager.applyEvent(TabKey).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface.flatMap(_.content match
      case SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)) => Some(workflow.activeField)
      case _                                                          => None) shouldBe Some(FileWorkflowField.Path)
  }

  it should "apply only the latest deferred find query" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        state
          .copy(buffers =
            state.buffers.updated(
              bufferId,
              state
                .buffers(bufferId)
                .copy(document = state.buffers(bufferId).document.copy(content = Rope("needle need")))
            )
          )
      }
      .unsafeRunSync()
    stateManager.showModal(Modal.Find("", Nil, 0)).unsafeRunSync()

    "need".foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    stateManager
      .updateState { state =>
        state.copy(buffers =
          state.buffers.updated(
            bufferId,
            state.buffers(bufferId).copy(document = state.buffers(bufferId).document.copy(content = Rope("other")))
          )
        )
      }
      .unsafeRunSync()

    IO.sleep(150.millis).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.Find("need", Nil, 0)))
    updatedState.buffers(bufferId).findState shouldBe None
  }

  it should "route focused pinned panel events through the typed local handler path" in {
    val stateManager = createStateManager()

    stateManager
      .pinPanel(
        PanelContent.DirectoryTree(DirectoryTreeData(java.nio.file.Paths.get("/repo")), None),
        PanelPosition.Left,
        24
      )
      .unsafeRunSync()
    val pinnedSurfaceId = stateManager.getCurrentState
      .unsafeRunSync()
      .uiSurfaces
      .collectFirst {
        case surface @ UiSurface(
              _,
              _,
              com.serenity.state.models.SurfacePresentation.Pinned(PanelPosition.Left, _),
              _
            ) =>
          surface.id
      }
      .get
    stateManager.switchFocus(Focus.Surface(pinnedSurfaceId)).unsafeRunSync()

    stateManager.applyEvent(PanelInputEvent.ReturnFocus).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.focus shouldBe Focus.EditorPane(com.serenity.state.models.PaneId(0))
  }

  it should "route focused peek events through the typed local handler path" in {
    val stateManager = createStateManager()

    stateManager.showPeek(PeekContent.QuickInfo("hint"), CursorPosition(1, 2)).unsafeRunSync()
    stateManager.applyEvent(PeekInputEvent.Navigate(Direction.Up)).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.uiSurfaces.exists(_.content == SurfaceContent.QuickInfo("hint")) shouldBe false
    updatedState.focus shouldBe Focus.EditorPane(com.serenity.state.models.PaneId(0))
  }

  it should "ignore local events when surface focus points at no surface" in {
    val stateManager = createStateManager()
    val missingFocus = Focus.Surface(SurfaceId("missing"))

    stateManager.updateState(_.copy(focus = missingFocus)).unsafeRunSync()
    stateManager.applyEvent(PeekInputEvent.Dismiss).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().focus shouldBe missingFocus
  }

  it should "route theme events through the application event path" in {
    val stateManager = createStateManager()

    stateManager.applyEvent(SwitchTheme("light")).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().theme.name shouldBe "light"

    stateManager.applyEvent(ReloadCurrentTheme).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().theme.name shouldBe "light"
  }
