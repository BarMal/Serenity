package com.serenity

import java.nio.file.Path

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

import com.serenity.io.FileUtils
import com.serenity.keystroke.events.{Enter, InsertChar, ToggleCommandRunner}
import com.serenity.lsp.config.LanguageId
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.PanelPosition

class CommandRunnerCoreCommandsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerCoreCommandsSpec"))
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

  "Command runner" should "create and focus a new empty buffer for the new command" in {
    val stateManager = createStateManager()
    val initialState = stateManager.getCurrentState.unsafeRunSync()

    initialState.bufferOrder shouldBe List(com.serenity.state.models.BufferId(0))

    executeCommandThroughRunner(stateManager, "new", "new")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.bufferOrder shouldBe List(
      com.serenity.state.models.BufferId(0),
      com.serenity.state.models.BufferId(1)
    )
    updatedState.focusedBufferId shouldBe Some(com.serenity.state.models.BufferId(1))
    updatedState.buffers(com.serenity.state.models.BufferId(1)).isNewEmpty shouldBe true
  }

  it should "open the goto-line modal for the goto-line command" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "goto-line", "goto-line")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = updatedState.modalSurface

    updatedState.commandRunnerSurface shouldBe None
    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("")))
    updatedState.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "open the find modal for the find command" in {
    val stateManager = createStateManager()
    val cursor       = CursorPosition(0, 0)

    executeCommandThroughRunner(stateManager, "find", "find")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = updatedState.modalSurface

    updatedState.commandRunnerSurface shouldBe None
    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.Find("", Nil, 0)))
    modalSurface.map(_.presentation) shouldBe Some(
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
    )
    updatedState.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "open the find modal from the command runner with the active buffer's existing query" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha\nbeta\nalpha"),
            cursors = List(CursorPosition(2, 0)),
            findState = Some(FindState("alpha", List(FindResult(0, 0), FindResult(2, 0)), 1))
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "find", "find")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = updatedState.modalSurface

    updatedState.commandRunnerSurface shouldBe None
    modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(Modal.Find("alpha", List(FindResult(0, 0), FindResult(2, 0)), 1))
    )
    modalSurface.map(_.presentation) shouldBe Some(
      SurfacePresentation.Floating(Some(CursorPosition(2, 0)), SurfacePlacement.BelowCursor)
    )
    updatedState.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "open the replace modal for the replace command" in {
    val stateManager = createStateManager()
    val cursor       = CursorPosition(0, 0)

    executeCommandThroughRunner(stateManager, "replace", "replace")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = updatedState.modalSurface

    updatedState.commandRunnerSurface shouldBe None
    modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          com.serenity.state.models.ReplaceWorkflowState()
        )
      )
    )
    modalSurface.map(_.presentation) shouldBe Some(
      SurfacePresentation.Floating(Some(cursor), SurfacePlacement.BelowCursor)
    )
    updatedState.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "toggle between dark and light themes for the toggle-theme command" in {
    val stateManager = createStateManager()
    val initialState = stateManager.getCurrentState.unsafeRunSync()

    initialState.theme.name shouldBe "dark"

    executeCommandThroughRunner(stateManager, "toggle-theme", "toggle-theme")

    val lightState = stateManager.getCurrentState.unsafeRunSync()
    lightState.commandRunnerSurface shouldBe None
    lightState.theme.name shouldBe "light"

    executeCommandThroughRunner(stateManager, "toggle-theme", "toggle-theme")

    val darkState = stateManager.getCurrentState.unsafeRunSync()
    darkState.theme.name shouldBe "dark"
  }

  it should "reload the current theme for the reload-theme command" in {
    val stateManager = createStateManager()

    stateManager.updateState(_.copy(theme = com.serenity.ui.theme.Theme.light)).unsafeRunSync()

    executeCommandThroughRunner(stateManager, "reload-theme", "reload-theme")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.theme.name shouldBe "light"
  }

  it should "open a save-as file workflow modal seeded from the focused buffer path" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)
    val filePath     = Path.of("temp", "notes.scala")

    stateManager
      .updateState { state =>
        val buffer = state.buffers(bufferId).copy(filePath = Some(filePath))
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "save-as", "save-as")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(
          com.serenity.state.models.FileWorkflowState(
            mode = FileWorkflowMode.SaveAs,
            filename = "notes.scala",
            path = filePath.getParent.toString
          )
        )
      )
    )
  }

  it should "open an open-file workflow modal rooted at the current working directory" in {
    val stateManager     = createStateManager()
    val currentDirectory = FileUtils.getCurrentDirectory.unsafeRunSync().toString

    executeCommandThroughRunner(stateManager, "open", "open")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(
          com.serenity.state.models.FileWorkflowState(
            mode = FileWorkflowMode.Open,
            path = currentDirectory
          )
        )
      )
    )
  }

  it should "fall back to a save-as file workflow modal when save is invoked for an unsaved buffer" in {
    val stateManager     = createStateManager()
    val currentDirectory = FileUtils.getCurrentDirectory.unsafeRunSync().toString

    executeCommandThroughRunner(stateManager, "save", "save")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.FileWorkflow(
          com.serenity.state.models.FileWorkflowState(
            mode = FileWorkflowMode.SaveAs,
            path = currentDirectory
          )
        )
      )
    )
  }

  it should "open an unsaved-changes workflow for the close command when the current buffer is dirty" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.buffers(bufferId).copy(isDirty = true)
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "close", "close")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.modalSurface
      .flatMap(_.content match
        case SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)) => Some(workflow)
        case _                                                           => None)
      .map(_.scope) shouldBe Some(CloseScope.Current)
    updatedState.focus shouldBe Focus.Surface(updatedState.modalSurface.get.id)
  }

  it should "open an unsaved-changes workflow for the close-all command when any affected buffer is dirty" in {
    val stateManager  = createStateManager()
    val dirtyBufferId = stateManager.createBuffer("dirty buffer").unsafeRunSync()

    stateManager
      .updateState { state =>
        val buffer = state.buffers(dirtyBufferId).copy(isDirty = true)
        state.copy(
          buffers = state.buffers + (dirtyBufferId -> buffer),
          bufferOrder = state.bufferOrder :+ dirtyBufferId
        )
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "close-all", "close-all")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface
      .flatMap(_.content match
        case SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)) => Some(workflow)
        case _                                                           => None)
      .map(_.scope) shouldBe Some(CloseScope.All)
  }

  it should "open an unsaved-changes workflow for the close-others command when any other buffer is dirty" in {
    val stateManager  = createStateManager()
    val dirtyBufferId = stateManager.createBuffer("dirty buffer").unsafeRunSync()

    stateManager
      .updateState { state =>
        val buffer = state.buffers(dirtyBufferId).copy(isDirty = true)
        state.copy(
          buffers = state.buffers + (dirtyBufferId -> buffer),
          bufferOrder = state.bufferOrder :+ dirtyBufferId
        )
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "close-others", "close-others")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface
      .flatMap(_.content match
        case SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)) => Some(workflow)
        case _                                                           => None)
      .map(_.scope) shouldBe Some(CloseScope.Others)
  }

  it should "open an unsaved-changes workflow for the quit command when any buffer is dirty" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.buffers(bufferId).copy(isDirty = true)
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "quit", "quit")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.modalSurface
      .flatMap(_.content match
        case SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)) => Some(workflow)
        case _                                                           => None)
      .map(_.scope) shouldBe Some(CloseScope.Quit)
  }

  it should "pin the explorer panel from the command runner" in {
    val stateManager     = createStateManager()
    val currentDirectory = FileUtils.getCurrentDirectory.unsafeRunSync()

    executeCommandThroughRunner(stateManager, "pin-explorer", "pin-explorer")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    val pinnedSurface = updatedState.pinnedSurfaces
      .collectFirst {
        case surface @ com.serenity.state.models.UiSurface(
              _,
              SurfaceContent.DirectoryTree(tree, _),
              com.serenity.state.models.SurfacePresentation.Pinned(PanelPosition.Left, _),
              _
            ) =>
          surface -> tree.rootPath
      }
      .getOrElse(fail("Expected pinned explorer surface"))

    pinnedSurface._2 shouldBe currentDirectory
  }

  it should "pin the outline panel from the command runner" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "pin-outline", "pin-outline")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.pinnedSurfaces.exists {
      _.presentation == com.serenity.state.models.SurfacePresentation.Pinned(PanelPosition.Right, 30)
    } shouldBe true
    updatedState.pinnedSurfaces.exists(_.content == SurfaceContent.Outline(Nil)) shouldBe true
  }

  it should "pin the diagnostics panel from the command runner" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "pin-diagnostics", "pin-diagnostics")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.pinnedSurfaces.exists {
      _.presentation == com.serenity.state.models.SurfacePresentation.Pinned(PanelPosition.Bottom, 10)
    } shouldBe true
    updatedState.pinnedSurfaces.exists(_.content == SurfaceContent.Diagnostics(Nil)) shouldBe true
  }

  it should "pin a right-side Markdown preview for the active Markdown buffer" in {
    val stateManager = createStateManager()

    stateManager
      .updateState { state =>
        val bufferId = BufferId(0)
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("# Notes\n\n![Diagram](diagram.png)"),
            language = Some(LanguageId.Markdown)
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "markdown-preview", "markdown-preview")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    val preview = updatedState.pinnedSurfaces.collectFirst {
      case surface @ UiSurface(
            _,
            SurfaceContent.MarkdownPreview(BufferId(0), "Untitled"),
            SurfacePresentation.Pinned(PanelPosition.Right, 40),
            _
          ) =>
        surface
    }

    preview should not be empty
    updatedState.buffers(BufferId(0)).content.collect() shouldBe "# Notes\n\n![Diagram](diagram.png)"
  }

  it should "leave the workspace unchanged when Markdown preview is requested for a non-Markdown buffer" in {
    val stateManager = createStateManager()
    val before       = stateManager.getCurrentState.unsafeRunSync()

    executeCommandThroughRunner(stateManager, "markdown-preview", "markdown-preview")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.pinnedSurfaces shouldBe before.pinnedSurfaces
  }

  it should "focus the left panel from the command runner" in {
    val stateManager = createStateManager()
    stateManager.loadDirectoryTree(FileUtils.getCurrentDirectory.unsafeRunSync().toString, List("src")).unsafeRunSync()

    executeCommandThroughRunner(stateManager, "focus-left-panel", "focus-left-panel")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.focus shouldBe a[Focus.Surface]
    val focusedId = updatedState.focus match
      case Focus.Surface(id) => id
      case other             => fail(s"Expected focus on a surface, got $other")
    updatedState.pinnedSurfaces.map(_.id) should contain(focusedId)
  }

  it should "unpin the left panel from the command runner" in {
    val stateManager = createStateManager()
    stateManager.loadDirectoryTree(FileUtils.getCurrentDirectory.unsafeRunSync().toString, List("src")).unsafeRunSync()

    executeCommandThroughRunner(stateManager, "unpin-left-panel", "unpin-left-panel")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.pinnedSurfaces.exists {
      _.presentation match
        case com.serenity.state.models.SurfacePresentation.Pinned(PanelPosition.Left, _) => true
        case _                                                                           => false
    } shouldBe false
  }
