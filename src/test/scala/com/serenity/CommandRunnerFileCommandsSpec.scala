package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.FileDialog
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CommandRunnerFileCommandsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def testFileDialog(
    openSelection: Option[Path] = None,
    saveSelection: Option[Path] = None
  ): FileDialog =
    FileDialog(
      chooseOpenFile = _ => IO.pure(openSelection),
      chooseSaveFile = (_, _) => IO.pure(saveSelection)
    )

  private def createStateManager(
    sessionRootOverride: Option[Path] = None,
    configPersistencePath: Option[Path] = None,
    fileDialog: Option[FileDialog] = None
  ): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerFileCommandsSpec"))
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


  "Command runner" should "create and focus a new empty buffer for the new command" in {
    val stateManager = createStateManager()
    val initialState = stateManager.getCurrentState.unsafeRunSync()

    initialState.persisted.bufferOrder shouldBe List(com.serenity.state.models.BufferId(0))

    executeCommandThroughRunner(stateManager, "new", "new")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.persisted.bufferOrder shouldBe List(
      com.serenity.state.models.BufferId(0),
      com.serenity.state.models.BufferId(1)
    )
    updatedState.focusedBufferId shouldBe Some(com.serenity.state.models.BufferId(1))
    updatedState.persisted.buffers(com.serenity.state.models.BufferId(1)).document.isNewEmpty shouldBe true
  }

  it should "open file search through the typed file-search command" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "file-search", "file-search")

    stateManager.getCurrentState.unsafeRunSync().runtime.uiSurfaces.exists {
      _.content match
        case SurfaceContent.FileSearch(_) => true
        case _                            => false
    } shouldBe true
  }

  it should "save the focused buffer through the native save-as file dialog" in {
    val targetPath   = Files.createTempDirectory("serenity-save-as").resolve("notes-copy.scala")
    val stateManager = createStateManager(fileDialog = Some(testFileDialog(saveSelection = Some(targetPath))))
    val bufferId     = BufferId(0)
    val filePath     = Path.of("temp", "notes.scala")

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("saved through dialog"),
                filePath = Some(filePath),
                isDirty = true
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "save-as", "save-as")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.topModal shouldBe None
    updatedState.persisted.buffers(bufferId).document.filePath shouldBe Some(targetPath)
    updatedState.persisted.buffers(bufferId).document.isDirty shouldBe false
    Files.readString(targetPath) shouldBe "saved through dialog"
  }

  it should "open a selected file through the native open-file dialog" in {
    val sourcePath = Files.createTempDirectory("serenity-open").resolve("notes.md")
    Files.writeString(sourcePath, "# Notes")
    val stateManager = createStateManager(fileDialog = Some(testFileDialog(openSelection = Some(sourcePath))))
    val viewportSize = ViewportSize(120, 40)
    stateManager.paneManager.handleViewportResize(viewportSize).unsafeRunSync()

    executeCommandThroughRunner(stateManager, "open", "open")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.topModal shouldBe None
    val openedBuffer = updatedState.persisted.buffers.values.find(_.document.filePath.contains(sourcePath))
    openedBuffer.map(_.document.content.collect()) shouldBe Some("# Notes")
    openedBuffer.flatMap(_.document.language) shouldBe Some(LanguageId.Markdown)
    val paneId = updatedState.persisted.layout.activeEditorPaneId.getOrElse(fail("Expected active pane"))
    val layout = LayoutEngine.calculateLayout(updatedState, viewportSize)
    val contentRect = LayoutEngine
      .calculateEditorPaneLayouts(updatedState, layout)(paneId)
      .contentRect
    openedBuffer.map(_.viewport.visibleColumns) shouldBe Some(contentRect.width)
    openedBuffer.map(_.viewport.visibleLines) shouldBe Some(contentRect.height)
  }

  it should "open the in-app save-as form when no native dialog is available" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document =
            state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("no dialog here"),
                filePath = Some(Path.of("temp", "notes.scala"))
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "save-as", "save-as")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val workflow = updatedState.topModal
      .flatMap(_.modal match
        case Modal.FileWorkflow(w) => Some(w)
        case _                     => None)
      .getOrElse(fail("Expected active file workflow modal"))
    workflow.mode shouldBe FileWorkflowMode.SaveAs
    workflow.filename shouldBe "notes.scala"
    updatedState.persisted.buffers(bufferId).document.filePath shouldBe Some(Path.of("temp", "notes.scala"))
  }

  it should "open the in-app open-file form when no native dialog is available" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "open", "open")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val workflow = updatedState.topModal
      .flatMap(_.modal match
        case Modal.FileWorkflow(w) => Some(w)
        case _                     => None)
      .getOrElse(fail("Expected active file workflow modal"))
    workflow.mode shouldBe FileWorkflowMode.Open
  }

  it should "save an unsaved buffer through the native save-as file dialog" in {
    val targetPath   = Files.createTempDirectory("serenity-unsaved-save").resolve("draft.txt")
    val stateManager = createStateManager(fileDialog = Some(testFileDialog(saveSelection = Some(targetPath))))

    stateManager.bufferManager.updateBuffer(BufferId(0), "draft body").unsafeRunSync()

    executeCommandThroughRunner(stateManager, "save", "save")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.topModal shouldBe None
    updatedState.persisted.buffers(BufferId(0)).document.filePath shouldBe Some(targetPath)
    updatedState.persisted.buffers(BufferId(0)).document.isDirty shouldBe false
    Files.readString(targetPath) shouldBe "draft body"
  }

  it should "open an unsaved-changes workflow for the close command when the current buffer is dirty" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document = state.persisted.buffers(bufferId).document.copy(isDirty = true))
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "close", "close")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.topModal
      .flatMap(_.modal match
        case Modal.CloseWorkflow(workflow) => Some(workflow)
        case _                             => None)
      .map(_.scope) shouldBe Some(CloseScope.Current)
    updatedState.persisted.focus shouldBe Focus.Modal
  }

  it should "open an unsaved-changes workflow for the close command when an untitled buffer was edited back to empty" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document =
            state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope.empty,
                filePath = None,
                isDirty = false,
                isNewEmpty = false
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "close", "close")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.persisted.buffers.contains(bufferId) shouldBe true
    updatedState.topModal
      .flatMap(_.modal match
        case Modal.CloseWorkflow(workflow) => Some(workflow)
        case _                             => None)
      .map(workflow => (workflow.scope, workflow.currentBufferId)) shouldBe Some((CloseScope.Current, bufferId))
  }

  it should "open an unsaved-changes workflow for the close-all command when any affected buffer is dirty" in {
    val stateManager  = createStateManager()
    val dirtyBufferId = stateManager.bufferManager.createBuffer("dirty buffer", None).unsafeRunSync()

    stateManager
      .updateState { state =>
        val buffer =
          state.persisted
            .buffers(dirtyBufferId)
            .copy(document = state.persisted.buffers(dirtyBufferId).document.copy(isDirty = true))
        state.copy(persisted =
          state.persisted.copy(
            buffers = state.persisted.buffers + (dirtyBufferId -> buffer)
          )
        )
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "close-all", "close-all")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.topModal
      .flatMap(_.modal match
        case Modal.CloseWorkflow(workflow) => Some(workflow)
        case _                             => None)
      .map(_.scope) shouldBe Some(CloseScope.All)
  }

  it should "open an unsaved-changes workflow for the close-others command when any other buffer is dirty" in {
    val stateManager  = createStateManager()
    val dirtyBufferId = stateManager.bufferManager.createBuffer("dirty buffer", None).unsafeRunSync()

    stateManager
      .updateState { state =>
        val buffer =
          state.persisted
            .buffers(dirtyBufferId)
            .copy(document = state.persisted.buffers(dirtyBufferId).document.copy(isDirty = true))
        state.copy(persisted =
          state.persisted.copy(
            buffers = state.persisted.buffers + (dirtyBufferId -> buffer)
          )
        )
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "close-others", "close-others")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.topModal
      .flatMap(_.modal match
        case Modal.CloseWorkflow(workflow) => Some(workflow)
        case _                             => None)
      .map(_.scope) shouldBe Some(CloseScope.Others)
  }

  it should "open an unsaved-changes workflow for the quit command when any buffer is dirty" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(document = state.persisted.buffers(bufferId).document.copy(isDirty = true))
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "quit", "quit")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.topModal
      .flatMap(_.modal match
        case Modal.CloseWorkflow(workflow) => Some(workflow)
        case _                             => None)
      .map(_.scope) shouldBe Some(CloseScope.Quit)
  }
