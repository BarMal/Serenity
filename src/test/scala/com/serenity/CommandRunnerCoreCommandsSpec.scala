package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.SpellCheckConfig
import com.serenity.io.{FileDialog, FileUtils}
import com.serenity.keystroke.events.{Enter, InsertChar, ToggleCommandRunner}
import com.serenity.lsp.config.LanguageId
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CommandRunnerCoreCommandsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private case class TestFileDialog(
      openSelection: Option[Path] = None,
      saveSelection: Option[Path] = None
  ) extends FileDialog:
    override def chooseOpenFile(initialDirectory: Option[Path]): IO[Option[Path]] =
      IO.pure(openSelection)

    override def chooseSaveFile(initialDirectory: Option[Path], suggestedFileName: Option[String]): IO[Option[Path]] =
      IO.pure(saveSelection)

  private def createStateManager(
    sessionRootOverride: Option[Path] = None,
    fileDialog: FileDialog = FileDialog.unavailable
  ): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerCoreCommandsSpec"))
    StateManager.apply(logger, sessionRootOverride = sessionRootOverride, fileDialog = fileDialog).unsafeRunSync()

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

  it should "open the find modal for the find-all command" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "find-all", "find-all")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = updatedState.modalSurface

    updatedState.commandRunnerSurface shouldBe None
    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.Find("", Nil, 0)))
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

  it should "open the replace-all workflow with the bulk action selected" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "replace-all", "replace-all")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val modalSurface = updatedState.modalSurface

    updatedState.commandRunnerSurface shouldBe None
    modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(
        Modal.ReplaceWorkflow(
          com.serenity.state.models.ReplaceWorkflowState(selectedAction = ReplaceWorkflowAction.ReplaceAll)
        )
      )
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

  it should "enable spell-checking from the command runner and refresh diagnostics immediately" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.buffers(bufferId).copy(content = com.serenity.rope.Rope("wurld"))
        state.copy(
          buffers = state.buffers + (bufferId -> buffer),
          config = state.config.withSpellCheck(SpellCheckConfig(enabled = false))
        )
      }
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().diagnostics shouldBe empty

    executeCommandThroughRunner(stateManager, "spellcheck-on", "spellcheck-on")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val diagnostics  = updatedState.diagnostics.getOrElse(SpellChecker.bufferDiagnosticsUri(bufferId), Nil)

    updatedState.config.spellCheck.enabled shouldBe true
    diagnostics.map(_.message) shouldBe List("Possible spelling issue: wurld")
  }

  it should "save the focused buffer through the native save-as file dialog" in {
    val targetPath   = Files.createTempDirectory("serenity-save-as").resolve("notes-copy.scala")
    val stateManager = createStateManager(fileDialog = TestFileDialog(saveSelection = Some(targetPath)))
    val bufferId     = BufferId(0)
    val filePath     = Path.of("temp", "notes.scala")

    stateManager
      .updateState { state =>
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("saved through dialog"),
            filePath = Some(filePath),
            isDirty = true
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "save-as", "save-as")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.modalSurface shouldBe None
    updatedState.buffers(bufferId).filePath shouldBe Some(targetPath)
    updatedState.buffers(bufferId).isDirty shouldBe false
    Files.readString(targetPath) shouldBe "saved through dialog"
  }

  it should "open a selected file through the native open-file dialog" in {
    val sourcePath = Files.createTempDirectory("serenity-open").resolve("notes.md")
    Files.writeString(sourcePath, "# Notes")
    val stateManager = createStateManager(fileDialog = TestFileDialog(openSelection = Some(sourcePath)))

    executeCommandThroughRunner(stateManager, "open", "open")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.modalSurface shouldBe None
    val openedBuffer = updatedState.buffers.values.find(_.filePath.contains(sourcePath))
    openedBuffer.map(_.content.collect()) shouldBe Some("# Notes")
    openedBuffer.flatMap(_.language) shouldBe Some(LanguageId.Markdown)
  }

  it should "save an unsaved buffer through the native save-as file dialog" in {
    val targetPath   = Files.createTempDirectory("serenity-unsaved-save").resolve("draft.txt")
    val stateManager = createStateManager(fileDialog = TestFileDialog(saveSelection = Some(targetPath)))

    stateManager.updateBuffer(BufferId(0), "draft body").unsafeRunSync()

    executeCommandThroughRunner(stateManager, "save", "save")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.modalSurface shouldBe None
    updatedState.buffers(BufferId(0)).filePath shouldBe Some(targetPath)
    updatedState.buffers(BufferId(0)).isDirty shouldBe false
    Files.readString(targetPath) shouldBe "draft body"
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

  it should "pin Markdown headings in the outline panel from the command runner" in {
    val stateManager = createStateManager()

    stateManager
      .updateState { state =>
        val bufferId = BufferId(0)
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("# Chapter One\n\nBody\n\n## Scene Two"),
            language = Some(LanguageId.Markdown)
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "pin-outline", "pin-outline")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val outlineSymbols = updatedState.pinnedSurfaces.collectFirst {
      case UiSurface(
            _,
            SurfaceContent.Outline(symbols, activeLocation),
            SurfacePresentation.Pinned(PanelPosition.Right, 30),
            _
          ) =>
        symbols -> activeLocation
    }

    outlineSymbols shouldBe Some(
      List(
        Symbol("Chapter One", SymbolKind.Heading, Location(0, 0)),
        Symbol("Scene Two", SymbolKind.Heading, Location(4, 0))
      ) -> Some(Location(0, 0))
    )
  }

  it should "navigate to the next Markdown heading from the command runner" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("# Chapter One\n\nBody\n\n## Scene Two\n\nText\n\n### Beat Three"),
            language = Some(LanguageId.Markdown),
            cursors = List(CursorPosition(1, 2))
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-document-symbol", "next-document-symbol")

    val updatedBuffer = stateManager.getCurrentState.unsafeRunSync().buffers(bufferId)
    updatedBuffer.cursors shouldBe List(CursorPosition(4, 0))
    updatedBuffer.selection shouldBe None
    updatedBuffer.selections shouldBe Nil
  }

  it should "animate the target buffer after document symbol navigation" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope(
              "# Chapter One\n\nBody\n\nMore\n\nStill more\n\nEven more\n\n## Scene Two\n\nText"
            ),
            language = Some(LanguageId.Markdown),
            cursors = List(CursorPosition(1, 2)),
            viewport = Viewport.default.copy(visibleLines = 4, visibleColumns = 40)
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-document-symbol", "next-document-symbol")

    val updatedBuffer = stateManager.getCurrentState.unsafeRunSync().buffers(bufferId)
    updatedBuffer.cursors shouldBe List(CursorPosition(10, 0))
    updatedBuffer.viewport.topLine should be > 0
    updatedBuffer.animations.activeAnimationCount should be > 0
  }

  it should "navigate to the previous Markdown heading from a command" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("# Chapter One\n\nBody\n\n## Scene Two"),
            language = Some(LanguageId.Markdown),
            cursors = List(CursorPosition(0, 0))
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    stateManager
      .executeCommand(
        Command.typed(
          "previous-document-symbol",
          "Go to the previous document symbol.",
          CommandIntent.PreviousDocumentSymbol,
          CommandCategory.View
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().buffers(bufferId).cursors shouldBe List(CursorPosition(4, 0))
  }

  it should "navigate between plaintext sections from the command runner" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("Opening\nbody\n\nSecond\nbody\n\nThird"),
            language = None,
            cursors = List(CursorPosition(1, 0))
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-document-symbol", "next-document-symbol")

    stateManager.getCurrentState.unsafeRunSync().buffers(bufferId).cursors shouldBe List(CursorPosition(3, 0))
  }

  it should "toggle a bookmark at the active cursor from the command runner" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state.buffers(bufferId).copy(cursors = List(CursorPosition(2, 4)))
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "toggle-bookmark", "toggle-bookmark")

    stateManager.getCurrentState.unsafeRunSync().buffers(bufferId).bookmarks shouldBe List(CursorPosition(2, 4))

    executeCommandThroughRunner(stateManager, "toggle-bookmark", "toggle-bookmark")

    stateManager.getCurrentState.unsafeRunSync().buffers(bufferId).bookmarks shouldBe Nil
  }

  it should "navigate between explicit bookmarks from command runner commands" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state
          .buffers(bufferId)
          .copy(
            cursors = List(CursorPosition(2, 0)),
            bookmarks = List(CursorPosition(0, 3), CursorPosition(4, 1))
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-bookmark", "next-bookmark")

    stateManager.getCurrentState.unsafeRunSync().buffers(bufferId).cursors shouldBe List(CursorPosition(4, 1))

    executeCommandThroughRunner(stateManager, "previous-bookmark", "previous-bookmark")

    stateManager.getCurrentState.unsafeRunSync().buffers(bufferId).cursors shouldBe List(CursorPosition(0, 3))
  }

  it should "animate the target buffer after bookmark navigation" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha\nbravo\ncharlie\ndelta\necho"),
            cursors = List(CursorPosition(1, 0)),
            bookmarks = List(CursorPosition(4, 1)),
            viewport = Viewport.default.copy(visibleLines = 8, visibleColumns = 40)
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-bookmark", "next-bookmark")

    val updatedBuffer = stateManager.getCurrentState.unsafeRunSync().buffers(bufferId)
    updatedBuffer.cursors shouldBe List(CursorPosition(4, 1))
    updatedBuffer.animations.activeAnimationCount should be > 0
  }

  it should "record document jumps in navigation history and move backward and forward" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("# Chapter One\n\nBody\n\n## Scene Two"),
            language = Some(LanguageId.Markdown),
            cursors = List(CursorPosition(1, 2))
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-document-symbol", "next-document-symbol")

    val afterJump = stateManager.getCurrentState.unsafeRunSync()
    afterJump.buffers(bufferId).cursors shouldBe List(CursorPosition(4, 0))
    afterJump.navigationBackStack shouldBe List(NavigationPoint(PaneId(0), bufferId, CursorPosition(1, 2)))
    afterJump.navigationForwardStack shouldBe Nil

    executeCommandThroughRunner(stateManager, "navigate-back", "navigate-back")

    val afterBack = stateManager.getCurrentState.unsafeRunSync()
    afterBack.buffers(bufferId).cursors shouldBe List(CursorPosition(1, 2))
    afterBack.navigationBackStack shouldBe Nil
    afterBack.navigationForwardStack shouldBe List(NavigationPoint(PaneId(0), bufferId, CursorPosition(4, 0)))

    executeCommandThroughRunner(stateManager, "navigate-forward", "navigate-forward")

    val afterForward = stateManager.getCurrentState.unsafeRunSync()
    afterForward.buffers(bufferId).cursors shouldBe List(CursorPosition(4, 0))
    afterForward.navigationBackStack shouldBe List(NavigationPoint(PaneId(0), bufferId, CursorPosition(1, 2)))
    afterForward.navigationForwardStack shouldBe Nil
  }

  it should "include explicit bookmarks in the outline panel" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("# Chapter One\n\nBody\n\n## Scene Two"),
            language = Some(LanguageId.Markdown),
            cursors = List(CursorPosition(2, 4)),
            bookmarks = List(CursorPosition(2, 4))
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "pin-outline", "pin-outline")

    val outlineSymbols = stateManager.getCurrentState.unsafeRunSync().pinnedSurfaces.collectFirst {
      case UiSurface(_, SurfaceContent.Outline(symbols, _), SurfacePresentation.Pinned(PanelPosition.Right, 30), _) =>
        symbols
    }

    outlineSymbols shouldBe Some(
      List(
        Symbol("Chapter One", SymbolKind.Heading, Location(0, 0)),
        Symbol("Bookmark 3:5", SymbolKind.Bookmark, Location(2, 4)),
        Symbol("Scene Two", SymbolKind.Heading, Location(4, 0))
      )
    )
  }

  it should "leave the cursor unchanged when document symbol navigation has no target" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("# Plain text only"),
            cursors = List(CursorPosition(0, 7))
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "next-document-symbol", "next-document-symbol")

    stateManager.getCurrentState.unsafeRunSync().buffers(bufferId).cursors shouldBe List(CursorPosition(0, 7))
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

  it should "toggle a cursor-attached comment lens for the active comment" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer = state
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("val x = 1\n// **Review** this value"),
            language = Some(LanguageId.Scala),
            cursors = List(CursorPosition(1, 3))
          )
        state.copy(buffers = state.buffers + (bufferId -> buffer))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "comment-lens", "comment-lens")

    val shownState = stateManager.getCurrentState.unsafeRunSync()
    val lens = shownState.commentLensSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommentLens(comment) => Some(comment)
          case _                                   => None
      }
      .getOrElse(fail("Expected comment lens"))

    lens.raw shouldBe "// **Review** this value"
    lens.inlineMarkdown shouldBe "Review this value"

    stateManager
      .executeCommand(
        Command.typed(
          "comment-lens",
          "Toggle comment lens.",
          CommandIntent.ToggleCommentLens,
          CommandCategory.View
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().commentLensSurface shouldBe None
  }

  it should "save, restore, and clear the current session from command runner commands" in {
    val sessionRoot  = Files.createTempDirectory("serenity-command-session")
    val stateManager = createStateManager(Some(sessionRoot))
    val bufferId     = BufferId(0)

    stateManager.updateBuffer(bufferId, "saved session").unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().buffers(bufferId).isNewEmpty shouldBe false

    executeCommandThroughRunner(stateManager, "save-session", "save-session")
    stateManager.sessionExists.unsafeRunSync() shouldBe true

    stateManager.updateBuffer(bufferId, "changed session").unsafeRunSync()

    executeCommandThroughRunner(stateManager, "restore-session", "restore-session")
    stateManager.getCurrentState.unsafeRunSync().buffers(bufferId).content.collect() shouldBe "saved session"

    executeCommandThroughRunner(stateManager, "clear-session", "clear-session")
    stateManager.sessionExists.unsafeRunSync() shouldBe false
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
