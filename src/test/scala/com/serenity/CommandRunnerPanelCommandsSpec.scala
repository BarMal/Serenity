package com.serenity

import java.nio.file.Path

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.io.{FileDialog, FileUtils}
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CommandRunnerPanelCommandsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def isPinnedAt(state: AppState, surface: UiSurface, position: PanelPosition): Boolean =
    surface.presentation == SurfacePresentation.Docked &&
      state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id)).contains(position)

  private def createStateManager(
    sessionRootOverride: Option[Path] = None,
    configPersistencePath: Option[Path] = None,
    fileDialog: Option[FileDialog] = None
  ): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerPanelCommandsSpec"))
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

  "Command runner" should "pin the explorer panel from the command runner" in {
    val stateManager     = createStateManager()
    val currentDirectory = FileUtils.getCurrentDirectory.unsafeRunSync()

    executeCommandThroughRunner(stateManager, "pin-explorer", "pin-explorer")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    val pinnedSurface = updatedState.pinnedSurfaces
      .collectFirst {
        case surface @ com.serenity.state.models.UiSurface(_, SurfaceContent.DirectoryTree(tree, _), _, _)
            if isPinnedAt(updatedState, surface, PanelPosition.Left) =>
          surface -> tree.rootPath
      }
      .getOrElse(fail("Expected pinned explorer surface"))

    pinnedSurface._2 shouldBe currentDirectory
  }

  it should "pin the outline panel from the command runner" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "pin-outline", "pin-outline")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.pinnedSurfaces.exists(isPinnedAt(updatedState, _, PanelPosition.Right)) shouldBe true
    updatedState.pinnedSurfaces.exists(_.content == SurfaceContent.Outline(Nil)) shouldBe true
  }

  it should "cap animation cells generated for a very tall pinned panel open" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(state => state.copy(runtime = state.runtime.copy(viewportSize = Some(ViewportSize(80, 3000)))))
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "pin-outline", "pin-outline")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val surfaceId    = updatedState.pinnedSurfaces.find(_.content == SurfaceContent.Outline(Nil)).get.id
    // +1 for the single fixed border/frame cell, which is separate from the capped content cells.
    updatedState.runtime.surfaceAnimations(surfaceId).animationState.animations.size should be <=
      com.serenity.state.manager.VisibleBufferAnimationCells.DefaultMaxAnimatedCells + 1
  }

  it should "pin the comments panel from the command runner" in {
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
              .copy(content = com.serenity.rope.Rope("Opening paragraph\nSecond paragraph")),
            annotations = state.persisted
              .buffers(bufferId)
              .annotations
              .copy(
                documentComments = List(DocumentComment(CursorPosition(1, 0), CursorPosition(1, 6), "Tighten this"))
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "pin-comments", "pin-comments")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.pinnedSurfaces.exists(isPinnedAt(updatedState, _, PanelPosition.Right)) shouldBe true
    val commentSymbols = updatedState.pinnedSurfaces.collectFirst {
      case surface @ UiSurface(_, SurfaceContent.Comments(symbols, _), _, _)
          if isPinnedAt(updatedState, surface, PanelPosition.Right) =>
        symbols
    }
    commentSymbols shouldBe Some(
      List(Symbol("Comment: Tighten this", SymbolKind.Comment, Location(1, 0)))
    )
  }

  it should "pin Markdown headings in the outline panel from the command runner" in {
    val stateManager = createStateManager()

    stateManager
      .updateState { state =>
        val bufferId = BufferId(0)
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("# Chapter One\n\nBody\n\n## Scene Two"),
                language = Some(LanguageId.Markdown)
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "pin-outline", "pin-outline")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val outlineSymbols = updatedState.pinnedSurfaces.collectFirst {
      case surface @ UiSurface(_, SurfaceContent.Outline(symbols, activeLocation), _, _)
          if isPinnedAt(updatedState, surface, PanelPosition.Right) =>
        symbols -> activeLocation
    }

    outlineSymbols shouldBe Some(
      List(
        Symbol("Chapter One", SymbolKind.Heading, Location(0, 0)),
        Symbol("Scene Two", SymbolKind.Heading, Location(4, 0))
      ) -> Some(Location(0, 0))
    )
  }

  it should "include explicit bookmarks but not document comments in the outline panel" in {
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
                content = com.serenity.rope.Rope("# Chapter One\n\nBody\n\n## Scene Two"),
                language = Some(LanguageId.Markdown)
              ),
            editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(2, 4))),
            annotations = state.persisted
              .buffers(bufferId)
              .annotations
              .copy(
                bookmarks = List(CursorPosition(2, 4)),
                documentComments = List(DocumentComment(CursorPosition(3, 0), CursorPosition(3, 3), "Revise bridge"))
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "pin-outline", "pin-outline")

    val currentState = stateManager.getCurrentState.unsafeRunSync()
    val outlineSymbols = currentState.pinnedSurfaces.collectFirst {
      case surface @ UiSurface(_, SurfaceContent.Outline(symbols, _), _, _)
          if isPinnedAt(currentState, surface, PanelPosition.Right) =>
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

  it should "pin the diagnostics panel from the command runner" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "pin-diagnostics", "pin-diagnostics")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.pinnedSurfaces.exists(isPinnedAt(updatedState, _, PanelPosition.Bottom)) shouldBe true
    updatedState.pinnedSurfaces.exists(_.content == SurfaceContent.Diagnostics(Nil)) shouldBe true
  }

  it should "pin a right-side Markdown preview for the active Markdown buffer" in {
    val stateManager = createStateManager()

    stateManager
      .updateState { state =>
        val bufferId = BufferId(0)
        val buffer = state.persisted
          .buffers(bufferId)
          .copy(
            document = state.persisted
              .buffers(bufferId)
              .document
              .copy(
                content = com.serenity.rope.Rope("# Notes\n\n![Diagram](diagram.png)"),
                language = Some(LanguageId.Markdown)
              )
          )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
      }
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "markdown-preview", "markdown-preview")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    val preview = updatedState.pinnedSurfaces.collectFirst {
      case surface @ UiSurface(_, SurfaceContent.MarkdownPreview(BufferId(0), "Untitled"), _, _)
          if isPinnedAt(updatedState, surface, PanelPosition.Right) =>
        surface
    }

    preview should not be empty
    updatedState.persisted.buffers(BufferId(0)).document.content.collect() shouldBe "# Notes\n\n![Diagram](diagram.png)"
  }

  it should "leave the workspace unchanged when Markdown preview is requested for a non-Markdown buffer" in {
    val stateManager = createStateManager()
    val before       = stateManager.getCurrentState.unsafeRunSync()

    executeCommandThroughRunner(stateManager, "markdown-preview", "markdown-preview")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.pinnedSurfaces shouldBe before.pinnedSurfaces
  }

  it should "set a typed panel pin without disturbing other pinned panels" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "panel-diagnostics-pin-bottom",
          "Pin diagnostics at the bottom.",
          CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Bottom))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "panel-outline-pin-right",
          "Pin outline on the right.",
          CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "panel-outline-pin-left",
          "Move outline to the left.",
          CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Left))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    val tree         = updatedState.persisted.layout.workspaceTree
    updatedState.pinnedSurfaces.collect {
      case surface @ UiSurface(_, SurfaceContent.Outline(_, _), _, _) =>
        tree.flatMap(_.positionForSurface(surface.id))
    }.flatten shouldBe List(PanelPosition.Left)
    updatedState.pinnedSurfaces.collect {
      case surface @ UiSurface(_, SurfaceContent.Diagnostics(_, _), _, _) =>
        tree.flatMap(_.positionForSurface(surface.id))
    }.flatten shouldBe List(PanelPosition.Bottom)
  }

  it should "turn off a typed panel pin without removing other pinned panels" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "panel-outline-pin-right",
          "Pin outline on the right.",
          CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "panel-diagnostics-pin-bottom",
          "Pin diagnostics at the bottom.",
          CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Bottom))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "panel-outline-pin-off",
          "Hide outline panel.",
          CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Outline, None)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.pinnedSurfaces.exists(_.content.isInstanceOf[SurfaceContent.Outline]) shouldBe false
    updatedState.pinnedSurfaces.exists(_.content.isInstanceOf[SurfaceContent.Diagnostics]) shouldBe true
  }

  it should "focus the left panel from the command runner" in {
    val sm = createStateManager()
    sm.panelManager.loadDirectoryTree(FileUtils.getCurrentDirectory.unsafeRunSync(), List("src")).unsafeRunSync()

    executeCommandThroughRunner(sm, "focus-left-panel", "focus-left-panel")

    val updatedState = sm.getCurrentState.unsafeRunSync()
    updatedState.persisted.focus shouldBe a[Focus.Surface]
    val focusedId = updatedState.persisted.focus match
      case Focus.Surface(id) => id
      case other             => fail(s"Expected focus on a surface, got $other")
    updatedState.pinnedSurfaces.map(_.id) should contain(focusedId)
  }

  it should "unpin the left panel from the command runner" in {
    val sm = createStateManager()
    sm.panelManager.loadDirectoryTree(FileUtils.getCurrentDirectory.unsafeRunSync(), List("src")).unsafeRunSync()

    executeCommandThroughRunner(sm, "unpin-left-panel", "unpin-left-panel")

    val updatedState = sm.getCurrentState.unsafeRunSync()
    updatedState.pinnedSurfaces.exists(isPinnedAt(updatedState, _, PanelPosition.Left)) shouldBe false
  }
