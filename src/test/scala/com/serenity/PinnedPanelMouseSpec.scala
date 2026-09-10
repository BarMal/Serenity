package com.serenity

import java.nio.file.Paths

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.document.DocumentNavigation
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class PinnedPanelMouseSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport              = ViewportSize(100, 32)
  private val compactSquareViewport = ViewportSize(40, 12)

  private def makeStateManager() =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

  private def dockExplorer(
    sm: StateManager,
    id: SurfaceId,
    tree: DirectoryTreeData,
    selectedPath: Option[java.nio.file.Path]
  ): Unit =
    sm.updateState(state =>
      DockedPanelFixtures.dock(state, id, SurfaceContent.DirectoryTree(tree, selectedPath), PanelPosition.Left, 28)
    ).unsafeRunSync()

  private def dockExpandedExplorer(
    sm: StateManager,
    id: SurfaceId,
    tree: DirectoryTreeData,
    selectedPath: Option[java.nio.file.Path]
  ): Unit =
    sm.updateState(state =>
      DockedPanelFixtures.expand(
        DockedPanelFixtures.dock(state, id, SurfaceContent.DirectoryTree(tree, selectedPath), PanelPosition.Left, 28),
        id
      )
    ).unsafeRunSync()

  private def leftPanelContentRect(state: AppState): LayoutRect =
    panelContentRect(state, SurfaceId("explorer"))

  private def panelContentRect(state: AppState, surfaceId: SurfaceId): LayoutRect =
    val contract = panelContract(state)
    contract
      .panelContentRect(surfaceId)
      .getOrElse(fail(s"Expected panel content rect for ${surfaceId.value}"))

  private def panelContract(
    state: AppState,
    viewportSize: ViewportSize = viewport
  ): EditorLayoutContract =
    val layout = LayoutEngine.calculateLayoutWithUI(state, viewportSize)
    EditorLayoutContract.from(state, viewportSize, layout)

  private def panelItemPoint(
    state: AppState,
    surfaceId: SurfaceId,
    displayedItemRow: Int,
    viewportSize: ViewportSize = viewport
  ): (Int, Int) =
    val contract = panelContract(state, viewportSize)
    val contentRect = contract
      .panelContentRect(surfaceId)
      .getOrElse(fail(s"Expected panel content rect for ${surfaceId.value}"))
    val rowY = contract
      .panelRowSlots(surfaceId)
      .collectFirst { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(`displayedItemRow`), y) => y }
      .getOrElse(fail(s"Expected pinned panel row $displayedItemRow for ${surfaceId.value}"))
    (contentRect.x + 1, rowY)

  private def panelFrameRect(state: AppState, surfaceId: SurfaceId, viewportSize: ViewportSize = viewport): LayoutRect =
    panelContract(state, viewportSize)
      .panelRect(surfaceId)
      .getOrElse(fail(s"Expected panel frame rect for ${surfaceId.value}"))

  private def withActiveBuffer(sm: StateManager, text: String): BufferId =
    val bufferId = BufferId(42)
    val paneId   = PaneId(0)
    val buffer   = Buffer.fromString(bufferId, text)
    sm.updateState(state =>
      state.copy(persisted =
        state.persisted.copy(
          buffers = Map(bufferId -> buffer),
          bufferOrder = List(bufferId),
          layout = Layout(
            editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
            activeEditorPaneId = Some(paneId),
            workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
          ),
          focus = Focus.EditorPane(paneId)
        )
      )
    ).unsafeRunSync()
    bufferId

  "Pinned panel mouse support" should "select and focus a directory tree row on primary click" in {
    val root = Paths.get("/repo")
    val src  = root.resolve("src")
    val test = root.resolve("test")
    val tree = DirectoryTreeData(
      root,
      entries = Map(
        root -> List(
          DirEntry(src, "src", isDirectory = true),
          DirEntry(test, "test", isDirectory = true)
        )
      )
    )
    val surfaceId = SurfaceId("explorer")
    val sm        = makeStateManager()
    dockExplorer(sm, surfaceId, tree, selectedPath = Some(root))
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val point = panelItemPoint(sm.getCurrentState.unsafeRunSync(), surfaceId, displayedItemRow = 2)
    sm.applyEvent(MouseClick(point._1, point._2)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.persisted.focus shouldBe Focus.Surface(surfaceId)
    updated.surfaceById(surfaceId).map(_.content) shouldBe Some(SurfaceContent.DirectoryTree(tree, Some(test)))
  }

  it should "not route clicks or hover to a pinned panel while a close confirmation is active" in {
    val root    = Paths.get("/repo")
    val src     = root.resolve("src")
    val tree    = DirectoryTreeData(root, entries = Map(root -> List(DirEntry(src, "src", isDirectory = true))))
    val panelId = SurfaceId("explorer")
    val close = ModalDialog(
      SurfaceId("close-confirmation"),
      Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, BufferId(0), "notes.scala")),
      ModalPlacement.Centered
    )
    val sm = makeStateManager()
    dockExplorer(sm, panelId, tree, selectedPath = Some(root))
    sm.updateState(state =>
      state.copy(
        persisted = state.persisted.copy(focus = Focus.Modal),
        runtime = state.runtime.copy(modalStack = List(close))
      )
    ).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val point = panelItemPoint(sm.getCurrentState.unsafeRunSync(), panelId, displayedItemRow = 1)
    sm.applyEvent(MouseClick(point._1, point._2)).unsafeRunSync()
    sm.applyEvent(MouseMove(point._1, point._2)).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.persisted.focus shouldBe Focus.Modal
    after.surfaceById(panelId).map(_.content) shouldBe Some(SurfaceContent.DirectoryTree(tree, Some(root)))
  }

  it should "select and focus an expanded directory tree row on primary click" in {
    val root = Paths.get("/repo")
    val src  = root.resolve("src")
    val test = root.resolve("test")
    val tree = DirectoryTreeData(
      root,
      entries = Map(
        root -> List(
          DirEntry(src, "src", isDirectory = true),
          DirEntry(test, "test", isDirectory = true)
        )
      )
    )
    val surfaceId = SurfaceId("expanded-explorer")
    val sm        = makeStateManager()
    dockExpandedExplorer(sm, surfaceId, tree, selectedPath = Some(root))
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val point = panelItemPoint(sm.getCurrentState.unsafeRunSync(), surfaceId, displayedItemRow = 2)
    sm.applyEvent(MouseClick(point._1, point._2)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.persisted.focus shouldBe Focus.Surface(surfaceId)
    updated.surfaceById(surfaceId).map(_.content) shouldBe Some(SurfaceContent.DirectoryTree(tree, Some(test)))
  }

  it should "characterize expanded panel clicks against the scene node content region" in {
    val root      = Paths.get("/repo")
    val src       = root.resolve("src")
    val tree      = DirectoryTreeData(root, entries = Map(root -> List(DirEntry(src, "src", isDirectory = true))))
    val surfaceId = SurfaceId("expanded-explorer")
    val sm        = makeStateManager()
    dockExpandedExplorer(sm, surfaceId, tree, selectedPath = Some(root))
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val scene = UiSceneSnapshot.from(state, viewport)
    val node = scene.workspace
      .find(_.id == SceneNodeId.Surface(surfaceId))
      .getOrElse(fail("expected expanded surface node"))
    val content = node.hitRegions
      .collectFirst { case SceneHitRegion(SceneHitKind.Content, rect) => rect }
      .getOrElse(fail("expected expanded content hit region"))
    val row = scene.editorContract
      .panelRowSlots(surfaceId)
      .collectFirst { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), y) => index -> y }
      .getOrElse(fail("expected expanded content row"))

    content.contains(content.x, row._2) shouldBe true
    node.frameRect.containsRect(content) shouldBe true
  }

  it should "select a directory tree row on hover without stealing focus" in {
    val root = Paths.get("/repo")
    val src  = root.resolve("src")
    val tree = DirectoryTreeData(
      root,
      entries = Map(root -> List(DirEntry(src, "src", isDirectory = true)))
    )
    val surfaceId = SurfaceId("explorer")
    val sm        = makeStateManager()
    dockExplorer(sm, surfaceId, tree, selectedPath = Some(root))
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val before = sm.getCurrentState.unsafeRunSync()
    val point  = panelItemPoint(before, surfaceId, displayedItemRow = 1)
    sm.applyEvent(MouseMove(point._1, point._2)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.persisted.focus shouldBe before.persisted.focus
    updated.surfaceById(surfaceId).map(_.content) shouldBe Some(SurfaceContent.DirectoryTree(tree, Some(src)))
  }

  it should "activate a double-clicked loaded directory tree row" in {
    val root = Paths.get("/repo")
    val src  = root.resolve("src")
    val tree = DirectoryTreeData(
      root,
      entries = Map(
        root -> List(DirEntry(src, "src", isDirectory = true)),
        src  -> List(DirEntry(src.resolve("Main.scala"), "Main.scala", isDirectory = false))
      )
    )
    val surfaceId = SurfaceId("explorer")
    val sm        = makeStateManager()
    dockExplorer(sm, surfaceId, tree, selectedPath = Some(root))
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val point = panelItemPoint(sm.getCurrentState.unsafeRunSync(), surfaceId, displayedItemRow = 1)
    sm.applyEvent(MouseClick(point._1, point._2, clickCount = 2)).unsafeRunSync()

    val expandedTree = tree.copy(expandedPaths = Set(src))
    val updated      = sm.getCurrentState.unsafeRunSync()
    updated.persisted.focus shouldBe Focus.Surface(surfaceId)
    updated.surfaceById(surfaceId).map(_.content) shouldBe Some(SurfaceContent.DirectoryTree(expandedTree, Some(src)))
  }

  it should "navigate to an outline row on primary click" in {
    val sm       = makeStateManager()
    val bufferId = withActiveBuffer(sm, "intro\nmiddle\nend")
    val symbols = List(
      Symbol("Intro", SymbolKind.Heading, Location(0, 0)),
      Symbol("Middle", SymbolKind.Heading, Location(1, 2))
    )
    val surfaceId = SurfaceId("outline")
    sm.updateState(state =>
      DockedPanelFixtures.dock(state, surfaceId, SurfaceContent.Outline(symbols), PanelPosition.Right, 28)
    ).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val point = panelItemPoint(sm.getCurrentState.unsafeRunSync(), surfaceId, displayedItemRow = 1)
    sm.applyEvent(MouseClick(point._1, point._2)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    updated.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(1, 2))
  }

  it should "highlight an outline row on hover without stealing focus" in {
    val sm = makeStateManager()
    withActiveBuffer(sm, "intro\nmiddle\nend")
    val symbols = List(
      Symbol("Intro", SymbolKind.Heading, Location(0, 0)),
      Symbol("Middle", SymbolKind.Heading, Location(1, 2))
    )
    val surfaceId = SurfaceId("outline")
    sm.updateState(state =>
      DockedPanelFixtures.dock(
        state,
        surfaceId,
        SurfaceContent.Outline(symbols, Some(Location(0, 0))),
        PanelPosition.Right,
        28
      )
    ).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val before = sm.getCurrentState.unsafeRunSync()
    val point  = panelItemPoint(before, surfaceId, displayedItemRow = 1)
    sm.applyEvent(MouseMove(point._1, point._2)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.persisted.focus shouldBe before.persisted.focus
    updated.surfaceById(surfaceId).map(_.content) shouldBe Some(SurfaceContent.Outline(symbols, Some(Location(1, 2))))
  }

  it should "navigate to a comments row on primary click and open the comment lens" in {
    val sm       = makeStateManager()
    val bufferId = withActiveBuffer(sm, "intro\nmiddle\nend")
    val comment  = DocumentComment(CursorPosition(1, 0), CursorPosition(1, 6), "Tighten this")
    sm.updateState(state =>
      state
        .copy(persisted =
          state.persisted.copy(buffers =
            state.persisted.buffers.updated(
              bufferId,
              state.persisted
                .buffers(bufferId)
                .copy(annotations =
                  state.persisted.buffers(bufferId).annotations.copy(documentComments = List(comment))
                )
            )
          )
        )
    ).unsafeRunSync()

    val symbols   = DocumentNavigation.commentSymbols(List(comment))
    val surfaceId = SurfaceId("comments")
    sm.updateState(state =>
      DockedPanelFixtures.dock(state, surfaceId, SurfaceContent.Comments(symbols), PanelPosition.Right, 28)
    ).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val point = panelItemPoint(sm.getCurrentState.unsafeRunSync(), surfaceId, displayedItemRow = 0)
    sm.applyEvent(MouseClick(point._1, point._2)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(1, 0))

    val lensSurface = updated.commentLensSurface.getOrElse(fail("Expected the comment lens to open"))
    val lensState = lensSurface.content match
      case SurfaceContent.CommentLens(lens) => lens
      case other                            => fail(s"Expected CommentLens content, got $other")
    lensState.draft shouldBe "Tighten this"
    lensState.target shouldBe Some(comment)
    updated.persisted.focus shouldBe Focus.Surface(lensSurface.id)
  }

  it should "highlight a comments row on hover without stealing focus" in {
    val sm       = makeStateManager()
    val bufferId = withActiveBuffer(sm, "intro\nmiddle\nend")
    val comment  = DocumentComment(CursorPosition(1, 0), CursorPosition(1, 6), "Tighten this")
    sm.updateState(state =>
      state
        .copy(persisted =
          state.persisted.copy(buffers =
            state.persisted.buffers.updated(
              bufferId,
              state.persisted
                .buffers(bufferId)
                .copy(annotations =
                  state.persisted.buffers(bufferId).annotations.copy(documentComments = List(comment))
                )
            )
          )
        )
    ).unsafeRunSync()

    val symbols   = DocumentNavigation.commentSymbols(List(comment))
    val surfaceId = SurfaceId("comments")
    sm.updateState(state =>
      DockedPanelFixtures.dock(state, surfaceId, SurfaceContent.Comments(symbols), PanelPosition.Right, 28)
    ).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val before = sm.getCurrentState.unsafeRunSync()
    val point  = panelItemPoint(before, surfaceId, displayedItemRow = 0)
    sm.applyEvent(MouseMove(point._1, point._2)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.persisted.focus shouldBe before.persisted.focus
    updated.surfaceById(surfaceId).map(_.content) shouldBe Some(SurfaceContent.Comments(symbols, Some(Location(1, 0))))
  }

  it should "navigate to a diagnostics row on primary click" in {
    val sm       = makeStateManager()
    val bufferId = withActiveBuffer(sm, "first\nsecond\nthird")
    val issues = List(
      Diagnostic("unused import", DiagnosticSeverity.Warning, Location(0, 1)),
      Diagnostic("type mismatch", DiagnosticSeverity.Error, Location(2, 3))
    )
    val surfaceId = SurfaceId("diagnostics")
    sm.updateState(state =>
      DockedPanelFixtures.dock(state, surfaceId, SurfaceContent.Diagnostics(issues), PanelPosition.Left, 28)
    ).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val point = panelItemPoint(sm.getCurrentState.unsafeRunSync(), surfaceId, displayedItemRow = 2)
    sm.applyEvent(MouseClick(point._1, point._2)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    updated.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(2, 3))
  }

  it should "highlight a diagnostics row on hover without stealing focus" in {
    val sm = makeStateManager()
    withActiveBuffer(sm, "first\nsecond\nthird")
    val issues = List(
      Diagnostic("unused import", DiagnosticSeverity.Warning, Location(0, 1)),
      Diagnostic("type mismatch", DiagnosticSeverity.Error, Location(2, 3))
    )
    val surfaceId = SurfaceId("diagnostics")
    sm.updateState(state =>
      DockedPanelFixtures.dock(state, surfaceId, SurfaceContent.Diagnostics(issues), PanelPosition.Left, 28)
    ).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val before = sm.getCurrentState.unsafeRunSync()
    val point  = panelItemPoint(before, surfaceId, displayedItemRow = 2)
    sm.applyEvent(MouseMove(point._1, point._2)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.persisted.focus shouldBe before.persisted.focus
    updated.surfaceById(surfaceId).map(_.content) shouldBe Some(
      SurfaceContent.Diagnostics(issues, Some(Location(2, 3)))
    )
  }

  it should "navigate to rendered diagnostics rows when frame and content layout kinds disagree" in {
    val sm       = makeStateManager()
    val bufferId = withActiveBuffer(sm, "first\nsecond\nthird")
    val issues = List(
      Diagnostic("unused import", DiagnosticSeverity.Warning, Location(0, 1)),
      Diagnostic("type mismatch", DiagnosticSeverity.Error, Location(2, 3))
    )
    val surfaceId = SurfaceId("diagnostics")
    sm.updateState { state =>
      // Seed the panel's ratio against the viewport it will actually render at (issue #817: the ratio is the sole
      // size record), so `SurfaceLayoutKind.classify` below sees the requested 18-cell width, not a mismatched
      // fallback total.
      val withViewport = state.copy(runtime = state.runtime.copy(viewportSize = Some(compactSquareViewport)))
      val docked =
        DockedPanelFixtures.dock(withViewport, surfaceId, SurfaceContent.Diagnostics(issues), PanelPosition.Right, 18)
      docked
        .copy(persisted = docked.persisted.copy(config = AppConfig.default.withLineNumbers(false).withGutter(false)))
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(compactSquareViewport)).unsafeRunSync()

    val state       = sm.getCurrentState.unsafeRunSync()
    val frameRect   = panelFrameRect(state, surfaceId, compactSquareViewport)
    val contentRect = panelContract(state, compactSquareViewport).panelContentRect(surfaceId).get

    SurfaceLayoutKind.classify(frameRect) shouldBe SurfaceLayoutKind.Square
    SurfaceLayoutKind.classify(contentRect) shouldBe SurfaceLayoutKind.Compact

    val point = panelItemPoint(state, surfaceId, displayedItemRow = 1, viewportSize = compactSquareViewport)
    sm.applyEvent(MouseClick(point._1, point._2)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    updated.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, 1))
  }

  it should "not navigate from blank rows in a horizontal bottom diagnostics panel" in {
    val sm       = makeStateManager()
    val bufferId = withActiveBuffer(sm, "first\nsecond\nthird")
    val issues = List(
      Diagnostic("unused import", DiagnosticSeverity.Warning, Location(0, 1)),
      Diagnostic("type mismatch", DiagnosticSeverity.Error, Location(2, 3))
    )
    val surfaceId = SurfaceId("diagnostics")
    sm.updateState { state =>
      val withViewport = state.copy(runtime = state.runtime.copy(viewportSize = Some(viewport)))
      DockedPanelFixtures.dock(withViewport, surfaceId, SurfaceContent.Diagnostics(issues), PanelPosition.Bottom, 10)
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val rect = panelContentRect(sm.getCurrentState.unsafeRunSync(), surfaceId)
    SurfaceLayoutKind.classify(rect) shouldBe SurfaceLayoutKind.Horizontal

    sm.applyEvent(MouseClick(rect.x + 1, rect.y + 1)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    updated.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, 0))
  }

  it should "update a pinned panel size from mouse drag before release" in {
    val root = Paths.get("/repo")
    val src  = root.resolve("src")
    val tree = DirectoryTreeData(
      root,
      entries = Map(root -> List(DirEntry(src, "src", isDirectory = true)))
    )
    val surfaceId = SurfaceId("explorer")
    val sm        = makeStateManager()
    dockExplorer(sm, surfaceId, tree, selectedPath = Some(root))
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val before       = sm.getCurrentState.unsafeRunSync()
    val beforeLayout = LayoutEngine.calculateLayoutWithUI(before, viewport)
    val beforeRect   = beforeLayout.pinnedPanelRects(PanelPosition.Left)
    val dragColumn   = beforeRect.right - 6

    sm.applyEvent(MouseDrag(dragColumn, beforeRect.y + 2)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.surfaceById(surfaceId).getOrElse(fail("Expected resized pinned panel"))
    val afterLayout = LayoutEngine.calculateLayoutWithUI(updated, viewport)
    val updatedSize = afterLayout.pinnedPanelRects(PanelPosition.Left).width

    updatedSize shouldBe dragColumn + 1
    afterLayout.editorPanelRect.x should be < beforeLayout.editorPanelRect.x
  }
