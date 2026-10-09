package com.serenity

import java.nio.file.Paths

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalStateReducer, PanelStateReducer, PeekStateReducer, UndoEffect}
import com.serenity.state.undo.{EditGrouping, HistoryEntry}
import com.serenity.ui.layout.*
import com.serenity.ui.widget.TextField
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UiStateReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId = PaneId(0)

  private def baseState: AppState =
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "hello")
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      ),
      runtime = AppState.initial.runtime.copy(nextBufferId = BufferId(2))
    )

  "ModalStateReducer" should "show and dismiss modals while preserving editor focus fallback" in {
    val shown = ModalStateReducer.show(Modal.TextPrompt(TextPrompt.gotoLine("12")), baseState)
    val modalSurface =
      shown.state.runtime.uiSurfaces
        .find(_.content == SurfaceContent.ModalWorkflow(Modal.TextPrompt(TextPrompt.gotoLine("12"))))

    modalSurface shouldBe defined
    shown.state.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)

    val dismissed = ModalStateReducer.dismiss(shown.state)
    dismissed.state.runtime.uiSurfaces
      .exists(_.content == SurfaceContent.ModalWorkflow(Modal.TextPrompt(TextPrompt.gotoLine("12")))) shouldBe false
    dismissed.state.persisted.focus shouldBe Focus.EditorPane(paneId)
  }

  it should "anchor find and replace modals below the active cursor" in {
    val bufferId = BufferId(1)
    val cursor   = CursorPosition(0, 3)
    val state = baseState.copy(
      persisted = baseState.persisted.copy(
        buffers = baseState.persisted.buffers.updated(
          bufferId,
          baseState.persisted
            .buffers(bufferId)
            .copy(editing = EditingState(List(cursor)))
        )
      )
    )

    val findShown    = ModalStateReducer.show(Modal.Find(TextField.of(""), Vector.empty, 0), state).state
    val findSurface  = findShown.modalSurface.getOrElse(fail("Expected find modal surface"))
    val replaceShown = ModalStateReducer.show(Modal.ReplaceWorkflow(ReplaceWorkflowState()), state).state
    val replaceSurface =
      replaceShown.modalSurface.getOrElse(fail("Expected replace modal surface"))

    findSurface.presentation shouldBe SurfacePresentation.Floating(
      Some(cursor),
      SurfacePlacement.BelowCursor
    )
    replaceSurface.presentation shouldBe SurfacePresentation.Floating(
      Some(cursor),
      SurfacePlacement.BelowCursor
    )
  }

  it should "focus a modeless modal workflow once it is shown" in {
    val prompt = Modal.TextPrompt(TextPrompt.gotoLine("12"))
    val shown  = ModalStateReducer.show(prompt, baseState)
    val modalSurface =
      shown.state.runtime.uiSurfaces
        .find(_.content == SurfaceContent.ModalWorkflow(prompt))

    modalSurface shouldBe defined
    shown.state.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  "PeekStateReducer" should "show and dismiss peek overlays while preserving editor focus fallback" in {
    val overlay     = PeekContent.QuickInfo("signature")
    val shown       = PeekStateReducer.show(overlay, CursorPosition(3, 4), baseState)
    val peekSurface = shown.state.runtime.uiSurfaces.find(_.content == SurfaceContent.QuickInfo("signature"))

    peekSurface shouldBe defined
    shown.state.persisted.focus shouldBe Focus.EditorPane(paneId)
    peekSurface.get.presentation shouldBe SurfacePresentation.Floating(
      Some(CursorPosition(3, 4)),
      SurfacePlacement.AboveCursor
    )

    val dismissed = PeekStateReducer.dismiss(shown.state)
    dismissed.state.runtime.uiSurfaces.exists(_.content == SurfaceContent.QuickInfo("signature")) shouldBe false
    dismissed.state.persisted.focus shouldBe Focus.EditorPane(paneId)
  }

  "PanelStateReducer" should "pin, focus, and unpin panels consistently" in {
    val content = PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/tmp")), None)

    val pinned = PanelStateReducer.pin(content, PanelPosition.Left, 24, baseState)
    val pinnedSurface = pinned.state.pinnedSurfaces.find(surface =>
      pinned.state.persisted.layout.workspaceTree
        .flatMap(_.positionForSurface(surface.id))
        .contains(
          PanelPosition.Left
        )
    )
    pinnedSurface shouldBe defined
    pinnedSurface.get.content shouldBe SurfaceContent.DirectoryTree(DirectoryTreeData(Paths.get("/tmp")), None)
    PanelStateReducer.currentSize(pinnedSurface.get.id, pinned.state) shouldBe Some(24)

    val focused = PanelStateReducer.focus(PanelPosition.Left, pinned.state)
    focused.state.persisted.focus shouldBe Focus.Surface(pinnedSurface.get.id)

    val unpinned = PanelStateReducer.unpin(PanelPosition.Left, focused.state)
    unpinned.state.pinnedSurfaces shouldBe empty
    unpinned.state.persisted.focus shouldBe Focus.EditorPane(paneId)
  }

  it should "dock a registered panel under its fixed surface id" in {
    val pinned = PanelStateReducer.pin(PanelContent.Outline(Nil), PanelPosition.Right, 24, baseState).state

    pinned.pinnedSurfaces.map(_.id) shouldBe List(PanelId.Outline.surfaceId)
    pinned.persisted.layout.workspaceTree.flatMap(_.positionForSurface(PanelId.Outline.surfaceId)) shouldBe
      Some(PanelPosition.Right)
  }

  it should "replace a registered panel that is already docked rather than docking a second one" in {
    val first  = PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/tmp")), None)
    val second = PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/srv")), None)
    val once   = PanelStateReducer.pin(first, PanelPosition.Left, 24, baseState).state
    val focusedOnce =
      once.copy(persisted = once.persisted.copy(focus = Focus.Surface(PanelId.Explorer.surfaceId)))

    val twice = PanelStateReducer.pin(second, PanelPosition.Right, 24, focusedOnce).state

    AppStateValidation.validationErrors(twice) shouldBe Nil
    twice.pinnedSurfaces.map(_.content) shouldBe List(second.asSurfaceContent)
    twice.persisted.layout.workspaceTree.flatMap(_.positionForSurface(PanelId.Explorer.surfaceId)) shouldBe
      Some(PanelPosition.Right)
    twice.persisted.focus shouldBe Focus.Surface(PanelId.Explorer.surfaceId)
  }

  it should "dock project output under its fixed surface id too" in {
    val pinned = PanelStateReducer.pin(PanelContent.Terminal("done", 0), PanelPosition.Bottom, 12, baseState).state

    pinned.pinnedSurfaces.map(_.id) shouldBe List(PanelId.ProjectOutput.surfaceId)
  }

  it should "declare an undo boundary for pin and unpin (#1016 PR4), capturing the pre-change state" in {
    val content = PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/tmp")), None)

    val pinned = PanelStateReducer.pin(content, PanelPosition.Left, 24, baseState)
    pinned.effects shouldBe List(
      AppEffect.Undo(UndoEffect.RecordBoundary(HistoryEntry.PanelChange.capture(baseState), EditGrouping.Standalone))
    )

    val unpinned = PanelStateReducer.unpin(PanelPosition.Left, pinned.state)
    unpinned.effects shouldBe List(
      AppEffect.Undo(UndoEffect.RecordBoundary(HistoryEntry.PanelChange.capture(pinned.state), EditGrouping.Standalone))
    )
  }

  it should "expand and collapse a pinned panel without losing its original position and size" in {
    val content = PanelContent.Diagnostics(List(Diagnostic("broken", DiagnosticSeverity.Error, Location(2, 4))))
    val pinned  = PanelStateReducer.pin(content, PanelPosition.Right, 28, baseState).state
    val panelId = pinned.pinnedSurfaces.head.id

    val expanded = PanelStateReducer.expand(PanelPosition.Right, pinned).state

    expanded.surfaceById(panelId).map(_.presentation) shouldBe Some(SurfacePresentation.Docked)
    expanded.persisted.layout.workspaceTree.flatMap(_.positionForSurface(panelId)) shouldBe Some(PanelPosition.Right)
    PanelStateReducer.currentSize(panelId, expanded) shouldBe Some(28)
    expanded.persisted.focus shouldBe Focus.Surface(panelId)
    expanded.pinnedSurfaces.map(_.id) shouldBe List(panelId)
    expanded.persisted.layout.maximizedWorkspaceNodeId shouldBe
      expanded.persisted.layout.workspaceTree.flatMap(_.nodeIdForSurface(panelId))

    val collapsed = PanelStateReducer.collapseExpandedPanel(expanded).state

    collapsed.surfaceById(panelId).map(_.presentation) shouldBe Some(SurfacePresentation.Docked)
    collapsed.persisted.layout.workspaceTree.flatMap(_.positionForSurface(panelId)) shouldBe Some(PanelPosition.Right)
    PanelStateReducer.currentSize(panelId, collapsed) shouldBe Some(28)
    collapsed.persisted.layout.maximizedWorkspaceNodeId shouldBe None
    collapsed.persisted.focus shouldBe Focus.Surface(panelId)
  }

  it should "restore editor focus when collapsing an expanded panel that is not focused" in {
    val content       = PanelContent.Outline(Nil)
    val pinned        = PanelStateReducer.pin(content, PanelPosition.Left, 20, baseState).state
    val expandedState = PanelStateReducer.expand(PanelPosition.Left, pinned).state
    val expanded      = expandedState.copy(persisted = expandedState.persisted.copy(focus = Focus.EditorPane(paneId)))

    val collapsed = PanelStateReducer.collapseExpandedPanel(expanded).state

    collapsed.persisted.focus shouldBe Focus.EditorPane(paneId)
  }

  it should "pin a directory listing from a peek overlay" in {
    val surface = UiSurface(
      SurfaceId("peek-directory"),
      SurfaceContent.DirectoryListing(
        Paths.get("/repo"),
        List(DirEntry(Paths.get("/repo/src"), "src", true)),
        None
      ),
      SurfacePresentation.Floating(Some(CursorPosition(0, 0)), SurfacePlacement.AboveCursor),
      dismissOnMove = true
    )
    val peekState = baseState.copy(
      persisted = baseState.persisted.copy(focus = Focus.Surface(surface.id)),
      runtime = baseState.runtime.copy(uiSurfaces = List(surface))
    )

    val pinned = PanelStateReducer.pinPeekOverlay(PanelPosition.Right, peekState)

    val pinnedSurface = pinned.state.pinnedSurfaces.find(surface =>
      pinned.state.persisted.layout.workspaceTree
        .flatMap(_.positionForSurface(surface.id))
        .contains(
          PanelPosition.Right
        )
    )
    pinnedSurface shouldBe defined
    pinned.state.persisted.focus shouldBe Focus.Surface(pinnedSurface.get.id)
    pinnedSurface.get.id shouldBe surface.id
    PanelStateReducer.currentSize(pinnedSurface.get.id, pinned.state) shouldBe Some(30)
    pinnedSurface.get.content shouldBe
      SurfaceContent.DirectoryTree(
        DirectoryTreeData(
          Paths.get("/repo"),
          entries = Map(Paths.get("/repo") -> List(DirEntry(Paths.get("/repo/src"), "src", true)))
        ),
        Some(Paths.get("/repo"))
      )
  }

  it should "pin the active floating surface when it is pinnable" in {
    val surface = UiSurface(
      SurfaceId("active-directory"),
      SurfaceContent.DirectoryListing(
        Paths.get("/repo"),
        List(DirEntry(Paths.get("/repo/src"), "src", true)),
        None
      ),
      SurfacePresentation.Floating(Some(CursorPosition(0, 0)), SurfacePlacement.AboveCursor),
      dismissOnMove = true
    )
    val floatingState = baseState.copy(
      persisted = baseState.persisted.copy(focus = Focus.Surface(surface.id)),
      runtime = baseState.runtime.copy(uiSurfaces = List(surface))
    )

    val pinned = PanelStateReducer.pinActiveFloatingSurface(PanelPosition.Left, floatingState)

    val pinnedSurface = pinned.state.pinnedSurfaces.find(surface =>
      pinned.state.persisted.layout.workspaceTree
        .flatMap(_.positionForSurface(surface.id))
        .contains(
          PanelPosition.Left
        )
    )
    pinnedSurface shouldBe defined
    pinned.state.persisted.focus shouldBe Focus.Surface(pinnedSurface.get.id)
    pinnedSurface.get.id shouldBe surface.id
    PanelStateReducer.currentSize(pinnedSurface.get.id, pinned.state) shouldBe Some(30)
    pinnedSurface.get.content shouldBe
      SurfaceContent.DirectoryTree(
        DirectoryTreeData(
          Paths.get("/repo"),
          entries = Map(Paths.get("/repo") -> List(DirEntry(Paths.get("/repo/src"), "src", true)))
        ),
        Some(Paths.get("/repo"))
      )
  }

  it should "leave unsupported floating surfaces unpinned" in {
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(
        com.serenity.command.CommandRunner(
          isActive = true,
          surface = com.serenity.command.CommandRunnerSurface.Palette(
            com.serenity.command.CommandPaletteState(searchTerm = "open")
          )
        )
      ),
      SurfacePresentation.Floating(Some(CursorPosition(0, 0)), SurfacePlacement.BelowCursor)
    )
    val unsupported = baseState.copy(
      persisted = baseState.persisted.copy(focus = Focus.Surface(surface.id)),
      runtime = baseState.runtime.copy(uiSurfaces = List(surface))
    )

    val result = PanelStateReducer.pinActiveFloatingSurface(PanelPosition.Bottom, unsupported)

    result.state.runtime.uiSurfaces shouldBe List(surface)
    result.state.persisted.focus shouldBe Focus.Surface(surface.id)
  }

  "PanelStateReducer.currentSize" should "report a pinned surface's current size" in {
    val content = PanelContent.Outline(Nil)
    val pinned  = PanelStateReducer.pin(content, PanelPosition.Right, 28, baseState).state
    val panelId = pinned.pinnedSurfaces.head.id

    PanelStateReducer.currentSize(panelId, pinned) shouldBe Some(28)
  }

  it should "report nothing for a surface id that isn't pinned" in {
    PanelStateReducer.currentSize(SurfaceId("missing"), baseState) shouldBe None
  }

  it should "reflect a resize applied through PanelStateReducer.resize" in {
    val content = PanelContent.Outline(Nil)
    val pinned  = PanelStateReducer.pin(content, PanelPosition.Right, 28, baseState).state
    val panelId = pinned.pinnedSurfaces.head.id

    val resized = PanelStateReducer.resize(panelId, 35, pinned).state

    PanelStateReducer.currentSize(panelId, resized) shouldBe Some(35)
  }
