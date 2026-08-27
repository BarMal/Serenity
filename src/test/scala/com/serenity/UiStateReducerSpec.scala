package com.serenity

import java.nio.file.Paths

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{ModalStateReducer, PanelStateReducer, PeekStateReducer}
import com.serenity.ui.layout.*
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
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.EditorPane(paneId)
      ),
      runtime = AppState.initial.runtime.copy(nextBufferId = BufferId(2))
    )

  "ModalStateReducer" should "show and dismiss modals while preserving editor focus fallback" in {
    val shown = ModalStateReducer.show(Modal.GotoLine("12"), baseState)
    val modalSurface =
      shown.state.runtime.uiSurfaces.find(_.content == SurfaceContent.ModalWorkflow(Modal.GotoLine("12")))

    modalSurface shouldBe defined
    shown.state.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)

    val dismissed = ModalStateReducer.dismiss(shown.state)
    dismissed.state.runtime.uiSurfaces
      .exists(_.content == SurfaceContent.ModalWorkflow(Modal.GotoLine("12"))) shouldBe false
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
            .copy(editing = baseState.persisted.buffers(bufferId).editing.copy(cursors = List(cursor)))
        )
      )
    )

    val findShown    = ModalStateReducer.show(Modal.Find("", Nil, 0), state).state
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

  it should "track custom modal names through focus typing" in {
    val shown = ModalStateReducer.show(Modal.Custom("signature-help", "map("), baseState)
    val modalSurface =
      shown.state.runtime.uiSurfaces
        .find(_.content == SurfaceContent.ModalWorkflow(Modal.Custom("signature-help", "map(")))

    modalSurface shouldBe defined
    shown.state.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  "PeekStateReducer" should "show and dismiss peek overlays while preserving editor focus fallback" in {
    val overlay     = PeekContent.QuickInfo("signature")
    val shown       = PeekStateReducer.show(overlay, CursorPosition(3, 4), baseState)
    val peekSurface = shown.state.runtime.uiSurfaces.find(_.content == SurfaceContent.QuickInfo("signature"))

    peekSurface shouldBe defined
    shown.state.persisted.focus shouldBe Focus.Surface(peekSurface.get.id)
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
    val pinnedSurface =
      pinned.state.runtime.uiSurfaces.find(_.presentation == SurfacePresentation.Pinned(PanelPosition.Left, 24))
    pinnedSurface shouldBe defined
    pinnedSurface.get.content shouldBe SurfaceContent.DirectoryTree(DirectoryTreeData(Paths.get("/tmp")), None)

    val focused = PanelStateReducer.focus(PanelPosition.Left, pinned.state)
    focused.state.persisted.focus shouldBe Focus.Surface(pinnedSurface.get.id)

    val unpinned = PanelStateReducer.unpin(PanelPosition.Left, focused.state)
    unpinned.state.runtime.uiSurfaces.exists(
      _.presentation == SurfacePresentation.Pinned(PanelPosition.Left, 24)
    ) shouldBe false
    unpinned.state.persisted.focus shouldBe Focus.EditorPane(paneId)
  }

  it should "expand and collapse a pinned panel without losing its original position and size" in {
    val content = PanelContent.Diagnostics(List(Diagnostic("broken", DiagnosticSeverity.Error, Location(2, 4))))
    val pinned  = PanelStateReducer.pin(content, PanelPosition.Right, 28, baseState).state
    val panelId = pinned.pinnedSurfaces.head.id

    val expanded = PanelStateReducer.expand(PanelPosition.Right, pinned).state

    expanded.surfaceById(panelId).map(_.presentation) shouldBe Some(
      SurfacePresentation.Pinned(PanelPosition.Right, 28)
    )
    expanded.persisted.focus shouldBe Focus.Surface(panelId)
    expanded.pinnedSurfaces.map(_.id) shouldBe List(panelId)
    expanded.persisted.layout.maximizedWorkspaceNodeId shouldBe
      expanded.persisted.layout.workspaceTree.flatMap(_.nodeIdForSurface(panelId))

    val collapsed = PanelStateReducer.collapseExpandedPanel(expanded).state

    collapsed.surfaceById(panelId).map(_.presentation) shouldBe Some(
      SurfacePresentation.Pinned(PanelPosition.Right, 28)
    )
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

    val pinnedSurface =
      pinned.state.runtime.uiSurfaces.find(_.presentation == SurfacePresentation.Pinned(PanelPosition.Right, 30))
    pinnedSurface shouldBe defined
    pinned.state.persisted.focus shouldBe Focus.Surface(pinnedSurface.get.id)
    pinnedSurface.get.id shouldBe surface.id
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

    val pinnedSurface =
      pinned.state.runtime.uiSurfaces.find(_.presentation == SurfacePresentation.Pinned(PanelPosition.Left, 30))
    pinnedSurface shouldBe defined
    pinned.state.persisted.focus shouldBe Focus.Surface(pinnedSurface.get.id)
    pinnedSurface.get.id shouldBe surface.id
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
          searchTerm = "open",
          selectedIndex = 0,
          filteredCommands = List.empty
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
