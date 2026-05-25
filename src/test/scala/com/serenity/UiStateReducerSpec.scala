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
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      nextBufferId = BufferId(2)
    )

  "ModalStateReducer" should "show and dismiss modals while preserving editor focus fallback" in {
    val shown = ModalStateReducer.show(Modal.GotoLine("12"), baseState)

    shown.state.modal shouldBe Some(Modal.GotoLine("12"))
    shown.state.focus shouldBe Focus.Modal(ModalType.GotoLine)

    val dismissed = ModalStateReducer.dismiss(shown.state)
    dismissed.state.modal shouldBe None
    dismissed.state.focus shouldBe Focus.EditorPane(paneId)
  }

  "PeekStateReducer" should "show and dismiss peek overlays while preserving editor focus fallback" in {
    val overlay = PeekContent.QuickInfo("signature")
    val shown   = PeekStateReducer.show(overlay, CursorPosition(3, 4), baseState)

    shown.state.peekOverlay shouldBe Some(PeekOverlay(overlay, CursorPosition(3, 4)))
    shown.state.focus shouldBe Focus.PeekOverlay

    val dismissed = PeekStateReducer.dismiss(shown.state)
    dismissed.state.peekOverlay shouldBe None
    dismissed.state.focus shouldBe Focus.EditorPane(paneId)
  }

  "PanelStateReducer" should "pin, focus, and unpin panels consistently" in {
    val content = PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/tmp")), None)

    val pinned = PanelStateReducer.pin(content, PanelPosition.Left, 24, baseState)
    pinned.state.layout.pinnedPanels(PanelPosition.Left) shouldBe PinnedPanel(PanelPosition.Left, content, 24)

    val focused = PanelStateReducer.focus(PanelPosition.Left, pinned.state)
    focused.state.focus shouldBe Focus.PinnedPanel(PanelPosition.Left)

    val unpinned = PanelStateReducer.unpin(PanelPosition.Left, focused.state)
    unpinned.state.layout.pinnedPanels.contains(PanelPosition.Left) shouldBe false
    unpinned.state.focus shouldBe Focus.EditorPane(paneId)
  }

  it should "pin a directory listing from a peek overlay" in {
    val peekState = baseState.copy(
      focus = Focus.PeekOverlay,
      peekOverlay = Some(
        PeekOverlay(
          PeekContent.DirectoryListing(
            Paths.get("/repo"),
            List(DirEntry(Paths.get("/repo/src"), "src", true))
          ),
          CursorPosition(0, 0)
        )
      )
    )

    val pinned = PanelStateReducer.pinPeekOverlay(PanelPosition.Right, peekState)

    pinned.state.focus shouldBe Focus.PinnedPanel(PanelPosition.Right)
    pinned.state.peekOverlay shouldBe None
    pinned.state.layout.pinnedPanels(PanelPosition.Right).content shouldBe
      PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/repo")), Some(Paths.get("/repo")))
  }

