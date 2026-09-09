package com.serenity.state.core

import com.serenity.config.{AppConfig, DefaultDocumentMode}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorStateSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "EditorState.openNewTab" should "create a new empty buffer, insert it into order, and focus it" in {
    val updatedState = EditorState
      .openNewTab(
        AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(200, 24))))
      )

    updatedState.persisted.buffers should have size 2
    updatedState.persisted.bufferOrder shouldBe List(BufferId(0), BufferId(1))
    updatedState.focusedBufferId shouldBe Some(BufferId(1))
    updatedState.persisted.buffers(BufferId(1)).document.isNewEmpty shouldBe true
  }

  it should "create new Markdown buffers when configured as the default document mode" in {
    val updatedState = EditorState.openNewTab(
      AppState.initial.copy(
        runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(200, 24))),
        persisted = AppState.initial.persisted
          .copy(config = AppConfig.default.withDefaultDocumentMode(DefaultDocumentMode.Markdown))
      )
    )

    val buffer = updatedState.persisted.buffers(BufferId(1))
    buffer.document.language shouldBe Some(LanguageId.Markdown)
    buffer.richText.richTextDocument shouldBe None
  }

  it should "create new rich text buffers when configured as the default document mode" in {
    val updatedState = EditorState.openNewTab(
      AppState.initial.copy(
        runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(200, 24))),
        persisted = AppState.initial.persisted
          .copy(config = AppConfig.default.withDefaultDocumentMode(DefaultDocumentMode.RichText))
      )
    )

    val buffer = updatedState.persisted.buffers(BufferId(1))
    buffer.document.language shouldBe None
    buffer.richText.richTextDocument.map(_.plainText) shouldBe Some("")
  }

  it should "insert a new buffer after the currently focused buffer" in {
    val withSecondBuffer = EditorState
      .openNewTab(
        AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(200, 24))))
      )
    val focusedFirst = EditorState.rebalancePanes(withSecondBuffer, Some(BufferId(0)))

    val updatedState = EditorState.openNewTab(focusedFirst)

    updatedState.persisted.bufferOrder shouldBe List(BufferId(0), BufferId(2), BufferId(1))
    updatedState.focusedBufferId shouldBe Some(BufferId(2))
  }

  "EditorState.navigateToNextBuffer" should "follow bufferOrder" in {
    val withThreeBuffers = EditorState.openNewTab(
      EditorState.openNewTab(
        AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(200, 24))))
      )
    )
    val focusedFirst =
      EditorState.focusBuffer(EditorState.rebalancePanes(withThreeBuffers, Some(BufferId(0))), BufferId(0))

    val updatedState = EditorState.navigateToNextBuffer(focusedFirst)

    updatedState.focusedBufferId shouldBe Some(BufferId(1))
  }

  "EditorState.navigateToPreviousBuffer" should "follow bufferOrder in reverse" in {
    val withThreeBuffers = EditorState.openNewTab(
      EditorState.openNewTab(
        AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(200, 24))))
      )
    )

    val updatedState = EditorState.navigateToPreviousBuffer(withThreeBuffers)

    updatedState.focusedBufferId shouldBe Some(BufferId(1))
  }

  "EditorState.removeBuffer" should "remove the buffer from state, order, and pane assignments" in {
    val initialState =
      EditorState
        .openNewTab(
          AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(200, 24))))
        )

    val updatedState = EditorState.removeBuffer(initialState, BufferId(1))

    updatedState.persisted.buffers should not contain key(BufferId(1))
    updatedState.persisted.bufferOrder shouldBe List(BufferId(0))
    updatedState.persisted.layout.editorPanes.values.flatMap(_.bufferId) shouldBe empty
  }

  "EditorState.removePane" should "remove the pane and focus the next available pane" in {
    val initialState =
      EditorState
        .openNewTab(
          AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(200, 24))))
        )

    val updatedState = EditorState.removePane(initialState, PaneId(1))

    updatedState.persisted.layout.editorPanes.keySet shouldBe Set(PaneId(0))
    updatedState.persisted.layout.activeEditorPaneId shouldBe Some(PaneId(0))
    updatedState.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  "EditorState.splitFocusedPane" should "split the focused pane, carrying its buffer into the new pane" in {
    import com.serenity.ui.layout.{SplitAxis, WorkspaceNode, WorkspaceNodeId}

    val updatedState = EditorState.splitFocusedPane(AppState.initial, SplitAxis.Horizontal)

    updatedState.persisted.layout.editorPanes.keySet shouldBe Set(PaneId(0), PaneId(1))
    updatedState.persisted.layout.editorPanes(PaneId(1)).bufferId shouldBe Some(BufferId(0))
    updatedState.persisted.layout.activeEditorPaneId shouldBe Some(PaneId(1))
    updatedState.persisted.focus shouldBe Focus.EditorPane(PaneId(1))
    updatedState.persisted.layout.paneOrder shouldBe List(PaneId(0), PaneId(1))
    updatedState.runtime.nextPaneId shouldBe PaneId(2)
    updatedState.persisted.layout.workspaceTree shouldBe Some(
      com.serenity.ui.layout.WorkspaceTree(
        WorkspaceNode.Split(
          WorkspaceNodeId("split-0-1"),
          SplitAxis.Horizontal,
          0.5,
          WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0)),
          WorkspaceNode.Leaf(WorkspaceNodeId("editor-1"), PaneId(1))
        )
      )
    )
  }

  it should "split along the requested axis" in {
    val updatedState = EditorState.splitFocusedPane(AppState.initial, com.serenity.ui.layout.SplitAxis.Vertical)

    updatedState.persisted.layout.workspaceTree.map(_.root.axis) shouldBe Some(
      Some(com.serenity.ui.layout.SplitAxis.Vertical)
    )
  }

  it should "create an empty new pane when the focused pane has no buffer" in {
    val emptyPaneState = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.empty(PaneId(0)))
        )
      )
    )

    val updatedState = EditorState.splitFocusedPane(emptyPaneState, com.serenity.ui.layout.SplitAxis.Horizontal)

    updatedState.persisted.layout.editorPanes(PaneId(1)).bufferId shouldBe None
  }

  it should "target the active pane when focus is on a surface rather than an editor pane" in {
    val (withSurfaceId, surfaceId) = AppState.initial.allocateSurfaceId
    val focusedOnSurface = withSurfaceId.copy(persisted = withSurfaceId.persisted.copy(focus = Focus.Surface(surfaceId)))

    val updatedState = EditorState.splitFocusedPane(focusedOnSurface, com.serenity.ui.layout.SplitAxis.Horizontal)

    updatedState.persisted.layout.editorPanes.keySet shouldBe Set(PaneId(0), PaneId(1))
    updatedState.persisted.layout.editorPanes(PaneId(1)).bufferId shouldBe Some(BufferId(0))
  }

  it should "leave state unchanged when there is no pane to target" in {
    val noPaneState = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        layout = AppState.initial.persisted.layout.copy(activeEditorPaneId = None),
        focus = Focus.Surface(SurfaceId("surface-999"))
      )
    )

    EditorState.splitFocusedPane(noPaneState, com.serenity.ui.layout.SplitAxis.Horizontal) shouldBe noPaneState
  }

  "EditorState.closeFocusedTab" should "keep a single pane and focus the remaining buffer when other tabs exist" in {
    val constrainedViewport = ViewportSize(80, 24)
    val withThreeBuffers = EditorState.openNewTab(
      EditorState.openNewTab(
        AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(constrainedViewport)))
      )
    )

    withThreeBuffers.persisted.layout.editorPanes should have size 1
    withThreeBuffers.focusedBufferId shouldBe Some(BufferId(2))

    val updatedState = EditorState.closeFocusedTab(withThreeBuffers)

    updatedState.persisted.buffers should not contain key(BufferId(2))
    updatedState.persisted.bufferOrder shouldBe List(BufferId(0), BufferId(1))
    updatedState.persisted.layout.editorPanes should have size 1
    updatedState.focusedBufferId shouldBe Some(BufferId(1))
  }

  it should "retain explicit pane topology when a focused tab closes" in {
    val wideViewport = ViewportSize(400, 24)
    val withThreeBuffers = EditorState.openNewTab(
      EditorState.openNewTab(
        AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(wideViewport)))
      )
    )
    val firstPane      = withThreeBuffers.persisted.layout.activeEditorPaneId.get
    val withSecondPane = addPane(withThreeBuffers, firstPane, PaneId(1), BufferId(0))
    val withThirdPane  = addPane(withSecondPane, PaneId(1), PaneId(2), BufferId(1))
    val withExplicitPanes = withThirdPane.copy(persisted =
      withThirdPane.persisted.copy(
        layout = withThirdPane.persisted.layout.copy(activeEditorPaneId = Some(firstPane)),
        focus = Focus.EditorPane(firstPane)
      )
    )

    withExplicitPanes.persisted.layout.editorPanes should have size 3

    val updatedState = EditorState.closeFocusedTab(withExplicitPanes)

    updatedState.persisted.buffers should not contain key(BufferId(2))
    updatedState.persisted.bufferOrder shouldBe List(BufferId(0), BufferId(1))
    updatedState.persisted.layout.editorPanes.keySet shouldBe Set(PaneId(0), PaneId(1), PaneId(2))
    updatedState.persisted.layout.activeEditorPaneId shouldBe Some(PaneId(2))
    updatedState.persisted.focus shouldBe Focus.EditorPane(PaneId(2))
  }

  private def addPane(state: AppState, after: PaneId, paneId: PaneId, bufferId: BufferId): AppState =
    val tree = state.persisted.layout.effectiveWorkspaceTree
      .flatMap(
        _.split(
          after,
          paneId,
          com.serenity.ui.layout.SplitAxis.Horizontal,
          com.serenity.ui.layout.WorkspaceNodeId(s"split-${after.value}-${paneId.value}"),
          com.serenity.ui.layout.WorkspaceNodeId(s"editor-${paneId.value}")
        )
      )
      .getOrElse(fail("expected workspace split"))
    state.copy(
      persisted = state.persisted.copy(
        layout = state.persisted.layout.copy(
          editorPanes = state.persisted.layout.editorPanes.updated(paneId, EditorPane.withBuffer(paneId, bufferId)),
          paneOrder = tree.paneIds,
          workspaceTree = Some(tree)
        )
      ),
      runtime = state.runtime.copy(nextPaneId = PaneId(paneId.value + 1))
    )
