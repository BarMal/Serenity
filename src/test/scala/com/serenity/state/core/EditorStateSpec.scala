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
