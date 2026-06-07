package com.serenity.state.core

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorStateSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "EditorState.openNewTab" should "create a new empty buffer, insert it into order, and focus it" in {
    val updatedState = EditorState
      .openNewTab(AppState.initial.copy(viewportSize = Some(ViewportSize(200, 24))))

    updatedState.buffers should have size 2
    updatedState.bufferOrder shouldBe List(BufferId(0), BufferId(1))
    updatedState.focusedBufferId shouldBe Some(BufferId(1))
    updatedState.buffers(BufferId(1)).isNewEmpty shouldBe true
  }

  it should "insert a new buffer after the currently focused buffer" in {
    val withSecondBuffer = EditorState
      .openNewTab(AppState.initial.copy(viewportSize = Some(ViewportSize(200, 24))))
    val focusedFirst = EditorState.focusBuffer(withSecondBuffer, BufferId(0))

    val updatedState = EditorState.openNewTab(focusedFirst)

    updatedState.bufferOrder shouldBe List(BufferId(0), BufferId(2), BufferId(1))
    updatedState.focusedBufferId shouldBe Some(BufferId(2))
  }

  "EditorState.navigateToNextBuffer" should "follow bufferOrder" in {
    val withThreeBuffers = EditorState.openNewTab(
      EditorState.openNewTab(AppState.initial.copy(viewportSize = Some(ViewportSize(200, 24))))
    )
    val focusedFirst =
      EditorState.focusBuffer(EditorState.rebalancePanes(withThreeBuffers, Some(BufferId(0))), BufferId(0))

    val updatedState = EditorState.navigateToNextBuffer(focusedFirst)

    updatedState.focusedBufferId shouldBe Some(BufferId(1))
  }

  "EditorState.navigateToPreviousBuffer" should "follow bufferOrder in reverse" in {
    val withThreeBuffers = EditorState.openNewTab(
      EditorState.openNewTab(AppState.initial.copy(viewportSize = Some(ViewportSize(200, 24))))
    )

    val updatedState = EditorState.navigateToPreviousBuffer(withThreeBuffers)

    updatedState.focusedBufferId shouldBe Some(BufferId(1))
  }

  "EditorState.removeBuffer" should "remove the buffer from state, order, and pane assignments" in {
    val initialState =
      EditorState
        .openNewTab(AppState.initial.copy(viewportSize = Some(ViewportSize(200, 24))))

    val updatedState = EditorState.removeBuffer(initialState, BufferId(1))

    updatedState.buffers should not contain key(BufferId(1))
    updatedState.bufferOrder shouldBe List(BufferId(0))
    updatedState.layout.editorPanes.values.flatMap(_.bufferId) shouldBe List(Some(BufferId(0))).flatten
  }

  "EditorState.removePane" should "remove the pane and focus the next available pane" in {
    val initialState =
      EditorState
        .openNewTab(AppState.initial.copy(viewportSize = Some(ViewportSize(200, 24))))

    val updatedState = EditorState.removePane(initialState, PaneId(1))

    updatedState.layout.editorPanes.keySet shouldBe Set(PaneId(0))
    updatedState.layout.activeEditorPaneId shouldBe Some(PaneId(0))
    updatedState.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  "EditorState.closeFocusedTab" should "keep a single pane and focus the remaining buffer when other tabs exist" in {
    val constrainedViewport = ViewportSize(120, 24)
    val withThreeBuffers = EditorState.openNewTab(
      EditorState.openNewTab(AppState.initial.copy(viewportSize = Some(constrainedViewport)))
    )

    withThreeBuffers.layout.editorPanes should have size 1
    withThreeBuffers.focusedBufferId shouldBe Some(BufferId(2))

    val updatedState = EditorState.closeFocusedTab(withThreeBuffers)

    updatedState.buffers should not contain key(BufferId(2))
    updatedState.bufferOrder shouldBe List(BufferId(0), BufferId(1))
    updatedState.layout.editorPanes should have size 1
    updatedState.focusedBufferId shouldBe Some(BufferId(1))
  }

  it should "remove the focused pane when multiple panes are visible" in {
    val wideViewport = ViewportSize(400, 24)
    val withThreeBuffers = EditorState.openNewTab(
      EditorState.openNewTab(AppState.initial.copy(viewportSize = Some(wideViewport)))
    )

    withThreeBuffers.layout.editorPanes should have size 3
    withThreeBuffers.layout.activeEditorPaneId shouldBe Some(PaneId(2))

    val updatedState = EditorState.closeFocusedTab(withThreeBuffers)

    updatedState.buffers should not contain key(BufferId(2))
    updatedState.bufferOrder shouldBe List(BufferId(0), BufferId(1))
    updatedState.layout.editorPanes.keySet shouldBe Set(PaneId(0), PaneId(1))
    updatedState.layout.activeEditorPaneId shouldBe Some(PaneId(1))
    updatedState.focus shouldBe Focus.EditorPane(PaneId(1))
  }
