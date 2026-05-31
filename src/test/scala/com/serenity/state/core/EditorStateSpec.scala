package com.serenity.state.core

import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId}
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
    val focusedFirst = EditorState.focusBuffer(EditorState.rebalancePanes(withThreeBuffers, Some(BufferId(0))), BufferId(0))

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
