package com.serenity

import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** TDD specs for #1203's status-bar element: real paint-call assertions (not just state), following the
  * `GutterAndLineNumbersSpec` convention of asserting against `MockRenderSurface.drawRunPxCalls`.
  */
class WordCountStatusBarSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def stateWithBuffer(text: String, wordCountEnabled: Boolean, selection: Option[Selection] = None): AppState =
    val buffer0 = Buffer.fromString(BufferId(1), text)
    val buffer  = buffer0.copy(editing = buffer0.editing.copy(selection = selection))
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        ),
        focus = Focus.EditorPane(PaneId(0)),
        theme = Theme.light,
        config = AppState.initial.persisted.config.withWordCount(wordCountEnabled)
      )
    )

  "the status bar" should "paint the whole-buffer word and character count when enabled" in {
    val state    = stateWithBuffer("hello brave new world", wordCountEnabled = true)
    val surface  = new MockRenderSurface(100, 24)
    val viewport = ViewportSize(100, 24)

    Renderer.render(state, cursorVisible = true, surface, viewport)

    surface.drawRunPxCalls.map(_.s).mkString should include("4 words")
    surface.drawRunPxCalls.map(_.s).mkString should include("21 chars")
  }

  it should "not paint a word count when the feature is off" in {
    val state    = stateWithBuffer("hello brave new world", wordCountEnabled = false)
    val surface  = new MockRenderSurface(100, 24)
    val viewport = ViewportSize(100, 24)

    Renderer.render(state, cursorVisible = true, surface, viewport)

    surface.drawRunPxCalls.map(_.s).mkString should not include "words"
  }

  it should "paint a selection-scoped count when a non-empty selection is active" in {
    val selection = Selection(CursorPosition(0, 0), CursorPosition(0, 5)) // "hello"
    val state     = stateWithBuffer("hello brave new world", wordCountEnabled = true, selection = Some(selection))
    val surface   = new MockRenderSurface(100, 24)
    val viewport  = ViewportSize(100, 24)

    Renderer.render(state, cursorVisible = true, surface, viewport)

    surface.drawRunPxCalls.map(_.s).mkString should include("1 of 4 words selected")
  }

  it should "fall back to the whole-buffer count when the selection is empty" in {
    val selection = Selection(CursorPosition(0, 3), CursorPosition(0, 3))
    val state     = stateWithBuffer("hello brave new world", wordCountEnabled = true, selection = Some(selection))
    val surface   = new MockRenderSurface(100, 24)
    val viewport  = ViewportSize(100, 24)

    Renderer.render(state, cursorVisible = true, surface, viewport)

    surface.drawRunPxCalls.map(_.s).mkString should include("4 words")
    surface.drawRunPxCalls.map(_.s).mkString should not include "selected"
  }
