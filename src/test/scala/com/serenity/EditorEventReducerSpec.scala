package com.serenity

import com.serenity.keystroke.events.{DeleteBackward, InsertChar, MoveDown, MoveLeft, MoveRight, MoveToEnd, MoveToStart, NewLine, OpenGotoLine, Paste, ScrollDown}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId, CursorPosition, Focus, Modal, PaneId, SurfaceContent}
import com.serenity.state.reducers.EditorEventReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "EditorEventReducer" should "insert characters into the focused pane buffer" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)

    val updatedState = EditorEventReducer.reduce(InsertChar('x'), paneId, initialState).state
    val bufferId     = updatedState.layout.editorPanes(paneId).bufferId.get
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "x"
    buffer.cursors.head shouldBe com.serenity.state.models.CursorPosition(0, 1)
    buffer.isDirty shouldBe true
  }

  it should "insert characters at every cursor position when multiple cursors are active" in {
    val paneId       = PaneId(0)
    val bufferId     = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("abcd"),
          cursors = List(CursorPosition(0, 1), CursorPosition(0, 3))
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(InsertChar('X'), paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "aXbcXd"
    buffer.cursors shouldBe List(CursorPosition(0, 2), CursorPosition(0, 5))
  }

  it should "insert newlines at every cursor position when multiple cursors are active" in {
    val paneId       = PaneId(0)
    val bufferId     = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("abcd"),
          cursors = List(CursorPosition(0, 1), CursorPosition(0, 3))
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(NewLine, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "a\nbc\nd"
    buffer.cursors shouldBe List(CursorPosition(1, 0), CursorPosition(2, 0))
  }

  it should "delete backward at every cursor position when multiple cursors are active" in {
    val paneId       = PaneId(0)
    val bufferId     = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("1a2b3"),
          cursors = List(CursorPosition(0, 2), CursorPosition(0, 4))
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteBackward, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "123"
    buffer.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 2))
  }

  it should "paste clipboard content at every cursor position when multiple cursors are active" in {
    val paneId       = PaneId(0)
    val bufferId     = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("ab"),
          cursors = List(CursorPosition(0, 0), CursorPosition(0, 2))
        )
      ),
      clipboard = Some("Z")
    )

    val updatedState = EditorEventReducer.reduce(Paste, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "ZabZ"
    buffer.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 4))
  }

  it should "replace every active selection when inserting with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = com.serenity.state.models.Selection(CursorPosition(0, 0), CursorPosition(0, 3))
    val second   = com.serenity.state.models.Selection(CursorPosition(0, 8), CursorPosition(0, 11))
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("abc def ghi"),
          cursors = List(first.focus, second.focus),
          selection = Some(first),
          selections = List(first, second)
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(InsertChar('X'), paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "X def X"
    buffer.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 7))
    buffer.allSelections shouldBe Nil
  }

  it should "delete every active selection when delete backward is pressed with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = com.serenity.state.models.Selection(CursorPosition(0, 0), CursorPosition(0, 3))
    val second   = com.serenity.state.models.Selection(CursorPosition(0, 8), CursorPosition(0, 11))
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("abc def ghi"),
          cursors = List(first.focus, second.focus),
          selection = Some(first),
          selections = List(first, second)
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteBackward, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe " def "
    buffer.cursors shouldBe List(CursorPosition(0, 0), CursorPosition(0, 5))
    buffer.allSelections shouldBe Nil
  }

  it should "move every cursor left when multiple cursors are active" in {
    val paneId       = PaneId(0)
    val bufferId     = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("abcd"),
          cursors = List(CursorPosition(0, 2), CursorPosition(0, 4))
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveLeft, paneId, initialState).state
    updatedState.buffers(bufferId).cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 3))
  }

  it should "move every cursor right when multiple cursors are active" in {
    val paneId       = PaneId(0)
    val bufferId     = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("abcd"),
          cursors = List(CursorPosition(0, 0), CursorPosition(0, 2))
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveRight, paneId, initialState).state
    updatedState.buffers(bufferId).cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 3))
  }

  it should "move every cursor to line start when multiple cursors are active" in {
    val paneId       = PaneId(0)
    val bufferId     = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("abcd"),
          cursors = List(CursorPosition(0, 2), CursorPosition(0, 4))
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToStart, paneId, initialState).state
    updatedState.buffers(bufferId).cursors shouldBe List(CursorPosition(0, 0))
  }

  it should "move every cursor to line end when multiple cursors are active" in {
    val paneId       = PaneId(0)
    val bufferId     = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("abcd"),
          cursors = List(CursorPosition(0, 0), CursorPosition(0, 2))
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToEnd, paneId, initialState).state
    updatedState.buffers(bufferId).cursors shouldBe List(CursorPosition(0, 4))
  }

  it should "move every cursor down while preserving per-cursor columns when multiple cursors are active" in {
    val paneId       = PaneId(0)
    val bufferId     = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("abcd\nwxyz"),
          cursors = List(CursorPosition(0, 1), CursorPosition(0, 3))
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveDown, paneId, initialState).state
    updatedState.buffers(bufferId).cursors shouldBe List(CursorPosition(1, 1), CursorPosition(1, 3))
  }

  it should "update viewport position for scroll events" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)
    val bufferId     = initialState.layout.editorPanes(paneId).bufferId.get
    val seededState = initialState.copy(
      buffers = initialState.buffers.updated(
        bufferId,
        initialState.buffers(bufferId).copy(
          content = com.serenity.rope.Rope("a\nb\nc\nd\ne\nf\ng"),
          viewport = initialState.buffers(bufferId).viewport.copy(visibleLines = 2)
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(ScrollDown(3), paneId, seededState).state

    updatedState.buffers(bufferId).viewport.topLine shouldBe 3
  }

  it should "open the goto line modal from editor events" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)

    val updatedState = EditorEventReducer.reduce(OpenGotoLine, paneId, initialState).state
    val modalSurface = updatedState.modalSurface

    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("")))
    updatedState.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "provide a typed reducer instance for editor events" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)
    val reducer      = EditorEventReducer.reducer(paneId)

    val updatedState = reducer.reduce(OpenGotoLine, initialState).state
    val modalSurface = updatedState.modalSurface

    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("")))
    updatedState.focus shouldBe Focus.Surface(modalSurface.get.id)
  }
