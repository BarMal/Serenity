package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Cross-cutting coverage for a laziness guarantee that spans navigation, editing, and clipboard events alike: none
  * of them may call `Rope.collect()` on the whole buffer. Each test seeds the buffer with `NonCollectingRope`, a test
  * double whose `collect()` throws, so any code path that accidentally materialises the full document fails loudly
  * rather than merely being slow (extracted from the former monolithic `EditorEventReducerSpec`, #1442).
  */
class EditorNonMaterializingOperationsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "EditorEventReducer" should "select all without materialising the buffer" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted
                .buffers(bufferId)
                .document
                .copy(content = NonCollectingRope(Rope("alpha\nbeta"))),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 0)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(SelectAll, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.editing.selection shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
    buffer.editing.cursors shouldBe List(CursorPosition(1, 4))
  }

  it should "delete words for multiple cursors without materialising the whole buffer" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val content  = NonCollectingRope(Rope("alpha beta gamma"))
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted.buffers(bufferId).document.copy(content = content),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(CursorPosition(0, 8), CursorPosition(0, 10)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteWordBackward, paneId, initialState).state

    val buffer = updatedState.persisted.buffers(bufferId)
    buffer.document.content.collect() shouldBe "alpha  gamma"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 6))
  }

  it should "delete the previous word for a single cursor without materialising the whole buffer" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val content  = NonCollectingRope(Rope("alpha beta gamma"))
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted.buffers(bufferId).document.copy(content = content),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 10)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteWordBackward, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "alpha  gamma"
    buffer.editing.cursors shouldBe List(CursorPosition(0, 6))
  }

  it should "move horizontally without materialising a large single-line buffer" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val content  = NonCollectingRope(Rope("{" + (1 to 5000).map(i => s""""k$i":$i""").mkString(",") + "}"))
    val initialBuffer = AppState.initial.persisted
      .buffers(bufferId)
      .copy(
        document = AppState.initial.persisted.buffers(bufferId).document.copy(content = content),
        editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 0))),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 120, visibleLines = 40)
      )
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(bufferId, initialBuffer),
        config = AppState.initial.persisted.config.withWordWrap(false)
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveRight, paneId, initialState).state

    updatedState.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, 1))
  }

  it should "move across line boundaries without materialising the whole buffer" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val content  = NonCollectingRope(Rope("alpha\n" + "x" * 5000))
    val initialBuffer = AppState.initial.persisted
      .buffers(bufferId)
      .copy(
        document = AppState.initial.persisted.buffers(bufferId).document.copy(content = content),
        editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(1, 0))),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 120, visibleLines = 40)
      )
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(bufferId, initialBuffer),
        config = AppState.initial.persisted.config.withWordWrap(false)
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveLeft, paneId, initialState).state

    updatedState.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(0, 5))
  }

  it should "move multiple cursors horizontally without materialising the whole buffer" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val content  = NonCollectingRope(Rope("alpha\n" + "x" * 5000))
    val initialBuffer = AppState.initial.persisted
      .buffers(bufferId)
      .copy(
        document = AppState.initial.persisted.buffers(bufferId).document.copy(content = content),
        editing = AppState.initial.persisted
          .buffers(bufferId)
          .editing
          .copy(cursors = List(CursorPosition(0, 5), CursorPosition(1, 0))),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 120, visibleLines = 40)
      )
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(bufferId, initialBuffer),
        config = AppState.initial.persisted.config.withWordWrap(false)
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveRight, paneId, initialState).state

    updatedState.persisted.buffers(bufferId).editing.cursors shouldBe List(CursorPosition(1, 0), CursorPosition(1, 1))
  }

  it should "copy the current line without materialising the whole buffer" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val content  = NonCollectingRope(Rope("alpha\n" + "x" * 5000))
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted.buffers(bufferId).document.copy(content = content),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 2)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(Copy, paneId, initialState).state

    updatedState.runtime.clipboard shouldBe Some("alpha")
  }

  it should "cut the current line without materialising the whole buffer" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val content  = NonCollectingRope(Rope("alpha\n" + "x" * 5000))
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted.buffers(bufferId).document.copy(content = content),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 2)))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(Cut, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    updatedState.runtime.clipboard shouldBe Some("alpha")
    buffer.document.content.collect() shouldBe "x" * 5000
    buffer.editing.cursors shouldBe List(CursorPosition(0, 0))
  }

  it should "replace a selection without materialising the whole buffer" in {
    val paneId    = PaneId(0)
    val bufferId  = BufferId(0)
    val selection = Selection(CursorPosition(0, 0), CursorPosition(0, 5))
    val content   = NonCollectingRope(Rope("alpha\n" + "x" * 5000))
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted.buffers(bufferId).document.copy(content = content),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(selection.focus), selection = Some(selection))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(InsertChar('Z'), paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "Z\n" + ("x" * 5000)
    buffer.editing.cursors shouldBe List(CursorPosition(0, 1))
    buffer.allSelections shouldBe Nil
  }

  it should "copy multiple selections without materialising the whole buffer" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(0, 0), CursorPosition(0, 5))
    val second   = Selection(CursorPosition(1, 0), CursorPosition(1, 3))
    val content  = NonCollectingRope(Rope("alpha\nbeta\n" + "x" * 5000))
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted.buffers(bufferId).document.copy(content = content),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(
                  cursors = List(first.focus, second.focus),
                  selection = Some(first),
                  selections = List(first, second)
                )
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(Copy, paneId, initialState).state

    updatedState.runtime.clipboard shouldBe Some("alpha\nbet")
  }

  it should "indent selected lines without materialising the whole buffer" in {
    val paneId    = PaneId(0)
    val bufferId  = BufferId(0)
    val selection = Selection(CursorPosition(0, 0), CursorPosition(1, 2))
    val content   = NonCollectingRope(Rope("alpha\nbeta\n" + "x" * 5000))
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted.buffers(bufferId).document.copy(content = content),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(selection.focus), selection = Some(selection))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(TabKey, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.getLine(0) shouldBe Some("    alpha")
    buffer.document.content.getLine(1) shouldBe Some("    beta")
    buffer.document.content.getLine(2) shouldBe Some("x" * 5000)
    buffer.editing.cursors shouldBe List(CursorPosition(1, 6))
  }

  it should "unindent selected lines without materialising the whole buffer" in {
    val paneId    = PaneId(0)
    val bufferId  = BufferId(0)
    val selection = Selection(CursorPosition(0, 4), CursorPosition(1, 2))
    val content   = NonCollectingRope(Rope("    alpha\n  beta\n" + "x" * 5000))
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document = AppState.initial.persisted.buffers(bufferId).document.copy(content = content),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(cursors = List(selection.focus), selection = Some(selection))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(ReverseTabKey, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.getLine(0) shouldBe Some("alpha")
    buffer.document.content.getLine(1) shouldBe Some("beta")
    buffer.document.content.getLine(2) shouldBe Some("x" * 5000)
    buffer.editing.cursors shouldBe List(CursorPosition(1, 0))
  }
