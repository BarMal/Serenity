package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for how `EditorEventReducer` keeps `DocumentComment` ranges anchored to the text they annotate as edits
  * shift, insert into, or delete from the buffer around them (extracted from the former monolithic
  * `EditorEventReducerSpec`, #1442).
  */
class EditorDocumentCommentTrackingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def stateWithCommentedText(
    text: String,
    cursor: CursorPosition,
    comment: DocumentComment
  ): (PaneId, BufferId, AppState) =
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document =
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope(text)),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(cursor)),
              annotations =
                AppState.initial.persisted.buffers(bufferId).annotations.copy(documentComments = List(comment))
            )
        )
      )
    )

    (paneId, bufferId, state)

  "EditorEventReducer" should "move document comments after inserted text before them" in {
    val comment = DocumentComment(CursorPosition(0, 4), CursorPosition(0, 7), "note")
    val (paneId, bufferId, initialState) =
      stateWithCommentedText("abc def", CursorPosition(0, 0), comment)

    val updatedState = EditorEventReducer.reduce(InsertChar('X'), paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "Xabc def"
    buffer.annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 5), CursorPosition(0, 8), "note")
    )
  }

  it should "move document comments after inserted text without materialising the buffer" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val comment  = DocumentComment(CursorPosition(0, 4), CursorPosition(0, 7), "note")
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
                .copy(content = NonCollectingRope(Rope("abc def"))),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 0))),
              annotations =
                AppState.initial.persisted.buffers(bufferId).annotations.copy(documentComments = List(comment))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(InsertChar('X'), paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.getLine(0) shouldBe Some("Xabc def")
    buffer.annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 5), CursorPosition(0, 8), "note")
    )
  }

  it should "move document comments down after newlines inserted before them" in {
    val comment = DocumentComment(CursorPosition(0, 4), CursorPosition(0, 7), "note")
    val (paneId, bufferId, initialState) =
      stateWithCommentedText("abc def", CursorPosition(0, 0), comment)

    val updatedState = EditorEventReducer.reduce(NewLine, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "\nabc def"
    buffer.annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(1, 4), CursorPosition(1, 7), "note")
    )
  }

  it should "move document comments left after deleted text before them" in {
    val comment = DocumentComment(CursorPosition(0, 4), CursorPosition(0, 7), "note")
    val (paneId, bufferId, initialState) =
      stateWithCommentedText("abc def", CursorPosition(0, 0), comment)

    val updatedState = EditorEventReducer.reduce(DeleteForward, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "bc def"
    buffer.annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 3), CursorPosition(0, 6), "note")
    )
  }

  it should "move document comments after deletion without materialising the buffer" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val comment  = DocumentComment(CursorPosition(0, 4), CursorPosition(0, 7), "note")
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
                .copy(content = NonCollectingRope(Rope("abc def"))),
              editing = AppState.initial.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(0, 0))),
              annotations =
                AppState.initial.persisted.buffers(bufferId).annotations.copy(documentComments = List(comment))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteForward, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.getLine(0) shouldBe Some("bc def")
    buffer.annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 3), CursorPosition(0, 6), "note")
    )
  }

  it should "expand document comments when text is inserted inside them" in {
    val comment = DocumentComment(CursorPosition(0, 4), CursorPosition(0, 7), "note")
    val (paneId, bufferId, initialState) =
      stateWithCommentedText("abc def", CursorPosition(0, 5), comment)

    val updatedState = EditorEventReducer.reduce(InsertChar('X'), paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "abc dXef"
    buffer.annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 4), CursorPosition(0, 8), "note")
    )
  }

  it should "collapse document comments to a point marker when their full range is deleted" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val comment  = DocumentComment(CursorPosition(0, 4), CursorPosition(0, 7), "note")
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
                .copy(content = com.serenity.rope.Rope("abc def ghi")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(
                  cursors = List(CursorPosition(0, 7)),
                  selection = Some(Selection(CursorPosition(0, 4), CursorPosition(0, 7)))
                ),
              annotations =
                AppState.initial.persisted.buffers(bufferId).annotations.copy(documentComments = List(comment))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteBackward, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "abc  ghi"
    buffer.annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 4), CursorPosition(0, 4), "note")
    )
  }

  it should "move document comments when a selected line is indented" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val comment  = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 4), "note")
    val initialState = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          AppState.initial.persisted
            .buffers(bufferId)
            .copy(
              document =
                AppState.initial.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("beta")),
              editing = AppState.initial.persisted
                .buffers(bufferId)
                .editing
                .copy(
                  cursors = List(CursorPosition(0, 4)),
                  selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 4)))
                ),
              annotations =
                AppState.initial.persisted.buffers(bufferId).annotations.copy(documentComments = List(comment))
            )
        )
      )
    )

    val updatedState = EditorEventReducer.reduce(TabKey, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "    beta"
    buffer.annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 4), CursorPosition(0, 8), "note")
    )
  }

  it should "move document comments when a line before them is cut" in {
    val comment = DocumentComment(CursorPosition(1, 0), CursorPosition(1, 4), "note")
    val (paneId, bufferId, initialState) =
      stateWithCommentedText("alpha\nbeta", CursorPosition(0, 0), comment)

    val updatedState = EditorEventReducer.reduce(Cut, paneId, initialState).state
    val buffer       = updatedState.persisted.buffers(bufferId)

    buffer.document.content.collect() shouldBe "beta"
    buffer.annotations.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 0), CursorPosition(0, 4), "note")
    )
  }
