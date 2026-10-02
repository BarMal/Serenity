package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `Placeholder` markers must follow the text they were dropped beside as edits land around them, the same way
  * `DocumentComment` ranges do (see `EditorDocumentCommentTrackingSpec`). Without it a placeholder silently drifts onto
  * unrelated text, and anything anchored to it -- navigation today, a linked note later -- points at the wrong place.
  */
class EditorPlaceholderTrackingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(0)

  private def stateWithPlaceholder(
    text: String,
    editing: EditingState,
    placeholder: Placeholder
  ): AppState =
    val initial = AppState.initial.persisted.buffers(bufferId)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          initial.copy(
            document = initial.document.copy(content = com.serenity.rope.Rope(text)),
            editing = editing,
            annotations = initial.annotations.copy(placeholders = List(placeholder))
          )
        )
      )
    )

  private def placeholdersAfter(event: TextEntryEvent, state: AppState): List[Placeholder] =
    EditorEventReducer.reduce(event, paneId, state).state.persisted.buffers(bufferId).annotations.placeholders

  private def cursorAt(position: CursorPosition): EditingState = EditingState(List(position))

  "EditorEventReducer" should "move a placeholder after text inserted before it" in {
    val state = stateWithPlaceholder("abc def", cursorAt(CursorPosition(0, 0)), Placeholder(CursorPosition(0, 4), "n"))

    placeholdersAfter(InsertChar('X'), state) shouldBe List(Placeholder(CursorPosition(0, 5), "n"))
  }

  it should "move a placeholder down after a newline inserted before it" in {
    val state = stateWithPlaceholder("abc def", cursorAt(CursorPosition(0, 0)), Placeholder(CursorPosition(0, 4), "n"))

    placeholdersAfter(NewLine, state) shouldBe List(Placeholder(CursorPosition(1, 4), "n"))
  }

  it should "move a placeholder left after text deleted before it" in {
    val state = stateWithPlaceholder("abc def", cursorAt(CursorPosition(0, 0)), Placeholder(CursorPosition(0, 4), "n"))

    placeholdersAfter(DeleteForward, state) shouldBe List(Placeholder(CursorPosition(0, 3), "n"))
  }

  it should "leave a placeholder before text typed exactly at its position" in {
    val state = stateWithPlaceholder("abc def", cursorAt(CursorPosition(0, 3)), Placeholder(CursorPosition(0, 3), "n"))

    placeholdersAfter(InsertChar('X'), state) shouldBe List(Placeholder(CursorPosition(0, 3), "n"))
  }

  it should "keep a placeholder at the end of a heading line when Enter is pressed there" in {
    val state = stateWithPlaceholder(
      "# Chapter 1\nbody",
      cursorAt(CursorPosition(0, 11)),
      Placeholder(CursorPosition(0, 11), "outline the opening")
    )

    placeholdersAfter(NewLine, state) shouldBe List(Placeholder(CursorPosition(0, 11), "outline the opening"))
  }

  it should "leave a placeholder alone when the edit is entirely after it" in {
    val state = stateWithPlaceholder("abc def", cursorAt(CursorPosition(0, 7)), Placeholder(CursorPosition(0, 2), "n"))

    placeholdersAfter(InsertChar('X'), state) shouldBe List(Placeholder(CursorPosition(0, 2), "n"))
  }

  it should "collapse a placeholder to the start of a deleted selection that covered it" in {
    val state = stateWithPlaceholder(
      "abc def ghi",
      EditingStateFixtures(
        cursors = List(CursorPosition(0, 7)),
        selection = Some(Selection(CursorPosition(0, 4), CursorPosition(0, 7)))
      ),
      Placeholder(CursorPosition(0, 5), "n")
    )

    placeholdersAfter(DeleteBackward, state) shouldBe List(Placeholder(CursorPosition(0, 4), "n"))
  }

  it should "move a placeholder on a selected line when the line is indented" in {
    val state = stateWithPlaceholder(
      "beta",
      EditingStateFixtures(
        cursors = List(CursorPosition(0, 4)),
        selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 4)))
      ),
      Placeholder(CursorPosition(0, 2), "n")
    )

    placeholdersAfter(TabKey, state) shouldBe List(Placeholder(CursorPosition(0, 6), "n"))
  }

  it should "move a placeholder up when a line before it is cut" in {
    val state = stateWithPlaceholder(
      "alpha\nbeta",
      cursorAt(CursorPosition(0, 0)),
      Placeholder(CursorPosition(1, 2), "n")
    )

    placeholdersAfter(Cut, state) shouldBe List(Placeholder(CursorPosition(0, 2), "n"))
  }
