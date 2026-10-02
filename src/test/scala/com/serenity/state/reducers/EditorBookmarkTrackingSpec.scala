package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A bookmark is a position the user wants to jump back to, so it has to follow the text it was set beside as edits
  * land around it -- exactly as placeholders and comments do (see `EditorPlaceholderTrackingSpec`).
  */
class EditorBookmarkTrackingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(0)

  private def stateWithBookmarks(text: String, editing: EditingState, bookmarks: CursorPosition*): AppState =
    val initial = AppState.initial.persisted.buffers(bufferId)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = AppState.initial.persisted.buffers.updated(
          bufferId,
          initial.copy(
            document = initial.document.copy(content = com.serenity.rope.Rope(text)),
            editing = editing,
            annotations = initial.annotations.copy(bookmarks = bookmarks.toList)
          )
        )
      )
    )

  private def bookmarksAfter(event: TextEntryEvent, state: AppState): List[CursorPosition] =
    EditorEventReducer.reduce(event, paneId, state).state.persisted.buffers(bufferId).annotations.bookmarks

  private def cursorAt(position: CursorPosition): EditingState = EditingState(List(position))

  "EditorEventReducer" should "move a bookmark after text inserted before it" in {
    val state = stateWithBookmarks("abc def", cursorAt(CursorPosition(0, 0)), CursorPosition(0, 4))

    bookmarksAfter(InsertChar('X'), state) shouldBe List(CursorPosition(0, 5))
  }

  it should "move a bookmark down after a newline inserted before it" in {
    val state = stateWithBookmarks("abc def", cursorAt(CursorPosition(0, 0)), CursorPosition(0, 4))

    bookmarksAfter(NewLine, state) shouldBe List(CursorPosition(1, 4))
  }

  it should "move a bookmark left after text deleted before it" in {
    val state = stateWithBookmarks("abc def", cursorAt(CursorPosition(0, 0)), CursorPosition(0, 4))

    bookmarksAfter(DeleteForward, state) shouldBe List(CursorPosition(0, 3))
  }

  it should "leave a bookmark before text typed exactly at its position" in {
    val state = stateWithBookmarks("abc def", cursorAt(CursorPosition(0, 3)), CursorPosition(0, 3))

    bookmarksAfter(InsertChar('X'), state) shouldBe List(CursorPosition(0, 3))
  }

  it should "leave a bookmark alone when the edit is entirely after it" in {
    val state = stateWithBookmarks("abc def", cursorAt(CursorPosition(0, 7)), CursorPosition(0, 2))

    bookmarksAfter(InsertChar('X'), state) shouldBe List(CursorPosition(0, 2))
  }

  it should "collapse a bookmark to the start of a deleted selection that covered it" in {
    val state = stateWithBookmarks(
      "abc def ghi",
      EditingStateFixtures(
        cursors = List(CursorPosition(0, 7)),
        selection = Some(Selection(CursorPosition(0, 4), CursorPosition(0, 7)))
      ),
      CursorPosition(0, 5)
    )

    bookmarksAfter(DeleteBackward, state) shouldBe List(CursorPosition(0, 4))
  }

  it should "move a bookmark up when a line before it is cut" in {
    val state = stateWithBookmarks("alpha\nbeta", cursorAt(CursorPosition(0, 0)), CursorPosition(1, 2))

    bookmarksAfter(Cut, state) shouldBe List(CursorPosition(0, 2))
  }

  it should "merge two bookmarks that a deletion collapses onto one spot" in {
    val state = stateWithBookmarks(
      "abc def ghi",
      EditingStateFixtures(
        cursors = List(CursorPosition(0, 7)),
        selection = Some(Selection(CursorPosition(0, 4), CursorPosition(0, 7)))
      ),
      CursorPosition(0, 5),
      CursorPosition(0, 6)
    )

    bookmarksAfter(DeleteBackward, state) shouldBe List(CursorPosition(0, 4))
  }
