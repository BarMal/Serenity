package com.serenity.state.core

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A notes pane that follows the cursor shows a keyword's note while the cursor touches that keyword, and the chapter's
  * note everywhere else -- and the keyword is only looked for on the cursor's own line, so moving about a line without
  * reaching one costs nothing.
  */
class NotesPaneKeywordFollowSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val manuscriptId   = BufferId(0)
  private val stormNoteId    = BufferId(5)
  private val seaNoteId      = BufferId(7)
  private val manuscriptPane = PaneId(0)
  private val notesPaneId    = PaneId(1)

  private val storm = NoteKey.Chapter(HeadingIdentity("storm", 0))
  private val sea   = NoteKey.Keyword("sea")

  private val text = "# Chapter 1: Storm\nthe sea\nthe shore"

  private def hiddenNote(id: BufferId, content: String): Buffer =
    Buffer.fromString(id, content).copy(hidden = true)

  /** Pane 0 holds the manuscript with its cursor at `line`/`column`; pane 1 is the notes pane, on the storm note. */
  private def stateWith(line: Int, column: Int, pinned: Boolean = false): AppState =
    val initial = AppState.initial
    val base    = initial.persisted.buffers(manuscriptId)
    val manuscript = base.copy(
      document = base.document.copy(content = Rope(text), language = Some(LanguageId.Markdown)),
      editing = EditingState(List(CursorPosition(line, column))),
      annotations = Annotations(notes = Map(storm -> Notes(stormNoteId), sea -> Notes(seaNoteId)))
    )
    val layout = initial.persisted.layout
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = initial.persisted.buffers
          .updated(manuscriptId, manuscript)
          .updated(stormNoteId, hiddenNote(stormNoteId, "storm note"))
          .updated(seaNoteId, hiddenNote(seaNoteId, "sea note")),
        layout = layout.copy(editorPanes =
          layout.editorPanes.updated(notesPaneId, EditorPane.withBuffer(notesPaneId, stormNoteId))
        )
      ),
      runtime = initial.runtime.copy(notesPane = Some(NotesPane(notesPaneId, manuscriptPane, pinned)))
    )

  private def withCursor(state: AppState, line: Int, column: Int): AppState =
    val buffer = state.persisted.buffers(manuscriptId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers
          .updated(manuscriptId, buffer.copy(editing = EditingState(List(CursorPosition(line, column)))))
      )
    )

  private def shown(state: AppState): Option[BufferId] =
    state.persisted.layout.editorPanes.get(notesPaneId).flatMap(_.bufferId)

  "The notes pane" should "show a keyword's note while the cursor touches the keyword" in {
    val before = stateWith(line = 1, column = 0)
    val after  = NotesPaneSync.synced(withCursor(before, 1, 5), before)

    shown(after) shouldBe Some(seaNoteId)
  }

  it should "return to the chapter's note when the cursor leaves the keyword" in {
    val before = stateWith(line = 1, column = 0)
    val onSea  = NotesPaneSync.synced(withCursor(before, 1, 5), before)
    val off    = NotesPaneSync.synced(withCursor(onSea, 1, 0), onSea)

    shown(onSea) shouldBe Some(seaNoteId)
    shown(off) shouldBe Some(stormNoteId)
  }

  it should "show the chapter's note once the cursor is on a line with no keyword" in {
    val before = stateWith(line = 1, column = 0)
    val onSea  = NotesPaneSync.synced(withCursor(before, 1, 5), before)
    val after  = NotesPaneSync.synced(withCursor(onSea, 2, 0), onSea)

    shown(after) shouldBe Some(stormNoteId)
  }

  it should "leave the layout alone when the cursor moves along a line without reaching a keyword" in {
    val before = stateWith(line = 1, column = 0)
    val moved  = withCursor(before, 1, 1)

    NotesPaneSync.synced(moved, before) shouldBe theSameInstanceAs(moved)
  }

  it should "stay on its note while pinned, even on a keyword" in {
    val before = stateWith(line = 1, column = 0, pinned = true)
    val after  = NotesPaneSync.synced(withCursor(before, 1, 5), before)

    shown(after) shouldBe Some(stormNoteId)
  }
