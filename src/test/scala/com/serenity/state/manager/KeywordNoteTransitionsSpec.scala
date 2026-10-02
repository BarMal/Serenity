package com.serenity.state.manager

import cats.data.NonEmptyList
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.core.ChapterNoteTransitions
import com.serenity.state.models.*
import com.serenity.ui.layout.SplitAxis
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Opening the note for a word: the note is keyed by the word's normalised text, so every spelling of it and every
  * occurrence finds the same note, and it is shown in the same notes pane as the chapter notes.
  */
class KeywordNoteTransitionsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val manuscriptId = BufferId(0)
  private val liz          = NoteKey.Keyword("liz")

  private val text = "# Chapter 1: Storm\nThen Liz left the Lady Liz behind.\nLIZ wept."

  private def withCursor(line: Int, column: Int, anchor: Option[CursorPosition] = None): AppState =
    val initial = AppState.initial
    val buffer  = initial.persisted.buffers(manuscriptId)
    initial.copy(persisted =
      initial.persisted.copy(buffers =
        initial.persisted.buffers.updated(
          manuscriptId,
          buffer.copy(
            document = buffer.document.copy(content = Rope(text)),
            editing = EditingState(NonEmptyList.one(Cursor(CursorPosition(line, column), anchor)))
          )
        )
      )
    )

  private def open(state: AppState): AppState =
    ChapterNoteTransitions.openCurrentKeywordNote(state, SplitAxis.Horizontal)

  private def notes(state: AppState): Map[NoteKey, Notes] =
    state.persisted.buffers(manuscriptId).annotations.notes

  private def backToManuscript(state: AppState, line: Int, column: Int): AppState =
    val buffer = state.persisted.buffers(manuscriptId)
    state.copy(persisted =
      state.persisted.copy(
        buffers = state.persisted.buffers
          .updated(manuscriptId, buffer.copy(editing = EditingState(List(CursorPosition(line, column))))),
        focus = Focus.EditorPane(PaneId(0)),
        layout = state.persisted.layout.copy(activeEditorPaneId = Some(PaneId(0)))
      )
    )

  "Opening the note for the word under the cursor" should "create a hidden note keyed by the word" in {
    val opened = open(withCursor(1, 6))

    notes(opened).keySet shouldBe Set(liz)
    opened.persisted.buffers(notes(opened)(liz).overview).hidden shouldBe true
  }

  it should "show the note in a new pane beside the manuscript, focus it and make it the notes pane" in {
    val opened = open(withCursor(1, 6))

    opened.persisted.layout.editorPanes should have size 2
    opened.persisted.focus shouldBe Focus.EditorPane(PaneId(1))
    opened.runtime.notesPane shouldBe Some(NotesPane(PaneId(1), PaneId(0)))
  }

  it should "find the same note from another occurrence and another spelling of the word" in {
    val first  = open(withCursor(1, 6))
    val second = open(backToManuscript(first, line = 2, column = 1))

    notes(second) shouldBe notes(first)
    second.persisted.buffers.keySet shouldBe first.persisted.buffers.keySet
  }

  it should "reuse the notes pane for a chapter note rather than splitting again" in {
    val keyword = open(withCursor(1, 6))
    val chapter = ChapterNoteTransitions.openCurrentChapterNote(
      backToManuscript(keyword, line = 1, column = 0),
      SplitAxis.Horizontal
    )

    chapter.persisted.layout.editorPanes should have size 2
    notes(chapter).keySet.exists { case NoteKey.Chapter(_) => true; case _ => false } shouldBe true
  }

  it should "key the note by the selected words when a selection sits on one line" in {
    val selected = withCursor(1, 26, anchor = Some(CursorPosition(1, 18)))

    notes(open(selected)).keySet shouldBe Set(NoteKey.Keyword("lady liz"))
  }

  it should "do nothing when the cursor is not on a word" in {
    val state = withCursor(0, 1)

    open(state) shouldBe state
  }

  it should "do nothing inside a note, which has no keywords of its own" in {
    val inNote = open(withCursor(1, 6))

    open(inNote) shouldBe inNote
  }
