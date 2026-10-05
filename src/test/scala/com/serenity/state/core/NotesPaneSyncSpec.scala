package com.serenity.state.core

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.ui.layout.Symbol
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The notes pane shows the note for the chapter the cursor is in, retargeting as the cursor crosses chapters, until it
  * is pinned. Each note is a buffer of its own, so each keeps its own scroll position and cursor across the swaps.
  */
class NotesPaneSyncSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val manuscriptId   = BufferId(0)
  private val stormNoteId    = BufferId(5)
  private val calmNoteId     = BufferId(6)
  private val manuscriptPane = PaneId(0)
  private val notesPaneId    = PaneId(1)
  private val storm          = NoteKey.Chapter(HeadingIdentity("storm", 0))
  private val calm           = NoteKey.Chapter(HeadingIdentity("calm", 0))

  private val text = "# Chapter 1: Storm\nthe sea\n# Chapter 2: Calm\nthe shore\n# Chapter 3: Dusk\nthe sky"

  private def hiddenNote(id: BufferId, content: String): Buffer =
    Buffer.fromString(id, content).copy(hidden = true)

  /** Pane 0 holds the manuscript with the cursor on `cursorLine`; pane 1 is the notes pane, showing the storm note. */
  private def stateWith(cursorLine: Int, pinned: Boolean = false, registered: Boolean = true): AppState =
    val initial = AppState.initial
    val base    = initial.persisted.buffers(manuscriptId)
    val manuscript = base.copy(
      document = base.document.copy(content = Rope(text), language = Some(LanguageId.Markdown)),
      editing = EditingState(List(CursorPosition(cursorLine, 0))),
      annotations = Annotations(notes = Map(storm -> Notes(stormNoteId), calm -> Notes(calmNoteId)))
    )
    val layout = initial.persisted.layout
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = initial.persisted.buffers
          .updated(manuscriptId, manuscript)
          .updated(stormNoteId, hiddenNote(stormNoteId, "storm note"))
          .updated(calmNoteId, hiddenNote(calmNoteId, "calm note")),
        layout = layout.copy(editorPanes =
          layout.editorPanes.updated(notesPaneId, EditorPane.withBuffer(notesPaneId, stormNoteId))
        )
      ),
      runtime = initial.runtime.copy(
        notesPane = Option.when(registered)(NotesPane(notesPaneId, manuscriptPane, pinned))
      )
    )

  private def withCursor(state: AppState, line: Int): AppState =
    val buffer = state.persisted.buffers(manuscriptId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers
          .updated(manuscriptId, buffer.copy(editing = EditingState(List(CursorPosition(line, 0)))))
      )
    )

  private def shown(state: AppState): Option[BufferId] =
    state.persisted.layout.editorPanes.get(notesPaneId).flatMap(_.bufferId)

  "The notes pane" should "show the note for the chapter the cursor moves into" in {
    val before = stateWith(cursorLine = 0)
    val after  = NotesPaneSync.synced(withCursor(before, 2), before)

    shown(after) shouldBe Some(calmNoteId)
  }

  private def headings(state: AppState): Memo[List[(HeadingIdentity, Symbol)]] =
    state.runtime.chapterHeadingMemo.forBuffer(manuscriptId).getOrElse(fail("expected the manuscript's headings"))

  it should "find the cursor's chapter again without re-parsing when only the cursor line changed" in {
    val first  = stateWith(cursorLine = 0)
    val inCalm = NotesPaneSync.synced(withCursor(first, 2), first)
    val later  = NotesPaneSync.synced(withCursor(inCalm, 3), inCalm)

    shown(later) shouldBe Some(calmNoteId)
    headings(later) should be theSameInstanceAs headings(inCalm)
  }

  it should "re-parse the headings once the text has changed" in {
    val first    = stateWith(cursorLine = 0)
    val inCalm   = NotesPaneSync.synced(withCursor(first, 2), first)
    val buffer   = inCalm.persisted.buffers(manuscriptId)
    val untitled = buffer.copy(document = buffer.document.withContent(Rope("# Chapter 1: Storm\nthe sea\nthe shore")))
    val rewritten =
      inCalm.copy(persisted = inCalm.persisted.copy(buffers = inCalm.persisted.buffers.updated(manuscriptId, untitled)))
    val after = NotesPaneSync.synced(withCursor(rewritten, 1), rewritten)

    shown(after) shouldBe Some(stormNoteId)
    headings(after) should not be theSameInstanceAs(headings(inCalm))
  }

  it should "come back to an earlier chapter's note when the cursor returns" in {
    val first  = stateWith(cursorLine = 0)
    val inCalm = NotesPaneSync.synced(withCursor(first, 2), first)
    val back   = NotesPaneSync.synced(withCursor(inCalm, 1), inCalm)

    shown(back) shouldBe Some(stormNoteId)
  }

  it should "show nothing in a chapter that has no note yet" in {
    val before = stateWith(cursorLine = 0)
    val after  = NotesPaneSync.synced(withCursor(before, 4), before)

    shown(after) shouldBe None
  }

  it should "retarget as soon as it is registered, without waiting for the cursor to move" in {
    val unregistered = stateWith(cursorLine = 2, registered = false)
    val registered =
      unregistered.copy(runtime = unregistered.runtime.copy(notesPane = Some(NotesPane(notesPaneId, manuscriptPane))))

    shown(NotesPaneSync.synced(registered, unregistered)) shouldBe Some(calmNoteId)
  }

  it should "stay on its note while pinned, wherever the cursor goes" in {
    val before = stateWith(cursorLine = 0, pinned = true)
    val after  = NotesPaneSync.synced(withCursor(before, 2), before)

    shown(after) shouldBe Some(stormNoteId)
  }

  it should "follow again once it is unpinned" in {
    val pinned   = withCursor(stateWith(cursorLine = 0, pinned = true), 2)
    val unpinned = pinned.copy(runtime = pinned.runtime.copy(notesPane = Some(NotesPane(notesPaneId, manuscriptPane))))

    shown(NotesPaneSync.synced(unpinned, pinned)) shouldBe Some(calmNoteId)
  }

  it should "leave the layout alone when the cursor has not changed line" in {
    val before = stateWith(cursorLine = 0)
    val edited = before.copy(persisted =
      before.persisted.copy(layout =
        before.persisted.layout.copy(editorPanes =
          before.persisted.layout.editorPanes.updated(notesPaneId, EditorPane.withBuffer(notesPaneId, calmNoteId))
        )
      )
    )

    NotesPaneSync.synced(edited, edited) shouldBe theSameInstanceAs(edited)
  }

  it should "be forgotten when its pane is closed" in {
    val before = stateWith(cursorLine = 0)
    val closed = before.copy(persisted =
      before.persisted.copy(layout =
        before.persisted.layout.copy(editorPanes = before.persisted.layout.editorPanes - notesPaneId)
      )
    )

    NotesPaneSync.synced(closed, before).runtime.notesPane shouldBe None
  }

  it should "be forgotten when the pane it follows is closed" in {
    val before = stateWith(cursorLine = 0)
    val closed = before.copy(persisted =
      before.persisted.copy(layout =
        before.persisted.layout.copy(editorPanes = before.persisted.layout.editorPanes - manuscriptPane)
      )
    )

    NotesPaneSync.synced(closed, before).runtime.notesPane shouldBe None
  }

  it should "leave every note's own scroll position and cursor alone as it swaps" in {
    val scrolled = Viewport.default.copy(topLine = 40)
    val before   = stateWith(cursorLine = 0)
    val withScroll = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(stormNoteId, before.persisted.buffers(stormNoteId).copy(viewport = scrolled))
      )
    )
    val toCalm = NotesPaneSync.synced(withCursor(withScroll, 2), withScroll)
    val back   = NotesPaneSync.synced(withCursor(toCalm, 0), toCalm)

    shown(back) shouldBe Some(stormNoteId)
    back.persisted.buffers(stormNoteId).viewport shouldBe scrolled
  }
