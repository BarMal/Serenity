package com.serenity.state.manager

import com.serenity.command.{CommandIntent, CommandRegistry, ViewIntent}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.core.ChapterNoteTransitions
import com.serenity.state.models.*
import com.serenity.ui.layout.SplitAxis
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Opening a chapter's note: the note's text lives in a hidden buffer, shown in a pane split beside the manuscript, and
  * the same chapter always finds the same note again.
  */
class ChapterNoteTransitionsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val manuscriptId = BufferId(0)
  private val storm        = NoteKey.Chapter(HeadingIdentity("storm", 0))
  private val calm         = NoteKey.Chapter(HeadingIdentity("calm", 0))

  private val manuscript = "# Chapter 1: Storm\nthe sea\n# Chapter 2: Calm\nthe shore"

  private def withManuscript(text: String, cursorLine: Int): AppState =
    val initial = AppState.initial
    val buffer  = initial.persisted.buffers(manuscriptId)
    initial.copy(persisted =
      initial.persisted.copy(buffers =
        initial.persisted.buffers.updated(
          manuscriptId,
          buffer.copy(
            document = buffer.document.copy(content = Rope(text), language = Some(LanguageId.Markdown)),
            editing = EditingState(List(CursorPosition(cursorLine, 0)))
          )
        )
      )
    )

  private def open(state: AppState): AppState =
    ChapterNoteTransitions.openCurrentChapterNote(state, SplitAxis.Horizontal)

  private def notes(state: AppState): Map[NoteKey, Notes] =
    state.persisted.buffers(manuscriptId).annotations.notes

  private def returnToManuscript(state: AppState, cursorLine: Int): AppState =
    val manuscriptBuffer = state.persisted.buffers(manuscriptId)
    state.copy(
      persisted = state.persisted.copy(
        buffers = state.persisted.buffers.updated(
          manuscriptId,
          manuscriptBuffer.copy(editing = EditingState(List(CursorPosition(cursorLine, 0))))
        ),
        focus = Focus.EditorPane(PaneId(0)),
        layout = state.persisted.layout.copy(activeEditorPaneId = Some(PaneId(0)))
      )
    )

  private def activePaneBuffer(state: AppState): Option[BufferId] =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)

  "Opening the current chapter's note" should "create a hidden buffer for the chapter the cursor is in" in {
    val opened = open(withManuscript(manuscript, cursorLine = 3))

    val noteId = notes(opened)(calm).overview
    opened.persisted.buffers(noteId).hidden shouldBe true
    opened.persisted.buffers(noteId).document.content.collect() shouldBe ""
    notes(opened).keySet shouldBe Set(calm)
  }

  it should "show the note in a new pane beside the manuscript and focus it" in {
    val opened = open(withManuscript(manuscript, cursorLine = 3))

    val noteId = notes(opened)(calm).overview
    opened.persisted.layout.editorPanes should have size 2
    opened.persisted.layout.editorPanes(PaneId(0)).bufferId shouldBe Some(manuscriptId)
    activePaneBuffer(opened) shouldBe Some(noteId)
    opened.persisted.focus shouldBe Focus.EditorPane(PaneId(1))
  }

  it should "leave the tab order alone" in {
    val opened = open(withManuscript(manuscript, cursorLine = 3))

    opened.persisted.bufferOrder shouldBe List(manuscriptId)
  }

  it should "find the same note again rather than making another" in {
    val first  = open(withManuscript(manuscript, cursorLine = 3))
    val second = open(returnToManuscript(first, cursorLine = 3))

    notes(second) shouldBe notes(first)
    second.persisted.buffers.keySet shouldBe first.persisted.buffers.keySet
    second.persisted.layout.editorPanes should have size 2
    second.persisted.focus shouldBe Focus.EditorPane(PaneId(1))
  }

  it should "give each chapter its own note" in {
    val stormOpened = open(withManuscript(manuscript, cursorLine = 1))
    val bothOpened  = open(returnToManuscript(stormOpened, cursorLine = 3))

    notes(bothOpened).keySet shouldBe Set(storm, calm)
    notes(bothOpened)(storm).overview should not be notes(bothOpened)(calm).overview
    bothOpened.persisted.layout.editorPanes should have size 3
  }

  it should "do nothing before the first heading" in {
    val state = withManuscript("an epigraph\n# Chapter 1: Storm\nthe sea", cursorLine = 0)

    open(state) shouldBe state
  }

  it should "do nothing in a document without headings" in {
    val state = withManuscript("just one paragraph\n\nand another", cursorLine = 0)

    open(state) shouldBe state
  }

  it should "do nothing when the focused pane is already showing a note" in {
    val inNote = open(withManuscript(manuscript, cursorLine = 3))

    open(inNote) shouldBe inNote
  }

  "The command registry" should "offer a command to open the current chapter's note" in {
    CommandRegistry.withToggleUI.findCommand("open-chapter-note").map(_.intent) shouldBe
      Some(CommandIntent.View(ViewIntent.OpenChapterNote))
  }
