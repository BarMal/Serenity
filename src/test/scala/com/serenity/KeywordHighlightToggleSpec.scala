package com.serenity

import com.serenity.command.{CommandIntent, CommandRegistry, ViewIntent}
import com.serenity.rope.Balance
import com.serenity.state.core.ChapterNoteTransitions
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Hiding the term highlights is a view change: the notes are untouched, and every row that held a highlight repaints
  * as the highlights appear or go.
  */
class KeywordHighlightToggleSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val manuscriptId = BufferId(0)

  private def withKeywordNote(notesPane: Option[NotesPane]): AppState =
    val initial = AppState.initial
    val buffer  = initial.persisted.buffers(manuscriptId)
    val notes   = Map[NoteKey, Notes](NoteKey.Keyword("liz") -> Notes(BufferId(5)))
    val layout  = initial.persisted.layout
    initial.copy(
      persisted = initial.persisted.copy(
        buffers =
          initial.persisted.buffers.updated(manuscriptId, buffer.copy(annotations = Annotations(notes = notes))),
        layout = layout.copy(editorPanes =
          layout.editorPanes.updated(PaneId(1), EditorPane.withBuffer(PaneId(1), BufferId(5)))
        )
      ),
      runtime = initial.runtime.copy(notesPane = notesPane)
    )

  private val open = Some(NotesPane(PaneId(1), PaneId(0)))

  "Term highlights" should "be on by default" in {
    AppState.initial.runtime.keywordHighlightsVisible shouldBe true
  }

  it should "toggle off and back on" in {
    val off = ChapterNoteTransitions.toggleTermHighlights(AppState.initial)

    off.runtime.keywordHighlightsVisible shouldBe false
    ChapterNoteTransitions.toggleTermHighlights(off).runtime.keywordHighlightsVisible shouldBe true
  }

  it should "leave the notes alone when hidden" in {
    val state = withKeywordNote(open)

    ChapterNoteTransitions.toggleTermHighlights(state).persisted.buffers shouldBe state.persisted.buffers
  }

  it should "repaint the document when toggled while the notes pane is open on it" in {
    val before = withKeywordNote(open)
    val after  = ChapterNoteTransitions.toggleTermHighlights(before)

    DamageProducer.forTransition(before, after) should not be Damage.Nothing
  }

  it should "repaint the document when the notes pane opens or closes" in {
    DamageProducer.forTransition(withKeywordNote(None), withKeywordNote(open)) should not be Damage.Nothing
    DamageProducer.forTransition(withKeywordNote(open), withKeywordNote(None)) should not be Damage.Nothing
  }

  it should "cost nothing to toggle while no notes pane is open" in {
    val before = withKeywordNote(None)
    val after  = ChapterNoteTransitions.toggleTermHighlights(before)

    DamageProducer.forTransition(before, after) shouldBe Damage.Nothing
  }

  it should "be toggled by a command" in {
    CommandRegistry.withToggleUI.findCommand("toggle-term-highlights").map(_.intent) shouldBe
      Some(CommandIntent.View(ViewIntent.ToggleTermHighlights))
  }
