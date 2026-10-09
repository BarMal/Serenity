package com.serenity

import com.serenity.document.{KeywordHighlight, KeywordHighlights}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Keyword occurrences are only painted while the notes pane is open on the document that owns the notes: the pane is
  * opened on purpose, so the highlights appear when the writer is looking at notes and not otherwise.
  */
class KeywordHighlightsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val manuscriptId = BufferId(0)
  private val liz          = NoteKey.Keyword("liz")
  private val ladyLiz      = NoteKey.Keyword("lady liz")
  private val storm        = NoteKey.Chapter(HeadingIdentity("storm", 0))

  private def stateWith(
    notes: Map[NoteKey, Notes],
    notesPane: Option[NotesPane] = Some(NotesPane(PaneId(1), PaneId(0))),
    visible: Boolean = true
  ): AppState =
    val initial = AppState.initial
    val base    = initial.persisted.buffers(manuscriptId)
    val layout  = initial.persisted.layout
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = initial.persisted.buffers.updated(manuscriptId, base.copy(annotations = Annotations(notes = notes))),
        layout = layout.copy(editorPanes =
          layout.editorPanes.updated(PaneId(1), EditorPane.withBuffer(PaneId(1), BufferId(5)))
        )
      ),
      runtime = initial.runtime.copy(notesPane = notesPane, keywordHighlightsVisible = visible)
    )

  private val keywordNotes: Map[NoteKey, Notes] =
    Map(liz -> Notes(BufferId(5)), ladyLiz -> Notes(BufferId(6)), storm -> Notes(BufferId(7)))

  "The terms painted in a document" should "be its keyword notes while the notes pane follows it" in {
    KeywordHighlights.paintedTerms(stateWith(keywordNotes), manuscriptId) shouldBe Set("liz", "lady liz")
  }

  it should "be none while no notes pane is open" in {
    KeywordHighlights.paintedTerms(stateWith(keywordNotes, notesPane = None), manuscriptId) shouldBe Set.empty
  }

  it should "be none once the highlights are switched off" in {
    KeywordHighlights.paintedTerms(stateWith(keywordNotes, visible = false), manuscriptId) shouldBe Set.empty
  }

  it should "be none for a document the notes pane is not following" in {
    KeywordHighlights.paintedTerms(stateWith(keywordNotes), BufferId(9)) shouldBe Set.empty
  }

  it should "be none for a document with only chapter notes" in {
    KeywordHighlights.paintedTerms(stateWith(Map(storm -> Notes(BufferId(7)))), manuscriptId) shouldBe Set.empty
  }

  private def onLines(text: String, terms: Set[String], lines: Set[Int]): Map[Int, List[(Int, Int)]] =
    KeywordHighlights.onLines(Rope(text), terms, lines).view.mapValues(_.map(h => (h.start.column, h.end.column))).toMap

  "The highlights on a line" should "cover each whole-word occurrence of each term" in {
    onLines("Liz saw Lizard and Liz.", Set("liz"), Set(0)) shouldBe Map(0 -> List((0, 3), (19, 22)))
  }

  it should "prefer the longer term where two overlap" in {
    onLines("Lady Liz left", Set("liz", "lady liz"), Set(0)) shouldBe Map(0 -> List((0, 8)))
  }

  it should "look only at the requested lines" in {
    onLines("Liz\nLiz\nLiz", Set("liz"), Set(1)) shouldBe Map(1 -> List((0, 3)))
  }

  it should "leave out lines with no occurrence" in {
    onLines("nothing\nLiz", Set("liz"), Set(0, 1)) shouldBe Map(1 -> List((0, 3)))
  }

  it should "report each highlight on its own line" in {
    KeywordHighlights.onLines(Rope("a Liz"), Set("liz"), Set(0)).get(0) shouldBe
      Some(List(KeywordHighlight(CursorPosition(0, 2), CursorPosition(0, 5))))
  }
