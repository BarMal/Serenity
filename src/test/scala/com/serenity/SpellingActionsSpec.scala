package com.serenity

import com.serenity.SpellingStateFixture.*
import com.serenity.spellcheck.{DictionarySnapshot, SpellChecker}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, SpellingIgnoreReducer, SpellingReplacementReducer, UndoEffect}
import com.serenity.state.undo.{EditGrouping, HistoryEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1939: replacing a misspelling is one undoable edit, and dismissing one hides exactly what the writer chose to. */
class SpellingActionsSpec extends AnyFlatSpec with Matchers:

  "SpellingReplacementReducer" should "replace the word and record one undo step that restores the original" in {
    val state  = editor("a wrold here")
    val result = SpellingReplacementReducer.replace(state, 0, 2, 7, "wrold", "world")

    result.map(r => textOf(r.state)) shouldBe Some("a world here")
    result.map(_.effects) match
      case Some(
            List(
              AppEffect.Undo(
                UndoEffect.RecordBoundary(HistoryEntry.BufferEdit(id, pane, before), EditGrouping.Standalone)
              )
            )
          ) =>
        id shouldBe bufferId
        pane shouldBe paneId
        before.content.collect() shouldBe "a wrold here"
      case other => fail(s"Expected one ungrouped undo boundary, got $other")
  }

  it should "move a cursor that followed the word along with the text" in {
    val state  = editor("a wrld here", CursorPosition(0, 11))
    val result = SpellingReplacementReducer.replace(state, 0, 2, 6, "wrld", "world")

    result.flatMap(_.state.activeCursorPosition) shouldBe Some(CursorPosition(0, 12))
  }

  it should "refuse a suggestion offered for text that has since changed" in {
    val state = editor("a wrold here")

    SpellingReplacementReducer.replace(state, 0, 2, 7, "wurld", "world") shouldBe None
    SpellingReplacementReducer.replace(state, 3, 0, 5, "wrold", "world") shouldBe None
    SpellingReplacementReducer.replace(state, 0, 2, 70, "wrold", "world") shouldBe None
  }

  "SpellingIgnoreReducer" should "hide one occurrence and leave the same word elsewhere flagged" in {
    val state   = editor("wrold and wrold")
    val ignored = SpellingIgnoreReducer.ignoreOnce(state, 0, 0, 5, "wrold")

    flaggedWords(state) shouldBe List("wrold", "wrold")
    ignored.map(flaggedWords) shouldBe Some(List("wrold"))
  }

  it should "keep an ignored occurrence hidden when the document is analysed again" in {
    val state   = editor("wrold and wrold")
    val ignored = SpellingIgnoreReducer.ignoreOnce(state, 0, 0, 5, "wrold").getOrElse(fail("expected an ignore"))
    val again = SpellChecker.refreshDiagnostics(
      ignored.copy(runtime =
        ignored.runtime.copy(languageService =
          ignored.runtime.languageService
            .copy(diagnosticsState = ignored.runtime.languageService.diagnosticsState.copy(spellCheckCache = Map.empty))
        )
      ),
      DictionarySnapshot(dictionary, Nil)
    )

    flaggedWords(again) shouldBe List("wrold")
  }

  it should "hide every occurrence of a word, in any case, and keep them hidden" in {
    val state   = editor("wrold and. Wrold and wrld")
    val ignored = SpellingIgnoreReducer.ignoreEverywhere(state, "wrold")

    flaggedWords(state) shouldBe List("wrold", "Wrold", "wrld")
    flaggedWords(ignored) shouldBe List("wrld")
    flaggedWords(SpellChecker.refreshDiagnostics(ignored, DictionarySnapshot(dictionary, Nil))) shouldBe List("wrld")
  }

  it should "apply a dismissal made while an analysis was running to the result of that analysis" in {
    val state    = editor("wrold and wrld")
    val analysed = state
    val ignored  = SpellingIgnoreReducer.ignoreEverywhere(state, "wrold")
    val expected = SpellChecker.analysisFingerprints(state, Nil)

    flaggedWords(SpellChecker.applyIfCurrent(ignored, analysed, expected, Nil)) shouldBe List("wrld")
  }

  it should "stop hiding an occurrence once the text at that place changed" in {
    val state   = editor("wrold and wrold")
    val ignored = SpellingIgnoreReducer.ignoreOnce(state, 0, 0, 5, "wrold").getOrElse(fail("expected an ignore"))
    val edited  = SpellingReplacementReducer.replace(ignored, 0, 0, 5, "wrold", "wrld").map(_.state)
    val again   = edited.map(SpellChecker.refreshDiagnostics(_, DictionarySnapshot(dictionary, Nil)))

    again.map(flaggedWords) shouldBe Some(List("wrld", "wrold"))
  }

  it should "refuse to ignore text that is no longer the flagged word" in {
    SpellingIgnoreReducer.ignoreOnce(editor("wrold here"), 0, 0, 5, "wurld") shouldBe None
  }
