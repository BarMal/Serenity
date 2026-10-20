package com.serenity.spellcheck

import com.serenity.config.AppConfig
import com.serenity.lsp.model.{LspPosition, LspRange}
import com.serenity.rope.{ChangeSet, Rope}
import com.serenity.spellcheck.IncrementalSpellFixture.{*, given}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1929: an analysis result is published onto the buffers it still fits, moved through the edits made while it ran,
  * and one buffer's edit does not make any other buffer be checked again.
  */
class SpellCheckerPublicationSpec extends AnyFlatSpec with Matchers:

  private val text = "the cat sat\nteh dog"

  private def unanalysed(state: AppState): AppState =
    state.copy(runtime =
      state.runtime.copy(languageService = state.runtime.languageService.copy(diagnosticsState = DiagnosticsState()))
    )

  private def withInsertedAtStart(state: AppState, insert: String): AppState =
    val buffer  = state.persisted.buffers(bufferId)
    val content = buffer.document.content
    val updated = Rope(insert + content.collect())
    val change  = ChangeSet.single(content.weight, 0, 0, insert)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(bufferId, buffer.copy(document = buffer.document.edited(updated, change)))
      )
    )

  "An analysis finished after an edit" should "be published moved to where the edit left the word" in {
    val analysed = stateWith(Rope(text))
    val waiting  = unanalysed(analysed)
    val expected = SpellChecker.analysisFingerprints(waiting, Nil)
    val current  = withInsertedAtStart(waiting, "hello\n")

    val result = SpellChecker.applyIfCurrent(current, analysed, expected, Nil)

    published(result).map(_.range) shouldBe List(LspRange(LspPosition(2, 0), LspPosition(2, 3)))
  }

  it should "leave the analysed text in the cache for the next check to continue from" in {
    val analysed = stateWith(Rope(text))
    val waiting  = unanalysed(analysed)
    val expected = SpellChecker.analysisFingerprints(waiting, Nil)
    val current  = withInsertedAtStart(waiting, "hello\n")

    val result = SpellChecker.applyIfCurrent(current, analysed, expected, Nil)

    val cache = result.runtime.languageService.diagnosticsState.spellCheckCache
    cache.values.flatMap(_.analysis).map(_.content.collect()).toList shouldBe List(text)
  }

  it should "be published as found when nothing changed meanwhile" in {
    val analysed = stateWith(Rope(text))
    val waiting  = unanalysed(analysed)
    val expected = SpellChecker.analysisFingerprints(waiting, Nil)

    val result = SpellChecker.applyIfCurrent(waiting, analysed, expected, Nil)

    published(result) shouldBe published(analysed)
  }

  "A pass after an edit to one buffer" should "not check another buffer again" in {
    val otherId = BufferId(2)
    val base    = AppState.initial
    val both = base.copy(persisted =
      base.persisted.copy(
        buffers = Map(
          bufferId -> Buffer(bufferId, Document(Rope(text))),
          otherId  -> Buffer(otherId, Document(Rope("the dog sat on teh mat")))
        ),
        bufferOrder = List(bufferId, otherId),
        config = AppConfig.default.withSpellCheck(config)
      )
    )
    val first                  = refreshed(both)
    val second                 = refreshed(withInsertedAtStart(first, "hello "))
    val otherUri               = SpellChecker.diagnosticsUri(first.persisted.buffers(otherId))
    def entry(state: AppState) = state.runtime.languageService.diagnosticsState.spellCheckCache(otherUri)

    entry(second) should be theSameInstanceAs entry(first)
  }
