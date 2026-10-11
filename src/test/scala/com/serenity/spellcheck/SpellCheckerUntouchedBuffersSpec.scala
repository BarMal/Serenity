package com.serenity.spellcheck

import com.serenity.config.{AppConfig, SpellCheckDictionaryFingerprint}
import com.serenity.lsp.model.Diagnostic
import com.serenity.rope.Rope
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A pass over several open buffers analyses the ones whose text changed, and every one when the dictionary did. */
class SpellCheckerUntouchedBuffersSpec extends AnyFlatSpec with Matchers:

  import IncrementalSpellFixture.{config, dictionary, given}

  private val ids = List(BufferId(1), BufferId(2), BufferId(3))

  private val texts = List("the cat sat", "hello world", "the dog and a mat")

  private def withBuffers(contents: List[String]): AppState =
    val base    = AppState.initial
    val buffers = ids.zip(contents).map((id, text) => id -> Buffer(id, Document(Rope(text))))
    base.copy(persisted =
      base.persisted.copy(
        buffers = buffers.toMap,
        bufferOrder = ids,
        config = AppConfig.default.withSpellCheck(config)
      )
    )

  private def edit(state: AppState, id: BufferId, text: String): AppState =
    val buffer = state.persisted.buffers(id)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(id, buffer.copy(document = buffer.document.copy(content = Rope(text))))
      )
    )

  private def entry(state: AppState, id: BufferId): SpellCheckCacheEntry =
    state.runtime.languageService.diagnosticsState
      .spellCheckCache(SpellChecker.diagnosticsUri(state.persisted.buffers(id)))

  private def shown(state: AppState, id: BufferId): List[Diagnostic] =
    state.runtime.languageService.diagnosticsState.diagnostics
      .getOrElse(SpellChecker.diagnosticsUri(state.persisted.buffers(id)), Nil)

  private def snapshotOf(fingerprints: List[SpellCheckDictionaryFingerprint]): DictionarySnapshot =
    DictionarySnapshot(dictionary, fingerprints)

  private def userDictionary(modified: Long): List[SpellCheckDictionaryFingerprint] =
    List(SpellCheckDictionaryFingerprint("/user/en.dic", exists = true, isDirectory = false, 10L, modified))

  "A spell pass" should "analyse only the buffer that was edited and carry the others' results over untouched" in {
    val before = SpellChecker.refreshDiagnostics(withBuffers(texts), snapshotOf(Nil))
    val after  = SpellChecker.refreshDiagnostics(edit(before, ids(1), "hello wurld"), snapshotOf(Nil))

    entry(after, ids.head) should be theSameInstanceAs entry(before, ids.head)
    entry(after, ids(2)) should be theSameInstanceAs entry(before, ids(2))
    entry(after, ids(1)) should not be theSameInstanceAs(entry(before, ids(1)))
    shown(after, ids(1)).map(_.range.start.character) shouldBe List(6)
  }

  it should "reuse every buffer's result when nothing changed" in {
    val before = SpellChecker.refreshDiagnostics(withBuffers(texts), snapshotOf(Nil))
    val after  = SpellChecker.refreshDiagnostics(before, snapshotOf(Nil))

    ids.foreach(id => entry(after, id) should be theSameInstanceAs entry(before, id))
  }

  it should "check every buffer again when the user dictionary changes" in {
    val before = SpellChecker.refreshDiagnostics(withBuffers(texts), snapshotOf(userDictionary(1L)))
    val after  = SpellChecker.refreshDiagnostics(before, snapshotOf(userDictionary(2L)))

    ids.foreach(id => entry(after, id) should not be theSameInstanceAs(entry(before, id)))
  }

  it should "show a changed dictionary's new verdicts in buffers that were not edited" in {
    val before = SpellChecker.refreshDiagnostics(
      withBuffers(List("the cat sat", "hello wurld", "the dog")),
      snapshotOf(userDictionary(1L))
    )
    val extended = DictionarySnapshot(dictionary.copy(words = dictionary.words + "wurld"), userDictionary(2L))
    val after    = SpellChecker.refreshDiagnostics(before, extended)

    shown(before, ids(1)) should have size 1
    shown(after, ids(1)) shouldBe empty
  }
