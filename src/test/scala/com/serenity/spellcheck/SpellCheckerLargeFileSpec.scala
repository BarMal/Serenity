package com.serenity.spellcheck

import com.serenity.config.AppConfig
import com.serenity.rope.Rope
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A document above the large-file tooling limit is not spell-checked (#1867). */
class SpellCheckerLargeFileSpec extends AnyFlatSpec with Matchers:

  import IncrementalSpellFixture.{bufferId, config, published, snapshot, given}

  private val text = "the cat sat on teh mat"

  private def refreshedWith(policy: LargeFilePolicy): AppState =
    val base = AppState.initial
    SpellChecker.refreshDiagnostics(
      base.copy(persisted =
        base.persisted.copy(
          buffers = Map(bufferId -> Buffer(bufferId, Document(Rope(text)))),
          bufferOrder = List(bufferId),
          config = AppConfig.default.withSpellCheck(config).withLargeFilePolicy(policy)
        )
      ),
      snapshot
    )

  "SpellChecker.refreshDiagnostics" should "flag a misspelling in a document under the limit" in {
    published(refreshedWith(LargeFilePolicy(warnAboveChars = 10, toolingOffAboveChars = 100))) should have size 1
  }

  it should "publish nothing and cache nothing for a document above the limit" in {
    val state = refreshedWith(LargeFilePolicy(warnAboveChars = 5, toolingOffAboveChars = 10))

    published(state) shouldBe Nil
    state.runtime.languageService.diagnosticsState.spellCheckCache shouldBe Map.empty
  }

  "SpellChecker.analysisFingerprints" should "leave a document above the limit out" in {
    val state = refreshedWith(LargeFilePolicy(warnAboveChars = 5, toolingOffAboveChars = 10))

    SpellChecker.analysisFingerprints(state, Nil) shouldBe Map.empty
  }
