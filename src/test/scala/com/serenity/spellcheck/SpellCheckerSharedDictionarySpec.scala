package com.serenity.spellcheck

import com.serenity.config.{AppConfig, SpellCheckConfig}
import com.serenity.rope.Rope
import com.serenity.spellcheck.IncrementalSpellFixture.{bufferId, edited, given_Balance, published}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Each refresh in the editor loads its dictionary snapshot again through the one [[DictionaryCache]] its state manager
  * owns. An edit is only re-checked incrementally if that load hands back the dictionary the document was last checked
  * against, so these specs go through the loader rather than a hand-built snapshot.
  */
class SpellCheckerSharedDictionarySpec extends AnyFlatSpec with Matchers:

  private val config = SpellCheckConfig()

  private def load(cache: DictionaryCache): DictionarySnapshot =
    DictionaryLoader.loadSnapshot(config, cache, Nil)

  private def refresh(state: AppState, snapshot: DictionarySnapshot): AppState =
    SpellChecker.refreshDiagnostics(state, snapshot)

  private def stateWith(content: Rope, snapshot: DictionarySnapshot): AppState =
    val base = AppState.initial
    refresh(
      base.copy(persisted =
        base.persisted.copy(
          buffers = Map(bufferId -> Buffer(bufferId, Document(content))),
          bufferOrder = List(bufferId),
          config = AppConfig.default.withSpellCheck(config)
        )
      ),
      snapshot
    )

  "Loading the snapshot again through the same cache with nothing changed" should "return the same dictionary" in {
    val cache = DictionaryCache()

    load(cache).context should be theSameInstanceAs load(cache).context
  }

  "A second edit refreshed against a freshly loaded snapshot" should "reuse the analysis of the first" in {
    val cache   = DictionaryCache()
    val first   = Rope("wurld of prose\n" + "the sun is up\n" * 50)
    val state   = stateWith(first, load(cache))
    val flagged = published(state)
    flagged should not be empty

    val second  = first.insert(first.weight, "x").getOrElse(first)
    val checked = refresh(edited(state, second), load(cache))

    published(checked) should have size flagged.size.toLong
    published(checked).zip(flagged).foreach((now, before) => now should be theSameInstanceAs before)
  }
end SpellCheckerSharedDictionarySpec
