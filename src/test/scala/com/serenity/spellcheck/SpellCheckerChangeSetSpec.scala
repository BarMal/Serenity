package com.serenity.spellcheck

import com.serenity.rope.{ChangeSet, Rope}
import com.serenity.spellcheck.IncrementalSpellFixture.{*, given}
import com.serenity.state.models.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Re-checking from the change an edit recorded (#1838) publishes what comparing the texts publishes, and what a full
  * check of the document finds.
  */
class SpellCheckerChangeSetSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  private val tokens: Gen[String] = Gen.oneOf("the ", "cat ", "teh ", "wurld ", ". ", "\n", "\n\n", "```\n", "---\n")

  private val genDocument: Gen[String] = Gen.listOf(tokens).map(_.mkString)

  private val genEdit: Gen[(Int, Int, String)] =
    for
      at      <- Gen.posNum[Int]
      removed <- Gen.choose(0, 12)
      text    <- Gen.oneOf(tokens, Gen.alphaChar.map(_.toString), Gen.const(""))
    yield (at, removed, text)

  private def withRecordedEdit(state: AppState, change: ChangeSet, content: Rope): AppState =
    val buffer = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers = Map(bufferId -> buffer.copy(document = buffer.document.edited(content, change))))
    )

  "Spell check from a recorded change" should "publish what comparing the texts publishes, after any edits" in
    forAll(genDocument, Gen.listOfN(8, genEdit), minSuccessful(200)) { (text, edits) =>
      val start = Rope(text)
      edits.foldLeft((start, stateWith(start), stateWith(start))) {
        case ((rope, recorded, compared), (at, removed, inserted)) =>
          val from     = at % (rope.weight + 1)
          val to       = math.min(rope.weight, from + removed)
          val change   = ChangeSet.single(rope.weight, from, to, inserted)
          val next     = change.applyTo(rope).getOrElse(rope)
          val byChange = refreshed(withRecordedEdit(recorded, change, next))
          val byTexts  = refreshed(edited(compared, next))
          withClue(s"after $change on ${rope.collect()}: ") {
            published(byChange) shouldBe published(byTexts)
            published(byChange) shouldBe fullAnalysis(next)
          }
          (next, byChange, byTexts)
      }
    }

  it should "publish a full check of the document after a one-character edit" in {
    val start  = Rope("the cat sat on the mat. " * 50)
    val change = ChangeSet.single(start.weight, 4, 4, "x")
    val next   = change.applyTo(start).getOrElse(fail("the change should apply"))

    published(refreshed(withRecordedEdit(stateWith(start), change, next))) shouldBe fullAnalysis(next)
  }

  "A spell-check fingerprint" should "tell two ropes of the same text apart, and a rope from itself" in {
    val rope     = Rope("same text")
    val twin     = Rope("same text")
    val buffer   = Buffer(bufferId, Document(rope))
    val fromRope = SpellCheckFingerprint.from(buffer, config, Nil)
    val fromTwin = SpellCheckFingerprint.from(buffer.copy(document = Document(twin)), config, Nil)

    fromRope shouldBe SpellCheckFingerprint.from(buffer, config, Nil)
    fromRope should not be fromTwin
  }
