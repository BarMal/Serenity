package com.serenity.spellcheck

import com.serenity.config.{SpellCheckConfig, SpellCheckDictionaryFingerprint}
import com.serenity.rope.{Leaf, Rope}
import com.serenity.spellcheck.IncrementalSpellFixture.{*, given}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Re-checking only what an edit touched must publish exactly what checking the whole document would. */
class SpellCheckerIncrementalSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  // Whatever `collect()` would read: a refresh that needs the whole text of an edited document fails on it.
  final private class UncollectableRope(text: String) extends Leaf(text):
    override def collect(): String = throw AssertionError("a one-character edit should not collect the document")

  private enum Edit:
    case Insert(at: Int, text: String)
    case Delete(from: Int, length: Int)

    def applyTo(rope: Rope): Rope = this match
      case Insert(at, text) => rope.insert(at % (rope.weight + 1), text).getOrElse(rope)
      case Delete(from, length) =>
        val start = from % (rope.weight + 1)
        rope.deleteRight(start, length % (rope.weight - start + 1)).getOrElse(rope)

  private val tokens: Gen[String] = Gen.oneOf(
    "the ",
    "cat ",
    "sat ",
    "teh ",
    "wurld ",
    "Hello ",
    "NASA ",
    "hyphen-ated ",
    "cat-dog ",
    "don't ",
    "COVID-19 ",
    ". ",
    "\n",
    "\n\n",
    "```\n",
    "~~~\n",
    "---\n",
    "...\n",
    "`code` ",
    "https://x.y/z ",
    "<b>",
    "[a](u) "
  )

  private val genDocument: Gen[String] = Gen.listOf(tokens).map(_.mkString)

  private val genEdit: Gen[Edit] =
    val inserted = Gen.oneOf(tokens, Gen.alphaChar.map(_.toString), Gen.const("\n"), Gen.const(" "))
    Gen.oneOf(
      for at <- Gen.posNum[Int]; text <- inserted yield Edit.Insert(at, text),
      for from <- Gen.posNum[Int]; length <- Gen.choose(0, 40) yield Edit.Delete(from, length)
    )

  "Incremental spell check" should "publish what a full check of the edited document finds, after any edits" in
    forAll(genDocument, Gen.listOfN(8, genEdit), minSuccessful(300)) { (text, edits) =>
      val start = Rope(text)
      edits.foldLeft((start, stateWith(start))) {
        case ((rope, state), edit) =>
          val next    = edit.applyTo(rope)
          val checked = refreshed(edited(state, next))
          withClue(s"after $edit on ${rope.collect()}: ")(published(checked) shouldBe fullAnalysis(next))
          (next, checked)
      }
    }

  it should "move the diagnostics below inserted and deleted lines with their lines" in {
    val before   = Rope("the cat\nteh\nthe wurld\nthe dog")
    val state    = stateWith(before)
    val inserted = before.insert(0, "a\nthe\n").getOrElse(before)
    val grown    = refreshed(edited(state, inserted))
    published(grown).map(_.range.start.line) shouldBe List(3, 4)
    published(grown) shouldBe fullAnalysis(inserted)

    val deleted = inserted.deleteRight(0, 6).getOrElse(inserted)
    published(refreshed(edited(grown, deleted))).map(_.range.start.line) shouldBe List(1, 2)
  }

  it should "stop and start checking a fenced block as its fence is deleted and typed" in {
    val fenced = Rope("teh\n```\nteh\nteh\n```\nteh")
    val state  = stateWith(fenced)
    published(state).map(_.range.start.line) shouldBe List(0, 5)

    val unfenced = fenced.deleteRight(4, 4).getOrElse(fenced)
    val opened   = refreshed(edited(state, unfenced))
    published(opened).map(_.range.start.line) shouldBe List(0, 1, 2)
    published(opened) shouldBe fullAnalysis(unfenced)

    val refenced = unfenced.insert(4, "```\n").getOrElse(unfenced)
    published(refreshed(edited(opened, refenced))) shouldBe fullAnalysis(refenced)
  }

  it should "not collect the document to re-check a one-character edit" in {
    val before = Rope("the cat\n" * 200 + "wurld\n" + "the dog\n" * 200)
    val state  = stateWith(before)
    val after  = before.insert(10, "x").getOrElse(before)

    val checked = refreshed(edited(state, UncollectableRope(after.collect())))

    published(checked) shouldBe fullAnalysis(after)
  }

  it should "check a document again when the dictionary changes under it" in {
    val content  = Rope("the cat\nhello wurld")
    val state    = stateWith(content)
    val narrower = dictionary.copy(words = dictionary.words - "hello")
    val reinstalled =
      SpellCheckDictionaryFingerprint("/dictionaries/en.dic", exists = true, isDirectory = false, 9L, 2L)
    val rechecked = SpellChecker.refreshDiagnostics(state, DictionarySnapshot(narrower, List(reinstalled)))

    published(rechecked) shouldBe SpellChecker.analyzeText(content.collect(), config, narrower)
    published(rechecked).map(_.range.start.line) shouldBe List(1, 1)
  }

  it should "hold nothing for a buffer that was closed" in {
    val state  = stateWith(Rope("the wurld"))
    val closed = state.copy(persisted = state.persisted.copy(buffers = Map.empty, bufferOrder = Nil))

    refreshed(closed).runtime.languageService.diagnosticsState.spellCheckCache shouldBe empty
  }

  it should "hold nothing once spell check is turned off" in {
    val state = stateWith(Rope("the wurld"))
    val off = state.copy(persisted =
      state.persisted.copy(config = state.persisted.config.withSpellCheck(SpellCheckConfig(enabled = false)))
    )

    refreshed(off).runtime.languageService.diagnosticsState.spellCheckCache shouldBe empty
  }
end SpellCheckerIncrementalSpec
