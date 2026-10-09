package com.serenity.rope

import com.serenity.testkit.Generators
import com.serenity.text.{TextCounts, TextStatistics}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Word and character counts (#1949): UAX#29 words with CJK counted per character, grapheme-cluster characters, and the
  * rope keeping both incrementally across leaf joins.
  */
class RopeWordCountSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given Balance = Balance.default

  private val family = "👨‍👩‍👧"

  "A spaceless Chinese sentence" should "count one word per character" in {
    Rope("我们今天").wordCount shouldBe 4
    TextStatistics.of(Rope("我们今天")).wordCount shouldBe 4
  }

  "Latin words next to Chinese" should "count each Latin word once and each Chinese character once" in {
    TextStatistics.of(Rope("hello 世界")).wordCount shouldBe 3
  }

  "An emoji ZWJ sequence" should "count as one character and no words" in {
    val stats = TextStatistics.of(Rope(family))

    stats.characterCount shouldBe 1
    stats.characterCountExcludingWhitespace shouldBe 1
    stats.wordCount shouldBe 0
  }

  "A rich document's inline atom placeholder" should "separate words and not count as a non-whitespace character" in {
    val stats = TextStatistics.of(Rope("ab￼cd"))

    stats.wordCount shouldBe 2
    stats.characterCountExcludingWhitespace shouldBe 4
  }

  "A rich document's block placeholder line" should "count no word and no non-whitespace character" in {
    val stats = TextStatistics.of(Rope("ab\n\u2064\ncd"))

    stats.wordCount shouldBe 2
    stats.characterCountExcludingWhitespace shouldBe 4
  }

  "Markdown syntax and punctuation" should "not count as words" in {
    TextStatistics.of(Rope("# Title\n\n- **bold** item ---")).wordCount shouldBe 3
  }

  "A hyphenated compound" should "count as one word, as it did under whitespace counting" in {
    TextStatistics.of(Rope("a well-known state-of-the-art fact")).wordCount shouldBe 4
  }

  private val tokens: Gen[String] = Gen.oneOf(
    Gen.alphaNumChar.map(_.toString),
    Gen.oneOf(" ", "  ", "\n", "\t", ".", ",", "-", "'", "_", ":", "3.14", "e.g."),
    Gen.oneOf("é", "café", "naïve", "Привет", "한국어"),
    Gen.oneOf("我", "们", "今", "天", "東京", "ひらがな", "カタカナ", "食べる"),
    Gen.oneOf(family, "🇺🇸", "❤️", "👍🏽")
  )

  private val genText: Gen[String] = Gen.listOf(tokens).map(_.mkString)

  private enum Edit:
    case Insert(at: Double, text: String)
    case Delete(at: Double, length: Int)

  private val genEdit: Gen[Edit] = Gen.oneOf(
    for
      at   <- Gen.choose(0.0, 1.0)
      text <- genText
    yield Edit.Insert(at, text),
    for
      at     <- Gen.choose(0.0, 1.0)
      length <- Gen.choose(0, 12)
    yield Edit.Delete(at, length)
  )

  private def applied(rope: Rope, edit: Edit): Rope =
    edit match
      case Edit.Insert(at, text)   => rope.insert((rope.weight * at).toInt, text).getOrElse(rope)
      case Edit.Delete(at, length) => rope.deleteRight((rope.weight * at).toInt, length).getOrElse(rope)

  private val ropeAfterEdits: Gen[Rope] =
    for
      text  <- genText
      rope  <- Generators.ropeOfShape(text)
      edits <- Gen.listOf(genEdit)
    yield edits.foldLeft(rope)(applied)

  "Incremental counts" should "equal a full recount of the text, whatever the leaf splits and edits" in
    forAll(ropeAfterEdits, minSuccessful(300))(rope => rope.textSummary.counts shouldBe TextCounts.of(rope.collect()))

  "Incremental counts over Thai" should "stay within one word per leaf join of a full recount" in {
    val thai = Gen.listOf(Gen.oneOf("สวัสดี", "ครับ", "ภาษาไทย", "ง่าย", " ", "\n")).map(_.mkString)
    val ropeWithText =
      for
        text <- thai
        rope <- Generators.ropeOfShape(text)
      yield (rope, text)

    forAll(ropeWithText) { (rope, text) =>
      val joins = math.max(0, rope.leafValues.size - 1)
      val full  = TextCounts.of(text)

      math.abs(rope.textSummary.counts.words - full.words) should be <= joins
      rope.textSummary.counts.characters shouldBe full.characters
    }
  }
