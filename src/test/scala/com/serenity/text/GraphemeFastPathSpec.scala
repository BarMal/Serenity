package com.serenity.text

import java.util.concurrent.atomic.AtomicInteger

import com.ibm.icu.text.BreakIterator
import com.serenity.rope.{
  Balance,
  Rope,
  RopeCharacterSource,
  graphemeBoundaryAfterOrAt,
  graphemeBoundaryBeforeOrAt,
  nextGraphemeBoundary,
  previousGraphemeBoundary
}
import com.serenity.text.TextEditing.CharacterSource
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** The ASCII fast path and the shared rope adapter must answer exactly as UAX #29 does, and spare the `BreakIterator`
  * (#1921).
  */
class GraphemeFastPathSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers:

  given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 3)

  private val pieces = Vector(
    "a",
    "Z",
    " ",
    "\r",
    "\n",
    "\r\n",
    "́",
    "‍",
    "️",
    "é",
    "é",
    "👨",
    "👨‍👩‍👧",
    "🇫🇷",
    "각",
    "क्ष",
    "©",
    "̀"
  )

  private val texts: Gen[String] = Gen.listOf(Gen.oneOf(pieces)).map(_.mkString)

  private def breakIteratorOver(text: String): BreakIterator =
    val iterator = BreakIterator.getCharacterInstance()
    iterator.setText(text)
    iterator

  "Grapheme boundaries" should "match the BreakIterator at every offset, on any rope shape" in
    forAll(texts) { generated =>
      val rope = Rope(generated)
      // A rope may normalise line endings, so the reference is the text it actually holds.
      val text     = rope.collect()
      val iterator = breakIteratorOver(text)
      (0 to text.length).foreach { index =>
        val following = if index >= text.length then text.length else iterator.following(index)
        val preceding = if index <= 0 then 0 else iterator.preceding(index)
        withClue(s"text ${text.map(c => f"u${c.toInt}%04x").mkString} at $index: ") {
          rope.nextGraphemeBoundary(index) shouldBe following
          rope.previousGraphemeBoundary(index) shouldBe preceding
          TextEditing.nextGraphemeBoundary(text, index) shouldBe following
          TextEditing.previousGraphemeBoundary(text, index) shouldBe preceding
        }
      }
    }

  they should "agree with isBoundary for the before-or-at and after-or-at queries" in
    forAll(texts.suchThat(_.nonEmpty)) { generated =>
      val rope     = Rope(generated)
      val text     = rope.collect()
      val iterator = breakIteratorOver(text)
      (0 to text.length).foreach { index =>
        val onBoundary = iterator.isBoundary(index)
        (rope.graphemeBoundaryBeforeOrAt(index) == index) shouldBe onBoundary
        (rope.graphemeBoundaryAfterOrAt(index) == index) shouldBe onBoundary
        TextEditing.isWholeGraphemeRange(text, index, index) shouldBe onBoundary
      }
    }

  private def probed(text: String): (CharacterSource, AtomicInteger) =
    val anchorReads = new AtomicInteger(0)
    val source = new CharacterSource:
      def length: Int              = text.length
      def charAt(index: Int): Char = text.charAt(index)
      override def identityAnchor: AnyRef =
        anchorReads.incrementAndGet()
        this
    (source, anchorReads)

  "Boundaries between two ASCII characters" should "be decided without consulting the BreakIterator" in {
    val (source, anchorReads) = probed("plain ascii text")

    TextEditing.nextGraphemeBoundary(source, 3) shouldBe 4
    TextEditing.previousGraphemeBoundary(source, 9) shouldBe 8
    TextEditing.graphemeBoundaryBeforeOrAt(source, 5) shouldBe 5
    TextEditing.graphemeBoundaryAfterOrAt(source, 5) shouldBe 5
    TextEditing.nextGraphemeBoundary(source, 15) shouldBe 16
    TextEditing.previousGraphemeBoundary(source, 1) shouldBe 0
    anchorReads.get() shouldBe 0
  }

  "A CR LF pair" should "stay one grapheme, which only the BreakIterator is asked about" in {
    val (source, anchorReads) = probed("ab\r\ncd")

    TextEditing.nextGraphemeBoundary(source, 2) shouldBe 4
    TextEditing.previousGraphemeBoundary(source, 4) shouldBe 2
    TextEditing.graphemeBoundaryBeforeOrAt(source, 3) shouldBe 2
    TextEditing.graphemeBoundaryAfterOrAt(source, 3) shouldBe 4
    anchorReads.get() should be > 0
  }

  "The adapter for a rope" should "be built once for repeated operations on that rope" in {
    val rope = Rope("ábc")

    RopeCharacterSource.of(rope) should be theSameInstanceAs RopeCharacterSource.of(rope)
    RopeCharacterSource.of(Rope("other")) should not be theSameInstanceAs(RopeCharacterSource.of(rope))
  }

  it should "let repeated boundary questions on one rope skip setText on the BreakIterator" in {
    val rope = Rope("ééé")

    rope.nextGraphemeBoundary(0) shouldBe 2
    val text = TextEditing.graphemeBreakIterator(RopeCharacterSource.of(rope)).getText()

    rope.previousGraphemeBoundary(4) shouldBe 2
    rope.graphemeBoundaryAfterOrAt(1) shouldBe 2
    rope.graphemeBoundaryBeforeOrAt(3) shouldBe 2

    TextEditing.graphemeBreakIterator(RopeCharacterSource.of(rope)).getText() should be theSameInstanceAs text
  }

  it should "never show one thread's rope to another running at the same time" in {
    val failures = new AtomicInteger(0)
    val workers = (0 until 8).map { n =>
      val text = ("éx" * (50 + n)) + "👨‍👩"
      val rope = Rope(text)
      val worker = new Thread(() =>
        val expected = breakIteratorOver(text)
        (0 until 5).foreach(_ =>
          (0 until text.length).foreach(index =>
            if rope.nextGraphemeBoundary(index) != expected.following(index) then failures.incrementAndGet()
          )
        )
      )
      worker.start()
      worker
    }
    workers.foreach(_.join())

    failures.get() shouldBe 0
  }
