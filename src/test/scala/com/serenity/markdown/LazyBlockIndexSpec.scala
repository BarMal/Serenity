package com.serenity.markdown

import com.serenity.markdown.MarkdownBlockLens.{BlockIndex, FenceRangeIndex}
import com.serenity.rope.{Balance, Rope}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class LazyBlockIndexSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given Balance = Balance.default

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 150)

  private val vocabulary = Vector(
    "```scala",
    "```",
    "~~~",
    "plain text",
    "more text",
    "",
    "",
    "| a | b |",
    "| --- | --- |",
    "| 1 | 2 |",
    "Heading",
    "---",
    "===",
    "# hash heading",
    "- item",
    "> quote"
  )

  private val lineGen: Gen[String] = Gen.oneOf(vocabulary)
  private def linesGen(max: Int): Gen[Vector[String]] =
    Gen.choose(0, max).flatMap(Gen.listOfN(_, lineGen).map(_.toVector))

  private def ropeOf(lines: Vector[String]): Rope = Rope(lines.mkString("\n"))

  private def textLines(rope: Rope): Vector[String] = rope.linesIteratorFrom(0).map(_._2).toVector

  private def agreesWithFullParse(index: BlockIndex, rope: Rope): Unit =
    val full = MarkdownBlockLens.fenceRangeIndex(rope.linesIteratorFrom(0).map(_._2))
    (0 until rope.lineCount.max(1)).foreach { line =>
      withClue(s"line $line of ${textLines(rope)}: ") {
        index.rangeAt(line) shouldBe full.rangeAt(line)
        index.tableAt(line) shouldBe full.tableAt(line)
        index.tableRowAt(line) shouldBe full.tableRowAt(line)
        index.setextHeadingAt(line) shouldBe full.setextHeadingAt(line)
      }
    }

  "A lazy block index" should "answer for every line as a scan of the whole text does" in
    forAll(linesGen(60))(lines => agreesWithFullParse(LazyBlockIndex.of(ropeOf(lines)), ropeOf(lines)))

  it should "answer the same whichever lines are asked about first" in
    forAll(linesGen(60), Gen.listOfN(8, Gen.choose(0, 70))) { (lines, order) =>
      val rope  = ropeOf(lines)
      val index = LazyBlockIndex.of(rope)
      val full  = MarkdownBlockLens.fenceRangeIndex(rope.linesIteratorFrom(0).map(_._2))
      order.foreach(line => index.rangeAt(line) shouldBe full.rangeAt(line))
      agreesWithFullParse(index, rope)
    }

  it should "list the tables that overlap a range of lines" in
    forAll(linesGen(60), Gen.choose(0, 60), Gen.choose(0, 20)) { (lines, from, length) =>
      val rope  = ropeOf(lines)
      val full  = MarkdownBlockLens.fenceRangeIndex(rope.linesIteratorFrom(0).map(_._2))
      val range = from to (from + length)
      LazyBlockIndex.of(rope).tablesIn(range) shouldBe full.tablesIn(range)
    }

  it should "resume from an edit and still agree with a scan of the edited text" in {
    val edits = for
      before <- linesGen(80)
      from   <- Gen.choose(0, before.length)
      cut    <- Gen.choose(0, 5)
      added  <- linesGen(6)
      warmed <- Gen.listOfN(5, Gen.choose(0, 90))
    yield (before, from, cut, added, warmed)
    forAll(edits) { (before, from, cut, added, warmed) =>
      val original = LazyBlockIndex.of(ropeOf(before))
      warmed.foreach(original.rangeAt)
      val after  = before.take(from) ++ added ++ before.drop(from + cut)
      val edited = ropeOf(after)
      agreesWithFullParse(original.after(edited, from.min(edited.lineCount)), edited)
    }
  }

  it should "resume far below the top across several marks" in {
    val long     = Vector.tabulate(1_300)(line => if line % 97 == 0 then "```" else s"text $line")
    val original = LazyBlockIndex.of(ropeOf(long))
    original.rangeAt(1_250)
    val edited = ropeOf(long.updated(700, "```"))

    agreesWithFullParse(original.after(edited, 700), edited)
  }

  it should "read only as far down as the lines asked about when the text above settles them" in {
    val document = ropeOf(Vector.tabulate(5_000)(line => s"# Heading $line"))
    val index    = LazyBlockIndex.of(document)

    index.rangeAt(4) shouldBe None
    index.tableAt(4) shouldBe None

    index.linesRead should be < 20
  }

  it should "read to the end of a fence the asked line is inside, and no further" in {
    val document = ropeOf(
      Vector.fill(100)("prose") ++ Vector("```") ++ Vector.fill(300)("code") ++ Vector("```") ++ Vector.fill(2_000)(
        "# tail"
      )
    )
    val index = LazyBlockIndex.of(document)

    index.rangeAt(150) shouldBe Some(100 to 401)

    index.linesRead should be < 450
  }

  it should "treat an unclosed fence as no block, reading to the end to know" in {
    val document = ropeOf(Vector("```", "code", "more"))
    val index    = LazyBlockIndex.of(document)

    index.rangeAt(1) shouldBe None
    index.rangeAt(1) shouldBe MarkdownBlockLens.fenceRangeIndex(Iterator("```", "code", "more")).rangeAt(1)
  }

  it should "keep what it read above an edit and say how far down it agrees with its predecessor" in {
    val lines    = Vector.tabulate(2_000)(line => s"# Heading $line")
    val original = LazyBlockIndex.of(ropeOf(lines))
    original.rangeAt(1_000)
    val edited = original.after(ropeOf(lines.updated(900, "changed")), 900)

    edited.follows(original) shouldBe true
    edited.agreesBefore shouldBe 768
    edited.linesRead shouldBe 768
    edited.rangeAt(1_000) shouldBe None
    edited.linesRead should be < 1_010
  }

  it should "agree with an eager index built by FenceRangeIndex for the hand-made cases" in {
    val fenceIndex: FenceRangeIndex = FenceRangeIndex(Vector(1 to 3), Vector(5 to 7))
    fenceIndex.tablesIn(6 to 6) shouldBe Vector(5 to 7)
    fenceIndex.tablesIn(8 to 9) shouldBe Vector.empty
  }
end LazyBlockIndexSpec
