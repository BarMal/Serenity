package com.serenity.rope

import com.serenity.testkit.Generators
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Offset and line/column conversion against a plain-`String` reference, over arbitrary tree shapes, with surrogate
  * pairs and lines that begin or end at leaf boundaries. The batch conversion must agree with converting one by one.
  */
class RopeLineConversionSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  given Balance = Balance.default

  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 300)

  private val genText: Gen[String] =
    Gen
      .listOf(
        Gen.frequency(8 -> Gen.const("a"), 2 -> Gen.const(" "), 3 -> Gen.const("\n"), 1 -> Gen.const("😀"))
      )
      .map(_.mkString)

  private val genRopeWithText: Gen[(Rope, String)] =
    genText.flatMap(text => Generators.ropeOfShape(text).map((_, text)))

  private def referenceLineColumn(text: String, offset: Int): (Int, Int) =
    val before = text.take(offset)
    (before.count(_ == '\n'), offset - (before.lastIndexOf('\n') + 1))

  private def referenceOffset(text: String, line: Int, column: Int): Int =
    val lines = text.split("\n", -1)
    if line >= lines.length then text.length
    else
      val lineStart = lines.take(line).map(_.length + 1).sum
      lineStart + math.min(column, lines(line).length)

  property("offsetToLineColumn agrees with a String reference at every offset") {
    forAll(genRopeWithText) { (rope, text) =>
      (0 to text.length).foreach(offset => rope.offsetToLineColumn(offset) shouldBe referenceLineColumn(text, offset))
    }
  }

  property("lineColumnToOffset agrees with a String reference, clamping a column past the end of its line") {
    forAll(genRopeWithText) { (rope, text) =>
      val lineCount = text.count(_ == '\n') + 1
      for
        line   <- 0 to lineCount
        column <- List(0, 1, 2, 3, 4, Int.MaxValue)
      do rope.lineColumnToOffset(line, column) shouldBe referenceOffset(text, line, column)
    }
  }

  property("offsetsToLineColumns equals converting each offset on its own") {
    val genCase = genRopeWithText.flatMap((rope, text) => Gen.listOf(Gen.choose(0, text.length)).map((rope, _)))
    forAll(genCase) { (rope, offsets) =>
      val ascending = offsets.sorted.toIndexedSeq
      rope.offsetsToLineColumns(ascending) shouldBe ascending.map(rope.offsetToLineColumn).toVector
    }
  }

  property("offsetsToLineColumns on an empty list is empty") {
    Rope("a\nb").offsetsToLineColumns(IndexedSeq.empty) shouldBe Vector.empty
  }

  property("offsetsToLineColumns repeats offsets that coincide, at the start, middle and end of the text") {
    forAll(genRopeWithText) { (rope, text) =>
      val offsets = IndexedSeq(0, 0, text.length / 2, text.length / 2, text.length, text.length)
      rope.offsetsToLineColumns(offsets) shouldBe offsets.map(rope.offsetToLineColumn).toVector
    }
  }

  property("a rope built from CRLF text converts as the LF text it was normalised to") {
    val rope = Rope("ab\r\ncd\r\n")
    rope.offsetsToLineColumns(IndexedSeq(0, 2, 3, 5, 6)) shouldBe Vector((0, 0), (0, 2), (1, 0), (1, 2), (2, 0))
  }
