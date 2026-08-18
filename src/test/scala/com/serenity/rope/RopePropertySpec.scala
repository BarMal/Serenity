package com.serenity.rope

import com.serenity.testkit.Generators
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** Representation-invariance coverage for [[Rope]].
  *
  * `docs/coding-standards.md` asks that rope behaviour be tested independently of tree shape. `RopeSpec` covers
  * behaviour against hand-built fixtures, which can only exercise the shapes someone thought to write down. These
  * properties generate the shapes instead, and state every expectation in terms of the `String` the rope was built
  * from, so they assert what the rope should do rather than restating how it does it.
  */
class RopePropertySpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  given Balance = Balance.default

  /** Guards every other property in this file.
    *
    * All the shape-independence claims below are vacuous if the generator only ever emits flat leaves, and a degenerate
    * generator fails silently -- the suite still passes, having tested nothing. This asserts the generated trees
    * genuinely vary in depth and internal structure.
    */
  property("the shape generator produces genuinely varied trees") {
    val samples = Gen
      .listOfN(200, Generators.ropeOfShape("the quick brown fox jumps over the lazy dog"))
      .sample
      .getOrElse(Nil)

    samples should not be empty
    samples.map(_.height).distinct.size should be > 3
    samples.exists(_.height > 3) shouldBe true
    // Content is invariant across all of them, which is the premise the rest of the file rests on.
    samples.map(_.collect()).distinct shouldBe List("the quick brown fox jumps over the lazy dog")
  }

  property("collect returns the text the rope was built from, whatever its shape") {
    forAll(Generators.ropeWithText)((rope, text) => rope.collect() shouldBe text)
  }

  property("weight is the text length, whatever its shape") {
    forAll(Generators.ropeWithText)((rope, text) => rope.weight shouldBe text.length)
  }

  property("index agrees with String.charAt at every valid offset") {
    forAll(Generators.ropeWithText) { (rope, text) =>
      text.indices.foreach(i => rope.index(i) shouldBe Some(text.charAt(i)))
    }
  }

  property("index is None outside the text") {
    forAll(Generators.ropeWithText) { (rope, text) =>
      rope.index(-1) shouldBe None
      rope.index(text.length) shouldBe None
    }
  }

  property("two ropes over the same text agree on every observation, however differently shaped") {
    forAll(Generators.differentlyShapedRopes) { (left, right, text) =>
      left.collect() shouldBe right.collect()
      left.weight shouldBe right.weight
      left.newlineCount shouldBe right.newlineCount
      left.lastLineLength shouldBe right.lastLineLength
      left.endsWithNewline shouldBe right.endsWithNewline
      left.lineCount shouldBe right.lineCount
      (0 until text.length).foreach(i => left.index(i) shouldBe right.index(i))
    }
  }

  property("rebalance preserves content and yields a balanced tree") {
    forAll(Generators.ropeWithText) { (rope, text) =>
      val rebalanced = rope.rebalance
      rebalanced.collect() shouldBe text
      rebalanced.rebuild.isHeightBalanced shouldBe true
      rebalanced.rebuild.isWeightBalanced shouldBe true
    }
  }

  property("splitAt partitions the text and recombines to the original") {
    forAll(Generators.ropeWithText) { (rope, text) =>
      (0 to text.length).foreach { at =>
        rope.splitAt(at) match
          case Some((prefix, suffix)) =>
            prefix.collect() shouldBe text.take(at)
            suffix.collect() shouldBe text.drop(at)
            prefix.concat(suffix).collect() shouldBe text
          case None =>
            fail(s"splitAt($at) returned None for a rope of weight ${rope.weight}")
      }
    }
  }

  property("splitAt is None outside the text") {
    forAll(Generators.ropeWithText) { (rope, text) =>
      rope.splitAt(-1) shouldBe None
      rope.splitAt(text.length + 1) shouldBe None
    }
  }

  property("insert matches String insertion at every valid offset") {
    forAll(Generators.ropeWithText, Generators.genText) {
      case ((rope, text), inserted) =>
        (0 to text.length).foreach { at =>
          rope.insert(at, inserted).collect() shouldBe (text.take(at) + inserted + text.drop(at))
        }
    }
  }

  property("delete matches String removal over every valid range") {
    forAll(Generators.ropeWithText) { (rope, text) =>
      (0 to text.length).foreach { start =>
        (start to text.length).foreach { end =>
          rope.delete(start, end).collect() shouldBe (text.take(start) + text.drop(end))
        }
      }
    }
  }

  property("slice matches String.substring over every valid range") {
    forAll(Generators.ropeWithText) { (rope, text) =>
      (0 to text.length).foreach { start =>
        (start to text.length).foreach(end => rope.sliceString(start, end) shouldBe text.substring(start, end))
      }
    }
  }

  /** Search terms drawn from the text itself as well as at random, so the property exercises hits and misses rather
    * than almost always confirming "no matches found".
    */
  private def ropeAndSearchTerm: Gen[(Rope, String, String)] =
    for
      (rope, text) <- Generators.ropeWithText
      term <-
        if text.isEmpty then Gen.nonEmptyListOf(Gen.alphaNumChar).map(_.mkString)
        else
          Gen.frequency(
            3 -> Gen
              .chooseNum(0, text.length - 1)
              .flatMap(start => Gen.chooseNum(start + 1, text.length).map(end => text.substring(start, end))),
            1 -> Gen.nonEmptyListOf(Gen.alphaNumChar).map(_.mkString)
          )
    yield (rope, text, term)

  property("searchAll finds exactly the offsets String.indexOf finds") {
    forAll(ropeAndSearchTerm) { (rope, text, term) =>
      def expected(from: Int, found: List[Int]): List[Int] =
        if from > text.length then found.reverse
        else
          text.indexOf(term, from) match
            case -1     => found.reverse
            case offset => expected(offset + 1, offset :: found)

      rope.searchAll(term) shouldBe expected(0, Nil)
    }
  }

  property("getLine agrees with splitting the text on newlines") {
    forAll(Generators.genMultilineText.flatMap(t => Generators.ropeOfShape(t).map(_ -> t))) { (rope, text) =>
      val lines = text.split("\n", -1).toVector
      rope.lineCount shouldBe lines.length
      lines.indices.foreach(i => rope.getLine(i) shouldBe Some(lines(i)))
    }
  }

  property("lineColumnToOffset and offsetToLineColumn are inverse over every valid offset") {
    forAll(Generators.genMultilineText.flatMap(t => Generators.ropeOfShape(t).map(_ -> t))) { (rope, text) =>
      (0 to text.length).foreach { offset =>
        val (line, column) = rope.offsetToLineColumn(offset)
        rope.lineColumnToOffset(line, column) shouldBe offset
      }
    }
  }
