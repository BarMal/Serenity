package com.serenity.rope

import com.serenity.testkit.Generators
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** `RopeDiff.changedOffsetRange` only earns its keep as a damage producer if it never under-reports: everything outside
  * the returned range must be provably identical between `before` and `after`, whatever shape the rope tree happens to
  * be in and however a rebalance may have widened the report. These properties check that directly against the
  * `String`s the ropes were built from, rather than against `RopeDiff`'s own reasoning about itself.
  */
class RopeDiffPropertySpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  given Balance = Balance.default

  private def assertNeverUnderReports(before: Rope, beforeText: String, after: Rope, afterText: String): Unit =
    RopeDiff.changedOffsetRange(before, after) match
      case None =>
        beforeText shouldBe afterText
      case Some((start, end)) =>
        start should be >= 0
        end should be >= start
        end should be <= after.weight
        val suffixLen = after.weight - end
        beforeText.take(start) shouldBe afterText.take(start)
        beforeText.takeRight(suffixLen) shouldBe afterText.takeRight(suffixLen)

  /** The range a character-by-character comparison of the two texts gives: the longest common prefix, then the longest
    * common suffix of what remains.
    */
  private def exactRange(beforeText: String, afterText: String): Option[(Int, Int)] =
    val bound  = math.min(beforeText.length, afterText.length)
    val prefix = (0 until bound).find(i => beforeText(i) != afterText(i)).getOrElse(bound)
    if prefix == bound && beforeText.length == afterText.length then None
    else
      val suffixBound = bound - prefix
      val suffix =
        (0 until suffixBound)
          .find(i => beforeText(beforeText.length - 1 - i) != afterText(afterText.length - 1 - i))
          .getOrElse(suffixBound)
      Some((prefix, afterText.length - suffix))

  property("an insert or delete at any offset reports exactly the common prefix and suffix") {
    forAll(Generators.ropeWithText, Generators.genText) {
      case ((before, beforeText), inserted) =>
        forAll(Gen.chooseNum(0, beforeText.length), Gen.chooseNum(0, 3)) { (at, deleted) =>
          val removed   = before.deleteRight(at, math.min(deleted, beforeText.length - at)).getOrElse(fail("delete"))
          val after     = removed.insert(at, inserted).getOrElse(fail("expected insert to succeed"))
          val afterText = beforeText.take(at) + inserted + beforeText.drop(at + deleted)
          RopeDiff.changedOffsetRange(before, after) shouldBe exactRange(beforeText, afterText)
        }
    }
  }

  property("a run of single-character edits on a large rope reports exactly the common prefix and suffix") {
    val start = Rope((1 to 400).map(i => s"line $i of the document").mkString("\n"))
    (1 to 60).foldLeft((start, start.collect())) {
      case ((rope, text), step) =>
        val at        = (step * 977) % (rope.weight + 1)
        val edited    = if step % 2 == 0 then rope.insert(at, "x") else rope.deleteRight(at, 1)
        val after     = edited.getOrElse(fail("expected the edit to succeed"))
        val afterText = after.collect()
        RopeDiff.changedOffsetRange(rope, after) shouldBe exactRange(text, afterText)
        (after, afterText)
    }
  }

  property("an insert at any offset never under-reports") {
    forAll(Generators.ropeWithText, Generators.genText) {
      case ((before, beforeText), inserted) =>
        forAll(Gen.chooseNum(0, beforeText.length)) { at =>
          val after     = before.insert(at, inserted).getOrElse(fail("expected insert to succeed"))
          val afterText = beforeText.take(at) + inserted + beforeText.drop(at)
          assertNeverUnderReports(before, beforeText, after, afterText)
        }
    }
  }

  property("a delete over any range never under-reports") {
    forAll(Generators.ropeWithText) {
      case (before, beforeText) =>
        forAll(Gen.chooseNum(0, beforeText.length)) { start =>
          forAll(Gen.chooseNum(start, beforeText.length)) { end =>
            val after     = before.delete(start, end).getOrElse(fail("expected delete to succeed"))
            val afterText = beforeText.take(start) + beforeText.drop(end)
            assertNeverUnderReports(before, beforeText, after, afterText)
          }
        }
    }
  }

  property("two differently-shaped ropes over the same content report no change") {
    forAll(Generators.differentlyShapedRopes) {
      case (left, right, text) =>
        RopeDiff.changedOffsetRange(left, right) shouldBe None
    }
  }

  property("a run of edits never under-reports at any step, even once rebalancing kicks in") {
    forAll(Generators.genText) { seed =>
      val start = Rope(if seed.isEmpty then "seed" else seed)
      (1 to 30).foldLeft((start, start.collect())) {
        case ((rope, text), step) =>
          val at              = step % (rope.weight + 1)
          val insertion       = if step % 3 == 0 then "" else s"edit-$step"
          val deleted         = math.min(2, rope.weight - at)
          val afterDelete     = rope.deleteRight(at, deleted).getOrElse(fail("expected deleteRight to succeed"))
          val afterDeleteText = text.take(at) + text.drop(at + deleted)
          assertNeverUnderReports(rope, text, afterDelete, afterDeleteText)

          val after     = afterDelete.insert(at, insertion).getOrElse(fail("expected insert to succeed"))
          val afterText = afterDeleteText.take(at) + insertion + afterDeleteText.drop(at)
          assertNeverUnderReports(afterDelete, afterDeleteText, after, afterText)

          (after, afterText)
      }
      succeed
    }
  }
