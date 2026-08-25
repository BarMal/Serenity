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

  property("an insert at any offset never under-reports") {
    forAll(Generators.ropeWithText, Generators.genText) {
      case ((before, beforeText), inserted) =>
        forAll(Gen.chooseNum(0, beforeText.length)) { at =>
          val after     = before.insert(at, inserted)
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
            val after     = before.delete(start, end)
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
          val afterDelete     = rope.deleteRight(at, deleted)
          val afterDeleteText = text.take(at) + text.drop(at + deleted)
          assertNeverUnderReports(rope, text, afterDelete, afterDeleteText)

          val after     = afterDelete.insert(at, insertion)
          val afterText = afterDeleteText.take(at) + insertion + afterDeleteText.drop(at)
          assertNeverUnderReports(afterDelete, afterDeleteText, after, afterText)

          (after, afterText)
      }
      succeed
    }
  }
