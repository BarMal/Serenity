package com.serenity.rope

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Hand-built fixtures for the shapes worth naming explicitly; `RopeDiffPropertySpec` covers the general claim (the
  * reported range always covers the true edit) over generated ropes and edits.
  */
class RopeDiffSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "changedOffsetRange" should "be None for the same rope reference" in {
    val rope = Rope("hello world")
    RopeDiff.changedOffsetRange(rope, rope) shouldBe None
  }

  it should "be None for two ropes with identical content but different objects" in {
    val before = Rope("hello world")
    val after  = Rope("hello world")
    RopeDiff.changedOffsetRange(before, after) shouldBe None
  }

  it should "report the exact single-character range for a mid-document insert" in {
    val before = Rope("helloworld")
    val after  = before.insert(5, "X").getOrElse(fail("expected insert to succeed"))
    RopeDiff.changedOffsetRange(before, after) shouldBe Some((5, 6))
  }

  it should "report only the appended range when text is added at the end" in {
    val before = Rope("hello")
    val after  = before.insert(before.weight, " world").getOrElse(fail("expected insert to succeed"))
    RopeDiff.changedOffsetRange(before, after) shouldBe Some((5, 11))
  }

  it should "report only the prepended range when text is added at the start" in {
    val before = Rope("world")
    val after  = before.insert(0, "hello ").getOrElse(fail("expected insert to succeed"))
    RopeDiff.changedOffsetRange(before, after) shouldBe Some((0, 6))
  }

  it should "report an empty range at the deletion point for a mid-document delete" in {
    val before = Rope("helloXworld")
    val after  = before.delete(5, 6).getOrElse(fail("expected delete to succeed"))
    RopeDiff.changedOffsetRange(before, after) shouldBe Some((5, 5))
  }

  it should "report the whole new content when the two ropes share no prefix or suffix" in {
    val before = Rope("aaaa")
    val after  = Rope("bbbb")
    RopeDiff.changedOffsetRange(before, after) shouldBe Some((0, 4))
  }

  it should "report the whole document as changed against an empty before" in {
    val before = Rope("")
    val after  = Rope("hello")
    RopeDiff.changedOffsetRange(before, after) shouldBe Some((0, 5))
  }

  it should "report the whole document as changed against an empty after" in {
    val before = Rope("hello")
    val after  = Rope("")
    RopeDiff.changedOffsetRange(before, after) shouldBe Some((0, 0))
  }

  it should "still cover the edit when it triggers a leaf rebuild" in {
    // Balance.default.leafChunkSize is 1000: growing a leaf past that forces Leaf.rebalance to rebuild via Rope.apply,
    // constructing fresh Leaf objects for the whole subtree even though only one character actually changed.
    val before = Rope("a" * 999)
    val after  = before.insert(500, "X").getOrElse(fail("expected insert to succeed"))

    after.weight shouldBe 1000
    val range = RopeDiff.changedOffsetRange(before, after)
    range shouldBe defined
    val Some((start, end)) = range: @unchecked
    // The walk may over-report once a rebuild replaces leaf identities, but it must never under-report: position 500
    // (where the actual edit landed) has to fall inside the reported range.
    start should be <= 500
    end should be >= 501
  }

  it should "cover a delete that spans a rebalance-triggering edit" in {
    val beforeText = "x" * 1250
    val before = (1 to 50).foldLeft(Rope(""))((rope, _) =>
      rope.insert(rope.weight, "x" * 25).getOrElse(fail("expected insert to succeed"))
    )
    val after     = before.delete(100, 900).getOrElse(fail("expected delete to succeed"))
    val afterText = beforeText.take(100) + beforeText.drop(900)

    val range = RopeDiff.changedOffsetRange(before, after)
    range shouldBe defined
    val Some((start, end)) = range: @unchecked
    val suffixLen          = after.weight - end
    // The rebuilds triggered along the way may widen the reported range beyond the literal [100, 900) delete, but
    // whatever it reports must still leave everything outside it provably unchanged.
    beforeText.take(start) shouldBe afterText.take(start)
    beforeText.takeRight(suffixLen) shouldBe afterText.takeRight(suffixLen)
  }
