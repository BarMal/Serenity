package com.serenity.rope

import com.serenity.perf.SettledAllocation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Finding what an edit changed must cost what the edit touched, wherever in the rope it falls. An insertion at the end
  * of a subtree is the awkward place: the subtrees on either side of it no longer line up, so the two ropes are
  * compared by cutting one at the boundary, and cutting must not rebuild what lies beyond it.
  */
class RopeDiffAllocationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def firstChildWeight(rope: Rope): Int = rope match
    case branch: Branch => branch.children(0).weight
    case other          => other.weight

  private def endOfLeftSubtreeEdit(size: Int): (Rope, Rope) =
    val before   = Rope("the cat sat on the mat\n" * (size / 23))
    val boundary = firstChildWeight(before)
    (before, before.insert(boundary, "x").getOrElse(before))

  "Finding an insertion at the end of a subtree" should "cost the same in a 1 MB rope as in a 10 KB one" in {
    val (before, after) = endOfLeftSubtreeEdit(1024 * 1024)
    withClue("the edit must land on the end of the left subtree: ") {
      RopeDiff.changedOffsetRange(before, after).map(_._1) shouldBe Some(firstChildWeight(before))
    }
    val (smallBefore, smallAfter) = endOfLeftSubtreeEdit(10 * 1024)
    SettledAllocation.perCall(
      () => RopeDiff.changedOffsetRange(smallBefore, smallAfter),
      () => RopeDiff.changedOffsetRange(before, after)
    ) match
      case Some((small, large)) =>
        info(s"10 KB rope allocated ${small}B, 1 MB rope allocated ${large}B")
        withClue(s"10 KB rope allocated ${small}B, 1 MB rope allocated ${large}B: ")(large should be < small * 2L)
      case None =>
        info("JVM per-thread allocation counter unsupported on this runtime -- skipping the allocation assertion")
  }
  private def chunked(text: String, size: Int): Rope =
    Rope.fromLeafValues(text.grouped(size).toVector)

  private def misalignedShapes(size: Int): (Rope, Rope) =
    val text   = "the cat sat on the mat\n" * (size / 23)
    val middle = text.length / 2
    (chunked(text, 100), chunked(text.take(middle) + "x" + text.drop(middle), 64))

  "Finding an edit between ropes of different shapes" should "allocate no more than its answer, however large they are" in {
    val (before, after) = misalignedShapes(1024 * 1024)
    val middle          = before.weight / 2
    RopeDiff.changedOffsetRange(before, after) shouldBe Some((middle, middle + 1))
    val (smallBefore, smallAfter) = misalignedShapes(10 * 1024)
    SettledAllocation.perCall(
      () => RopeDiff.changedOffsetRange(smallBefore, smallAfter),
      () => RopeDiff.changedOffsetRange(before, after)
    ) match
      case Some((small, large)) =>
        withClue(s"10 KB ropes allocated ${small}B, 1 MB ropes allocated ${large}B: ")(large should be <= 256L)
      case None =>
        info("JVM per-thread allocation counter unsupported on this runtime -- skipping the allocation assertion")
  }
end RopeDiffAllocationSpec
