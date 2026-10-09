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

  private def endOfLeftSubtreeEdit(size: Int): (Rope, Rope) =
    val before = Rope("the cat sat on the mat\n" * (size / 23))
    val boundary = before match
      case Node(left, _) => left.weight
      case other         => other.weight
    (before, before.insert(boundary, "x").getOrElse(before))

  "Finding an insertion at the end of a subtree" should "cost the same in a 1 MB rope as in a 10 KB one" in {
    val (before, after) = endOfLeftSubtreeEdit(1024 * 1024)
    withClue("the edit must land on the end of the left subtree: ") {
      RopeDiff.changedOffsetRange(before, after).map(_._1) shouldBe Some(before match
        case Node(left, _) => left.weight
        case other         => other.weight)
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
end RopeDiffAllocationSpec
