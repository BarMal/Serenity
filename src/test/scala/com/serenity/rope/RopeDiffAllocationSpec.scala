package com.serenity.rope

import java.lang.management.ManagementFactory

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Finding what an edit changed must cost what the edit touched, wherever in the rope it falls. An insertion at the end
  * of a subtree is the awkward place: the subtrees on either side of it no longer line up, so the two ropes are
  * compared by cutting one at the boundary, and cutting must not rebuild what lies beyond it.
  */
class RopeDiffAllocationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val allocationBean = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
      if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
      Some(bean)
    case _ => None

  private def endOfLeftSubtreeEdit(size: Int): (Rope, Rope) =
    val before = Rope("the cat sat on the mat\n" * (size / 23))
    val boundary = before match
      case Node(left, _) => left.weight
      case other         => other.weight
    (before, before.insert(boundary, "x").getOrElse(before))

  private def allocatedBy(size: Int): Option[Long] =
    allocationBean.map { bean =>
      val (before, after) = endOfLeftSubtreeEdit(size)
      val threadId        = Thread.currentThread().threadId()
      (1 to 200).foreach(_ => RopeDiff.changedOffsetRange(before, after))
      (1 to 5).map { _ =>
        val start = bean.getThreadAllocatedBytes(threadId)
        RopeDiff.changedOffsetRange(before, after)
        bean.getThreadAllocatedBytes(threadId) - start
      }.min
    }

  "Finding an insertion at the end of a subtree" should "cost the same in a 1 MB rope as in a 10 KB one" in {
    val (before, after) = endOfLeftSubtreeEdit(1024 * 1024)
    withClue("the edit must land on the end of the left subtree: ") {
      RopeDiff.changedOffsetRange(before, after).map(_._1) shouldBe Some(before match
        case Node(left, _) => left.weight
        case other         => other.weight)
    }
    (allocatedBy(10 * 1024), allocatedBy(1024 * 1024)) match
      case (Some(small), Some(large)) =>
        info(s"10 KB rope allocated ${small}B, 1 MB rope allocated ${large}B")
        withClue(s"10 KB rope allocated ${small}B, 1 MB rope allocated ${large}B: ")(large should be < small * 2L)
      case _ =>
        info("JVM per-thread allocation counter unsupported on this runtime -- skipping the allocation assertion")
  }
end RopeDiffAllocationSpec
