package com.serenity.rope

import com.serenity.perf.SettledAllocation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Comparing two independently built ropes of equal content walks every leaf, so it must cost the string comparisons
  * and nothing else: the `equals.*.independent_equal_content` benchmarks slowed 2.6x when the walk built a list of
  * child pairs for every branch.
  */
class RopeEqualityAllocationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val text = "the cat sat on the mat and looked at the dog\n" * 25_000

  "Comparing independently built ropes of equal content" should "allocate nothing that grows with the rope" in {
    val (left, right)           = (Rope(text), Rope(text))
    val (smallLeft, smallRight) = (Rope(text.take(20_000)), Rope(text.take(20_000)))
    withClue("the ropes must differ as objects: ") {
      (left eq right) shouldBe false
      left.height should be > smallLeft.height
    }
    SettledAllocation.perCall(() => smallLeft == smallRight, () => left == right) match
      case Some((small, large)) =>
        withClue(s"20 KB ropes allocated ${small}B, 1 MB ropes allocated ${large}B: ")(large should be <= 256L)
      case None =>
        info("JVM per-thread allocation counter unsupported on this runtime -- skipping the allocation assertion")
  }

  "Hashing a rope" should "allocate nothing that grows with the rope" in {
    val (small, large) = (Rope(text.take(20_000)), Rope(text))
    SettledAllocation.perCall(() => small.hashCode, () => large.hashCode) match
      case Some((smallBytes, largeBytes)) =>
        withClue(s"20 KB rope allocated ${smallBytes}B, 1 MB rope allocated ${largeBytes}B: ")(
          largeBytes should be <= 256L
        )
      case None =>
        info("JVM per-thread allocation counter unsupported on this runtime -- skipping the allocation assertion")
  }
end RopeEqualityAllocationSpec
