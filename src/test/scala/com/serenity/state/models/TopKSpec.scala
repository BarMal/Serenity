package com.serenity.state.models

import java.util.concurrent.atomic.AtomicInteger

import scala.util.Random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TopKSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  private def counting(comparisons: AtomicInteger): Ordering[Int] =
    new Ordering[Int]:
      def compare(left: Int, right: Int): Int =
        val _ = comparisons.incrementAndGet()
        Integer.compare(left, right)

  "TopK.smallest" should "be the first k of the sorted items, whatever the input" in
    forAll { (items: List[Int], k: Int) =>
      val bound = k % 20
      TopK.smallest(items, bound) shouldBe items.sorted.take(bound)
    }

  it should "keep equal items in their input order" in
    forAll { (items: List[(Int, Int)], k: Int) =>
      val bound                  = (k % 20).abs
      given Ordering[(Int, Int)] = Ordering.by(_._1)
      TopK.smallest(items, bound) shouldBe items.sortBy(_._1).take(bound)
    }

  it should "return everything, sorted, when k exceeds the count" in {
    TopK.smallest(Vector(3, 1, 2), 10) shouldBe Vector(1, 2, 3)
    TopK.smallest(Vector.empty[Int], 5) shouldBe Vector.empty
    TopK.smallest(Vector(3, 1, 2), 0) shouldBe Vector.empty
  }

  it should "compare far fewer times than sorting every item when only a few are wanted" in {
    val items       = new Random(7).shuffle((0 until 4096).toVector)
    val comparisons = new AtomicInteger(0)

    val best = TopK.smallest(items, 50)(using counting(comparisons))

    best shouldBe (0 until 50).toVector
    comparisons.get should be < (3 * items.size)
  }
