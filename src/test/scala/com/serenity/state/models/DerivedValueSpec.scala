package com.serenity.state.models

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1852: a derived value is recomputed only when one of its declared inputs is a different reference. */
class DerivedValueSpec extends AnyFlatSpec with Matchers:

  final private case class Source(words: List[String], separator: String, unrelated: Int)

  private def countingJoin(computations: AtomicInteger): DerivedValue[Source, Source, String] =
    DerivedValue(
      inputs = identity,
      references = source => List(source.words, source.separator),
      compute = source =>
        val _ = computations.incrementAndGet()
        source.words.mkString(source.separator)
    )

  "DerivedValue.refreshed" should "compute once and reuse the memo while every input is the same reference" in {
    val computations = AtomicInteger(0)
    val joined       = countingJoin(computations)
    val source       = Source(List("a", "b"), "-", 0)

    val first  = joined.refreshed(None, source)
    val second = joined.refreshed(Some(first), source.copy(unrelated = 1))

    second should be theSameInstanceAs first
    second.value shouldBe "a-b"
    computations.get shouldBe 1
  }

  it should "recompute when one input is a different reference, even if it is equal by value" in {
    val computations = AtomicInteger(0)
    val joined       = countingJoin(computations)
    val source       = Source(List("a", "b"), "-", 0)

    val first  = joined.refreshed(None, source)
    val second = joined.refreshed(Some(first), source.copy(words = List("a", "b")))
    val third  = joined.refreshed(Some(second), source.copy(words = List("c")))

    second should not be theSameInstanceAs(first)
    third.value shouldBe "c"
    computations.get shouldBe 3
  }

  "DerivedValue.valueFor" should "serve a current memo without computing, and never serve a stale one" in {
    val computations = AtomicInteger(0)
    val joined       = countingJoin(computations)
    val source       = Source(List("a", "b"), "-", 0)
    val memo         = joined.refreshed(None, source)

    joined.valueFor(Some(memo), source.copy(unrelated = 7)) should be theSameInstanceAs memo.value
    computations.get shouldBe 1

    joined.valueFor(Some(memo), source.copy(separator = "+")) shouldBe "a+b"
    joined.valueFor(None, source) shouldBe "a-b"
    computations.get shouldBe 3
  }
