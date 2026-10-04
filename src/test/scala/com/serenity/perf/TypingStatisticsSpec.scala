package com.serenity.perf

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypingStatisticsSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val oneToHundred = (1 to 100).map(_.toDouble).toVector

  "summarize" should "report count, mean, p50 and p95 by nearest rank" in {
    val summary = TypingStatistics.summarize(oneToHundred).value
    summary.count shouldBe 100
    summary.meanMs shouldBe 50.5 +- 1e-9
    summary.p50Ms shouldBe 50.0
    summary.p95Ms shouldBe 95.0
  }

  it should "not depend on the order of the samples" in {
    TypingStatistics.summarize(oneToHundred.reverse) shouldBe TypingStatistics.summarize(oneToHundred)
  }

  it should "take the lone value as every statistic for a single sample" in {
    TypingStatistics.summarize(Vector(3.5)).value shouldBe TypingStatistics.Summary(1, 3.5, 3.5, 3.5)
  }

  it should "report nothing for no samples" in {
    TypingStatistics.summarize(Vector.empty) shouldBe None
  }

  "bucketed" should "summarise each 1-based inclusive keystroke range" in {
    val buckets =
      TypingStatistics.bucketed(oneToHundred, Vector(TypingStatistics.Bucket(1, 50), TypingStatistics.Bucket(51, 100)))
    buckets.map(_.bucket) shouldBe Vector(TypingStatistics.Bucket(1, 50), TypingStatistics.Bucket(51, 100))
    buckets.map(_.summary.value.meanMs) shouldBe Vector(25.5, 75.5)
    buckets.map(_.summary.value.p95Ms) shouldBe Vector(48.0, 98.0)
    buckets.map(_.summary.value.count) shouldBe Vector(50, 50)
  }

  it should "summarise only the samples that exist when a bucket is larger than what remains" in {
    val buckets = TypingStatistics.bucketed(Vector(1.0, 2.0, 3.0), Vector(TypingStatistics.Bucket(1, 50)))
    buckets.flatMap(_.summary).map(s => (s.count, s.meanMs, s.p95Ms)) shouldBe Vector((3, 2.0, 3.0))
  }

  it should "leave a bucket past the last sample empty" in {
    val buckets = TypingStatistics.bucketed(Vector(1.0, 2.0, 3.0), Vector(TypingStatistics.Bucket(51, 100)))
    buckets.map(_.summary) shouldBe Vector(None)
  }

  it should "report every bucket empty when there are no samples" in {
    TypingStatistics.bucketed(Vector.empty, TypingStatistics.Bucket.cold).map(_.summary).distinct shouldBe Vector(None)
  }

  it should "use the 1-50, 51-100, 101-200, 201-400 and 401-600 buckets for cold typing" in {
    TypingStatistics.Bucket.cold.map(b => (b.first, b.last)) shouldBe
      Vector((1, 50), (51, 100), (101, 200), (201, 400), (401, 600))
  }

  "Bucket.label" should "read first-last" in {
    TypingStatistics.Bucket(101, 200).label shouldBe "101-200"
  }
