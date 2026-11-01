package com.serenity.perf

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A shard is one contiguous slice of the suite, so each shard keeps the suite's order and a CI job can run only its
  * own rows.
  */
class BenchmarkShardSpec extends AnyFlatSpec with Matchers:

  private val rows = (1 to 10).toList.map(i => s"row$i")

  "A shard" should "be read from index/count" in {
    BenchmarkShard.parse("2/3") shouldBe Right(BenchmarkShard(2, 3))
    BenchmarkShard.parse("1/1") shouldBe Right(BenchmarkShard(1, 1))
  }

  it should "reject an index outside 1..count and anything that is not index/count" in {
    List("0/2", "3/2", "2/0", "x", "1/", "/2", "1/2/3", "-1/2").foreach(text =>
      withClue(text)(BenchmarkShard.parse(text).isLeft shouldBe true)
    )
  }

  it should "take a contiguous slice and, together, the shards take every row once in order" in {
    val shards = List(BenchmarkShard(1, 3), BenchmarkShard(2, 3), BenchmarkShard(3, 3))
    shards.flatMap(_.select(rows)) shouldBe rows
    shards.map(_.select(rows).size).max - shards.map(_.select(rows).size).min should be <= 1
  }

  it should "take everything when there is only one shard" in {
    BenchmarkShard(1, 1).select(rows) shouldBe rows
  }

  it should "take nothing from an empty suite" in {
    BenchmarkShard(2, 2).select(Nil) shouldBe Nil
  }

  "The arguments" should "separate a --shard option from the benchmark names" in {
    BenchmarkShard.fromArgs(List("--shard=2/2", "layout.", "render.")) shouldBe Right((Some(BenchmarkShard(2, 2)), List("layout.", "render.")))
    BenchmarkShard.fromArgs(List("layout.")) shouldBe Right((None, List("layout.")))
    BenchmarkShard.fromArgs(Nil) shouldBe Right((None, Nil))
  }

  they should "fail on a malformed --shard" in {
    BenchmarkShard.fromArgs(List("--shard=3/2")).isLeft shouldBe true
  }
