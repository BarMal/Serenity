package com.serenity.build

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rule that splits `sbt test` across CI jobs (#2484): every suite runs in exactly one shard. */
class TestShardsSpec extends AnyFlatSpec with Matchers with EitherValues:

  "TestShards.parseShard" should "read index/of" in {
    TestShards.parseShard("2/3").value shouldBe TestShards.Shard(2, 3)
  }

  it should "reject an index outside 1..of, a non-number and a missing slash" in
    List("0/2", "3/2", "a/2", "1/0", "1", "", "1/2/3").foreach { spec =>
      withClue(spec)(TestShards.parseShard(spec).isLeft shouldBe true)
    }

  "TestShards.parseTable" should "read suite, shard and seconds, skipping comments and blank lines" in {
    val table = TestShards.parseTable(List("# header", "", "com.A\t1\t10.5", "com.B\t2\t3")).value

    table shouldBe Map("com.A" -> TestShards.Entry(1, 10.5), "com.B" -> TestShards.Entry(2, 3.0))
  }

  it should "reject a suite listed twice, a short row and a bad shard" in {
    TestShards.parseTable(List("com.A\t1\t1", "com.A\t2\t1")).left.value should include("com.A")
    TestShards.parseTable(List("com.A\t1")).left.value should include("com.A")
    TestShards.parseTable(List("com.A\tx\t1")).left.value should include("com.A")
  }

  "TestShards.shardOf" should "use the table when the suite is listed" in {
    val table = Map("a" -> TestShards.Entry(2, 1.0))

    TestShards.shardOf(table, "a", 2) shouldBe 2
  }

  it should "fall back to a fixed hash of the name for an unlisted suite" in {
    TestShards.shardOf(Map.empty, "a", 2) shouldBe 1
    TestShards.shardOf(Map.empty, "b", 2) shouldBe 2
    TestShards.shardOf(Map.empty, "a", 3) shouldBe 2
  }

  "TestShards.select" should "run a suite only in the shard that owns it" in {
    val table = Map("a" -> TestShards.Entry(1, 1.0), "b" -> TestShards.Entry(2, 1.0))

    TestShards.select(table, TestShards.Shard(1, 2), "a") shouldBe true
    TestShards.select(table, TestShards.Shard(2, 2), "a") shouldBe false
    TestShards.select(table, TestShards.Shard(2, 2), "b") shouldBe true
  }

  "TestShards.check" should "put every defined suite in exactly one shard and report each shard's size and weight" in {
    val table   = Map("a" -> TestShards.Entry(1, 4.0), "b" -> TestShards.Entry(2, 6.0))
    val defined = List("a", "b", "c", "d")

    val summary = TestShards.check(table, defined, 2).value

    summary.suites.values.sum shouldBe 4
    summary.suites(1) + summary.suites(2) shouldBe defined.size
    summary.seconds(1) shouldBe 4.0
    summary.seconds(2) shouldBe 6.0
  }

  it should "fail on a listed suite that no longer exists, so a rename or removal regenerates the table" in {
    val table = Map("gone" -> TestShards.Entry(1, 1.0), "a" -> TestShards.Entry(1, 1.0))

    TestShards.check(table, List("a"), 2).left.value.exists(_.contains("gone")) shouldBe true
  }

  it should "fail on a listed shard beyond the shard count" in {
    val table = Map("a" -> TestShards.Entry(3, 1.0))

    TestShards.check(table, List("a"), 2).left.value.exists(_.contains("a")) shouldBe true
  }

  "TestShards.balance" should "give the shards weights within the heaviest suite of each other" in {
    val seconds = Map("a" -> 100.0, "b" -> 90.0, "c" -> 50.0, "d" -> 45.0, "e" -> 30.0, "f" -> 7.0, "g" -> 2.0)

    val assigned = TestShards.balance(seconds, 2)
    val totals   = (1 to 2).map(s => seconds.filter((name, _) => assigned(name) == s).values.sum)

    assigned.keySet shouldBe seconds.keySet
    math.abs(totals.head - totals.last) should be <= 100.0
    totals.sum shouldBe seconds.values.sum
  }

  it should "be deterministic, breaking ties by name" in {
    val seconds = Map("b" -> 5.0, "a" -> 5.0, "c" -> 5.0, "d" -> 5.0)

    TestShards.balance(seconds, 2) shouldBe TestShards.balance(seconds, 2)
    TestShards.balance(seconds, 2)("a") shouldBe 1
  }

  "TestShards.secondsFromReports" should "read each suite's name and time from JUnit XML" in {
    val xml = """<?xml version="1.0"?><testsuite hostname="h" name="com.serenity.AThing" tests="2" time="12.5">"""

    TestShards.secondsFromReports(List(xml)) shouldBe Map("com.serenity.AThing" -> 12.5)
  }

  "TestShards.render" should "write a table that parseTable reads back" in {
    val entries = Map("com.A" -> TestShards.Entry(1, 10.5), "com.B" -> TestShards.Entry(2, 3.0))

    TestShards.parseTable(TestShards.render(entries).split("\n").toList).value shouldBe entries
  }
