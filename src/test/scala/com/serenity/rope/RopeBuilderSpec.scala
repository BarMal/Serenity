package com.serenity.rope

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RopeBuilderSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def leavesOf(count: Int): Vector[String] =
    Vector.tabulate(count)(index => s"line $index\n".padTo(Balance.default.leafChunkSize, 'x'))

  "RopeBuilder.fromLeaves" should "join the leaves in order" in
    List(0, 1, 2, 3, 5, 64, 1000).foreach { count =>
      val leaves = leavesOf(count)
      RopeBuilder.fromLeaves(leaves).collect() shouldBe leaves.mkString
    }

  it should "skip empty leaves" in {
    RopeBuilder.fromLeaves(List("a", "", "b")).collect() shouldBe "ab"
    RopeBuilder.fromLeaves(List("", "")).collect() shouldBe ""
  }

  it should "build a rope that needs no rebalancing" in {
    val rope = RopeBuilder.fromLeaves(leavesOf(777))
    rope.isDepthBalanced shouldBe true
    rope.isHeightBalanced shouldBe true
    rope.rebalance shouldBe theSameInstanceAs(rope)
  }

  it should "keep the rope editable" in {
    val rope   = RopeBuilder.fromLeaves(leavesOf(50))
    val edited = rope.insert(1234, "INSERTED").getOrElse(fail("insert failed"))
    edited.collect() shouldBe leavesOf(50).mkString.patch(1234, "INSERTED", 0)
  }

  "RopeChunks.of" should "yield the leaves of a rope left to right" in {
    val leaves = leavesOf(9)
    RopeChunks.of(RopeBuilder.fromLeaves(leaves)).toVector shouldBe leaves
  }

  it should "yield nothing for an empty rope" in {
    RopeChunks.of(Rope("")).toVector shouldBe Vector.empty
  }
