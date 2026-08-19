package com.serenity.rope

import scala.annotation.tailrec

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `rebuild` is what every unbalanced `concat` and `splitAt` falls back to, so its cost is the rope's edit cost. These
  * pin that it reorganises the leaves it already has rather than flattening the rope to a string and re-splitting it.
  */
class RopeRebuildSpec extends AnyFlatSpec with Matchers:

  private given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 5)

  private def leavesOf(rope: Rope): List[String] =
    @tailrec
    def go(stack: List[Rope], acc: List[String]): List[String] = stack match
      case Nil                 => acc.reverse
      case Node(l, r) :: rest  => go(l :: r :: rest, acc)
      case Leaf(value) :: rest => go(rest, value :: acc)
      case other :: rest       => go(rest, other.collect() :: acc)
    go(List(rope), Nil)

  private def leftSpine(values: List[String]): Rope =
    values.map(Leaf(_): Rope).reduceLeft((left, right) => Node(left, right))

  /** The point of rebuilding from leaves is that the characters copied are bounded by the chunk size times the depth,
    * not by the size of the document. Only the leaf straddling each split point is cut; the rest pass through by
    * reference, which reference equality is the only way to observe.
    */
  "rebuild" should "pass most leaf strings through by reference rather than copying every character" in {
    val values  = (1 to 64).toList.map(index => f"$index%04d")
    val rebuilt = leftSpine(values).rebuild

    val reused = leavesOf(rebuilt).count(leaf => values.exists(_ eq leaf))

    rebuilt.collect() shouldBe values.mkString
    reused should be >= values.size - rebuilt.height
  }

  it should "copy nothing at all when the leaves already fall on the split points" in {
    val values  = List("aaaa", "bbbb")
    val rebuilt = leftSpine(values).rebuild

    leavesOf(rebuilt).foreach(leaf =>
      withClue(s"leaf '$leaf' was copied: ") {
        values.exists(_ eq leaf) shouldBe true
      }
    )
  }

  it should "preserve the text" in {
    val rope = leftSpine(List("Hello", ", ", "world", "!"))

    rope.rebuild.collect() shouldBe "Hello, world!"
  }

  it should "yield a balanced tree from a degenerate spine" in {
    val rope = leftSpine((1 to 64).toList.map(index => f"$index%04d"))

    rope.height should be > 32
    rope.rebuild.height should be <= 10
    rope.rebuild.isHeightBalanced shouldBe true
    rope.rebuild.isWeightBalanced shouldBe true
  }

  it should "drop the empty leaves that splitting leaves behind" in {
    val rope = leftSpine(List("aaaa", "", "bbbb", "", ""))

    leavesOf(rope.rebuild) shouldBe List("aaaa", "bbbb")
  }

  it should "re-chunk a leaf that exceeds the chunk size" in {
    val rope = leftSpine(List("abcdefghijkl", "mn"))

    leavesOf(rope.rebuild).foreach(_.length should be <= 5)
    rope.rebuild.collect() shouldBe "abcdefghijklmn"
  }

  it should "keep an already-balanced rope's content and balance" in {
    val rope = Rope("the quick brown fox jumps over the lazy dog")

    rope.rebuild.collect() shouldBe rope.collect()
    rope.rebuild.isWeightBalanced shouldBe true
    rope.rebuild.isHeightBalanced shouldBe true
  }
