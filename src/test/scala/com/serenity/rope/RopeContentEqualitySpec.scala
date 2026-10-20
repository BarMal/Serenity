package com.serenity.rope

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Two ropes are equal when they hold the same text, whatever tree holds it (#1942). */
class RopeContentEqualitySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 5)

  /** Fails if a comparison or hash flattens the rope to a string, which is the cost this equality exists to avoid. */
  final private class NoCollectLeaf(text: String) extends Leaf(text):
    override def collect(): String = fail("equality must compare chunks, not materialise the rope")

  private def leaf(text: String): Rope = Leaf(text)

  /** A rope cut into exactly these leaves, in order. */
  private def cutInto(chunks: String*): Rope = RopeBuilder.fromLeaves(chunks)

  "A single leaf" should "equal a rope of several leaves holding the same text, in either order" in {
    val whole = leaf("ab")
    val split = cutInto("a", "b")

    whole shouldBe split
    split shouldBe whole
  }

  it should "hash the same as a rope of several leaves holding the same text" in {
    leaf("ab").hashCode shouldBe cutInto("a", "b").hashCode
  }

  "Ropes with different leaf boundaries" should "be equal and hash alike" in {
    val early = cutInto("ab", "cdef")
    val late  = cutInto("abcd", "ef")

    early shouldBe late
    early.hashCode shouldBe late.hashCode
  }

  "Ropes with different tree heights" should "be equal and hash alike" in {
    val text    = "abcdefghijklmnopqrstuvwxyz" * 20
    val shallow = cutInto(text.grouped(5).toSeq*)
    val deeper  = cutInto(text.grouped(2).toSeq*)

    deeper.height should be > shallow.height
    shallow shouldBe deeper
    deeper shouldBe shallow
    shallow.hashCode shouldBe deeper.hashCode
  }

  "A rope" should "equal a copy cut at different places" in {
    val text   = "the quick brown fox jumps over the lazy dog " * 20
    val edited = (0 until 30).foldLeft(Rope(text))((rope, i) => rope.insert(i * 7, "x").getOrElse(rope))
    val recut  = cutInto(edited.collect().grouped(7).toSeq*)

    recut should not be theSameInstanceAs(edited)
    recut shouldBe edited
    recut.hashCode shouldBe edited.hashCode
  }

  it should "ignore empty leaves" in {
    cutInto("", "ab") shouldBe leaf("ab")
    cutInto("a", "", "b") shouldBe leaf("ab")
    cutInto("", "") shouldBe leaf("")
    cutInto("", "").hashCode shouldBe leaf("").hashCode
  }

  it should "equal itself" in {
    val rope = Rope("same object " * 50)
    rope shouldBe rope
  }

  it should "hash as the string it holds" in {
    val text = "hash " * 40
    Rope(text).hashCode shouldBe text.hashCode
    cutInto("ab", "cd").hashCode shouldBe "abcd".hashCode
  }

  "Ropes holding different text" should "differ when only the first character differs" in {
    cutInto("ab", "cd") should not be cutInto("xb", "cd")
  }

  it should "differ when only the last character differs" in {
    cutInto("ab", "cd") should not be cutInto("a", "bc", "x")
  }

  it should "differ when one is a prefix of the other" in {
    leaf("abc") should not be cutInto("ab", "cd")
    cutInto("ab", "cd") should not be leaf("abc")
  }

  it should "differ from a string or any non-rope" in {
    leaf("ab").equals("ab") shouldBe false
    leaf("ab").equals(null) shouldBe false
  }

  "Comparing ropes" should "not materialise either of them" in {
    val flat   = new NoCollectLeaf("abcd")
    val shaped = cutInto("ab", "cd")

    flat shouldBe shaped
    flat.hashCode shouldBe shaped.hashCode
    flat should not be cutInto("ab", "cx")
  }

  it should "cope with ropes of a hundred thousand leaves" in {
    val chunks = Vector.fill(100000)("ab")
    val left   = cutInto(chunks*)
    val right  = cutInto(chunks.updated(99999, "ab")*)

    left shouldBe right
    left.hashCode shouldBe right.hashCode
    left should not be cutInto(chunks.updated(99999, "ax")*)
  }

  it should "fall back to chunks when only some of two identically shaped subtrees differ in where they cut" in {
    val first  = cutInto("ab", "cd", "ef", "gh")
    val second = cutInto("ab", "cd", "e", "fgh")

    first shouldBe second
    first.hashCode shouldBe second.hashCode
    first should not be cutInto("ab", "cd", "e", "fgx")
  }

  "Comparing a rope with text" should "match however the rope is cut" in {
    leaf("abcd").contentEquals("abcd") shouldBe true
    cutInto("ab", "", "cd").contentEquals("abcd") shouldBe true
    cutInto("ab", "cd").contentEquals("") shouldBe false
    leaf("").contentEquals("") shouldBe true
  }

  it should "reject text that differs in length or in any character" in {
    cutInto("ab", "cd").contentEquals("abc") shouldBe false
    cutInto("ab", "cd").contentEquals("abcde") shouldBe false
    cutInto("ab", "cd").contentEquals("abxd") shouldBe false
  }

  it should "not materialise the rope" in {
    new NoCollectLeaf("abcd").contentEquals("abcd") shouldBe true
    new NoCollectLeaf("abcd").contentEquals("abcx") shouldBe false
  }
end RopeContentEqualitySpec
