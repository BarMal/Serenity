package com.serenity.rope

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Two ropes are equal when they hold the same text, whatever tree holds it (#1942). */
class RopeContentEqualitySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 5)

  /** Fails if a comparison or hash flattens the rope to a string, which is the cost this equality exists to avoid. */
  final private class NoCollectLeaf(text: String) extends Leaf(text):
    override def collect(): String = fail("equality must compare chunks, not materialise the rope")

  private def node(left: Rope, right: Rope): Rope = Node(left, right)
  private def leaf(text: String): Rope            = Leaf(text)

  private def skewed(chunks: Seq[String]): Rope =
    chunks.foldLeft(leaf(""))((rope, chunk) => node(rope, leaf(chunk)))

  "A single leaf" should "equal a node holding the same text, in either order" in {
    val whole = leaf("ab")
    val split = node(leaf("a"), leaf("b"))

    whole shouldBe split
    split shouldBe whole
  }

  it should "hash the same as a node holding the same text" in {
    leaf("ab").hashCode shouldBe node(leaf("a"), leaf("b")).hashCode
  }

  "Ropes with different leaf boundaries" should "be equal and hash alike" in {
    val early = node(leaf("ab"), leaf("cdef"))
    val late  = node(leaf("abcd"), leaf("ef"))

    early shouldBe late
    early.hashCode shouldBe late.hashCode
  }

  "Ropes with different tree depths" should "be equal and hash alike" in {
    val leftHeavy  = node(node(leaf("a"), leaf("b")), leaf("c"))
    val rightHeavy = node(leaf("a"), node(leaf("b"), leaf("c")))

    leftHeavy shouldBe rightHeavy
    leftHeavy.hashCode shouldBe rightHeavy.hashCode
  }

  "A rope" should "equal its own rebuild" in {
    val text    = "the quick brown fox jumps over the lazy dog " * 20
    val edited  = (0 until 30).foldLeft(Rope(text))((rope, i) => rope.insert(i * 7, "x").getOrElse(rope))
    val rebuilt = edited.rebuild

    rebuilt should not be theSameInstanceAs(edited)
    rebuilt shouldBe edited
    rebuilt.hashCode shouldBe edited.hashCode
  }

  it should "ignore empty leaves" in {
    node(leaf(""), leaf("ab")) shouldBe leaf("ab")
    node(leaf("a"), node(leaf(""), leaf("b"))) shouldBe leaf("ab")
    node(leaf(""), leaf("")) shouldBe leaf("")
    node(leaf(""), leaf("")).hashCode shouldBe leaf("").hashCode
  }

  it should "equal itself" in {
    val rope = Rope("same object " * 50)
    rope shouldBe rope
  }

  it should "hash as the string it holds" in {
    val text = "hash " * 40
    Rope(text).hashCode shouldBe text.hashCode
    node(leaf("ab"), leaf("cd")).hashCode shouldBe "abcd".hashCode
  }

  "Ropes holding different text" should "differ when only the first character differs" in {
    node(leaf("ab"), leaf("cd")) should not be node(leaf("xb"), leaf("cd"))
  }

  it should "differ when only the last character differs" in {
    node(leaf("ab"), leaf("cd")) should not be node(leaf("a"), node(leaf("bc"), leaf("x")))
  }

  it should "differ when one is a prefix of the other" in {
    leaf("abc") should not be node(leaf("ab"), leaf("cd"))
    node(leaf("ab"), leaf("cd")) should not be leaf("abc")
  }

  it should "differ from a string or any non-rope" in {
    leaf("ab").equals("ab") shouldBe false
    leaf("ab").equals(null) shouldBe false
  }

  "Comparing ropes" should "not materialise either of them" in {
    val flat   = new NoCollectLeaf("abcd")
    val shaped = node(leaf("ab"), leaf("cd"))

    flat shouldBe shaped
    flat.hashCode shouldBe shaped.hashCode
    flat should not be node(leaf("ab"), leaf("cx"))
  }

  it should "cope with ropes far deeper than the stack" in {
    val chunks = Vector.fill(100000)("ab")
    val left   = skewed(chunks)
    val right  = skewed(chunks.updated(99999, "ab"))

    left shouldBe right
    left.hashCode shouldBe right.hashCode
    left should not be skewed(chunks.updated(99999, "ax"))
  }

  it should "fall back to chunks when only some of two identically shaped subtrees differ in where they cut" in {
    val first  = node(node(leaf("ab"), leaf("cd")), node(leaf("ef"), leaf("gh")))
    val second = node(node(leaf("ab"), leaf("cd")), node(leaf("e"), leaf("fgh")))

    first shouldBe second
    first.hashCode shouldBe second.hashCode
    first should not be node(node(leaf("ab"), leaf("cd")), node(leaf("e"), leaf("fgx")))
  }

  "Comparing a rope with text" should "match however the rope is cut" in {
    leaf("abcd").contentEquals("abcd") shouldBe true
    node(leaf("ab"), node(leaf(""), leaf("cd"))).contentEquals("abcd") shouldBe true
    node(leaf("ab"), leaf("cd")).contentEquals("") shouldBe false
    leaf("").contentEquals("") shouldBe true
  }

  it should "reject text that differs in length or in any character" in {
    node(leaf("ab"), leaf("cd")).contentEquals("abc") shouldBe false
    node(leaf("ab"), leaf("cd")).contentEquals("abcde") shouldBe false
    node(leaf("ab"), leaf("cd")).contentEquals("abxd") shouldBe false
  }

  it should "not materialise the rope" in {
    new NoCollectLeaf("abcd").contentEquals("abcd") shouldBe true
    new NoCollectLeaf("abcd").contentEquals("abcx") shouldBe false
  }
end RopeContentEqualitySpec
