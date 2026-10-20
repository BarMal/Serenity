package com.serenity.rope

import scala.util.Random

import com.serenity.rope.btree.LeafLimits
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rope is a B-tree (#1900): these pin its shape rather than its behaviour -- all leaves at one depth, bounded node
  * fill, cached figures that match the text, edits that copy a path and share the rest, small leaves merged after
  * deletes, and leaf boundaries kept clear of surrogate pairs. Behaviour is covered by `RopeEquivalenceSpec`.
  */
class RopeBTreeSpec extends AnyFlatSpec with Matchers:

  private val tiny    = Balance(weightBalance = 8, heightBalance = 1, leafChunkSize = 12)
  private val regular = Balance.default

  private def prose(random: Random, chars: Int): String =
    Iterator
      .unfold(0) { length =>
        Option.when(length < chars) {
          val word = random.alphanumeric.take(1 + random.nextInt(9)).mkString
          val gap  = if random.nextInt(8) == 0 then "\n" else " "
          (word + gap, length + word.length + 1)
        }
      }
      .mkString

  private def assertValid(rope: Rope)(using Balance): Unit =
    RopeInvariants.violations(rope) shouldBe Nil

  private def leafCount(rope: Rope): Int = RopeInvariants.leaves(rope).size

  "A rope built from text" should "hold every leaf at one depth within the node and leaf bounds" in
    List(tiny, regular).foreach { balance =>
      given Balance = balance
      List(0, 1, 11, 12, 13, 100, 5000, 120000).foreach { chars =>
        val text = prose(Random(chars), chars)
        val rope = Rope(text)
        withClue(s"leaf size ${balance.leafChunkSize}, $chars chars: ") {
          assertValid(rope)
          rope.collect() shouldBe text
        }
      }
    }

  it should "be as shallow as its leaf count allows" in {
    given Balance = regular
    val rope      = Rope(prose(Random(7), 10_000_000))
    val leaves    = leafCount(rope)

    leaves should be > 8000
    rope.height should be <= 5
  }

  it should "fill leaves to about half their capacity so typing has room before a split" in {
    given Balance = regular
    val limits    = LeafLimits.of(regular)
    val sizes     = RopeInvariants.leaves(Rope(prose(Random(3), 200000))).map(_.length)

    sizes.foreach(size => size should be <= limits.target + 1)
    sizes.dropRight(1).foreach(size => size should be >= limits.target - limits.target / 4)
  }

  "Edits" should "keep a valid tree through long random sequences" in
    List(tiny, regular).foreach { balance =>
      given Balance = balance
      (1 to 6).foreach { seed =>
        val random = Random(seed)
        val start  = Rope(prose(random, 3000))
        val _ = (1 to 120).foldLeft(start) { (rope, step) =>
          val length = rope.weight
          val next = random.nextInt(6) match
            case 0 | 1 => rope.insert(random.nextInt(length + 1), prose(random, 1 + random.nextInt(8)))
            case 2 =>
              val from = random.nextInt(length + 1)
              rope.delete(from, math.min(length, from + 1 + random.nextInt(30)))
            case 3 =>
              val from = random.nextInt(length + 1)
              rope.delete(from, math.min(length, from + random.nextInt(length / 2 + 1)))
            case 4 => rope.insert(random.nextInt(length + 1), prose(random, 400))
            case _ => Some(rope.concat(Rope(prose(random, random.nextInt(500)))))
          val edited = next.getOrElse(fail("an in-range edit was rejected"))
          withClue(s"leaf size ${balance.leafChunkSize}, seed $seed, step $step: ")(assertValid(edited))
          if edited.weight < 50 then edited.insert(0, prose(random, 800)).getOrElse(edited) else edited
        }
      }
    }

  they should "split and rejoin into a valid tree at every offset" in {
    given Balance = tiny
    val text      = prose(Random(5), 900)
    val rope      = Rope(text)

    (0 to text.length).foreach { at =>
      val (left, right) = rope.splitAt(at).getOrElse(fail("splitAt rejected an in-range offset"))
      withClue(s"split at $at: ") {
        assertValid(left)
        assertValid(right)
        left.collect() shouldBe text.take(at)
        right.collect() shouldBe text.drop(at)
        assertValid(left.concat(right))
        left.concat(right).collect() shouldBe text
      }
    }
  }

  they should "copy one node per level and share the rest with the old tree" in {
    given Balance = regular
    val rope      = Rope(prose(Random(9), 2_000_000))
    val edited    = rope.insert(rope.weight / 3, "x").getOrElse(fail("insert rejected"))

    def sharedLeaves(before: Rope, after: Rope): Int = (before, after) match
      case (a: Branch, b: Branch) if a.children.size == b.children.size =>
        a.children.zip(b.children).map((x, y) => if x eq y then leafCount(x) else sharedLeaves(x, y)).sum
      case _ => 0

    edited.height shouldBe rope.height
    leafCount(rope) - sharedLeaves(rope, edited) shouldBe 1
  }

  they should "merge the small leaves a delete leaves behind" in {
    given Balance = tiny
    val text      = prose(Random(11), 6000)
    val rope      = Rope(text)
    val limits    = LeafLimits.of(tiny)
    val sparse = (text.length / 10 - 1 to 0 by -1).foldLeft(rope) { (current, block) =>
      current.delete(block * 10 + 1, block * 10 + 10).getOrElse(fail("delete rejected"))
    }

    assertValid(sparse)
    sparse.weight should be < text.length
    leafCount(sparse) should be <= sparse.weight / limits.min + 1
  }

  they should "leave no empty leaf behind when a rope is emptied and refilled" in {
    given Balance = tiny
    val full      = Rope(prose(Random(13), 2000))
    val emptied   = full.delete(0, full.weight).getOrElse(fail("delete rejected"))

    emptied.weight shouldBe 0
    assertValid(emptied)
    RopeInvariants.leaves(emptied) shouldBe Vector.empty
    assertValid(emptied.insert(0, prose(Random(14), 500)).getOrElse(fail("insert rejected")))
  }

  they should "keep a pasted block in a valid, shallow tree" in {
    given Balance = regular
    val rope      = Rope(prose(Random(15), 1_000_000))
    val block     = prose(Random(16), 5_000_000)
    val pasted    = rope.insert(rope.weight / 2, block).getOrElse(fail("insert rejected"))

    assertValid(pasted)
    pasted.weight shouldBe rope.weight + block.length
    pasted.height should be <= 5
  }

  "Leaf boundaries" should "never fall between the halves of a surrogate pair" in {
    given Balance = tiny
    val random    = Random(17)
    val emoji     = Vector("😀", "🎉", "👍", "a", "b", " ", "\n")
    val text      = Iterator.continually(emoji(random.nextInt(emoji.size))).take(1500).mkString

    def pairAligned(rope: Rope, at: Int): Boolean =
      at == 0 || at == rope.weight || !(rope.index(at - 1).exists(Character.isHighSurrogate) &&
        rope.index(at).exists(Character.isLowSurrogate))

    def splitPairs(rope: Rope): Int =
      RopeInvariants.leaves(rope).sliding(2).count {
        case Vector(left, right) => Character.isHighSurrogate(left.last) && Character.isLowSurrogate(right.head)
        case _                   => false
      }

    val built = Rope(text)
    splitPairs(built) shouldBe 0
    val _ = (1 to 200).foldLeft(built) { (rope, _) =>
      val at     = Iterator.continually(random.nextInt(rope.weight + 1)).find(pairAligned(rope, _)).getOrElse(0)
      val edited = rope.insert(at, Iterator.continually(emoji(random.nextInt(emoji.size))).take(40).mkString)
      val next   = edited.getOrElse(fail("insert rejected"))
      splitPairs(next) shouldBe 0
      next
    }
  }

  "Line endings" should "be normalised in inserted text but not rescanned in the leaf it lands in" in {
    given Balance = regular
    val untouched = Leaf("a\rb")

    Rope("one\r\ntwo\rthree").collect() shouldBe "one\ntwo\nthree"
    Leaf("ab").insert(1, "x\r\ny\rz").map(_.collect()) shouldBe Some("ax\ny\nzb")
    untouched.insert(3, "c").map(_.collect()) shouldBe Some("a\rbc")
    untouched.insert(1, "\n").map(_.collect()) shouldBe Some("a\n\rb")
  }
