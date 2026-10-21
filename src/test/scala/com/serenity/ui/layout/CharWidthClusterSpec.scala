package com.serenity.ui.layout

import com.ibm.icu.text.BreakIterator
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2365: a grapheme cluster, not a codepoint, is what takes cells on a cell grid. An emoji cluster (VS16 on an emoji
  * base, a ZWJ join, an emoji modifier, a tag sequence, a regional-indicator pair, a keycap) takes two cells; any other
  * cluster takes the sum of its codepoints' cells, so combining marks, VS15 and Hangul or Indic sums are unchanged.
  */
class CharWidthClusterSpec extends AnyFlatSpec with Matchers:

  private def s(codePoints: Int*): String = String(codePoints.toArray, 0, codePoints.length)

  private val heart      = s(0x2764)
  private val heartVs16  = s(0x2764, 0xfe0f)
  private val plane      = s(0x2708, 0xfe0f)
  private val bolt       = s(0x26a1)
  private val boltVs15   = s(0x26a1, 0xfe0e)
  private val thumbs     = s(0x1f44d)
  private val thumbsTone = s(0x1f44d, 0x1f3fd)
  private val family     = s(0x1f468, 0x200d, 0x1f469, 0x200d, 0x1f467)
  private val flagUs     = s(0x1f1fa, 0x1f1f8)
  private val keycap     = s('1'.toInt, 0xfe0f, 0x20e3)
  private val rainbow    = s(0x1f3f3, 0xfe0f, 0x200d, 0x1f308)
  private val england    = s(0x1f3f4, 0xe0067, 0xe0062, 0xe0065, 0xe006e, 0xe0067, 0xe007f)
  private val eAcute     = s('e'.toInt, 0x0301)
  private val aVs16      = s('a'.toInt, 0xfe0f)
  private val loneRegion = s(0x1f1fa)
  private val conjunct   = s(0x0915, 0x094d, 0x200d, 0x0937)
  private val emojiSeqs  = Vector(heartVs16, plane, thumbsTone, family, flagUs, keycap, rainbow, england)
  private val everything = Vector(heart, bolt, boltVs15, thumbs, eAcute, aVs16, loneRegion) ++ emojiSeqs

  private def width(text: String): Int = CharWidth.ofCluster(text, 0, text.length)

  "CharWidth.of" should "give a zero-width joiner and a zero-width non-joiner no cells (#2365)" in {
    CharWidth.of(0x200d) shouldBe 0
    CharWidth.of(0x200c) shouldBe 0
  }

  it should "leave a Devanagari conjunct that uses a joiner at the two cells of its two letters" in {
    conjunct.codePoints().map(CharWidth.of).sum() shouldBe 2
  }

  "An emoji cluster" should "take two cells whatever its parts are" in
    emojiSeqs.foreach(sequence =>
      withClue(sequence.codePoints().toArray.toVector.map(_.toHexString))(width(sequence) shouldBe 2)
    )

  it should "take the cells of the codepoint alone when it is one codepoint" in {
    width(heart) shouldBe 1
    width(bolt) shouldBe 2
    width(thumbs) shouldBe 2
  }

  "Any other cluster" should "keep the sum of its codepoints' cells" in {
    width(eAcute) shouldBe 1
    width(aVs16) shouldBe 1
    width(boltVs15) shouldBe 2
    width(loneRegion) shouldBe 1
  }

  "A variation selector 16" should "widen exactly the bases with an emoji presentation (Emoji=Yes)" in {
    val emojiBases = Vector(0x2764, 0x2708, 0x263a, 0x00a9, 0x2714, 0x25b6, 0x260e, 0x2601, 0x00ae, 0x2122)
    emojiBases.foreach(base => withClue(base.toHexString)(width(s(base, 0xfe0f)) shouldBe 2))
    val otherBases = Vector('a'.toInt, 'z'.toInt, '-'.toInt, 0x00e9, 0x4e2d)
    otherBases.foreach(base => withClue(base.toHexString)(width(s(base, 0xfe0f)) shouldBe CharWidth.of(base)))
  }

  "clusterEnd" should "cut at the end of each cluster" in {
    val text = "a" + heartVs16 + "b" + family + eAcute
    CharWidth.clusterEnd(text, 0) shouldBe 1
    CharWidth.clusterEnd(text, 1) shouldBe 3
    CharWidth.clusterEnd(text, 3) shouldBe 4
    CharWidth.clusterEnd(text, 4) shouldBe 4 + family.length
    CharWidth.clusterEnd(text, 4 + family.length) shouldBe text.length
  }

  it should "pair regional indicators from the left" in {
    val three = flagUs + loneRegion
    CharWidth.clusterEnd(three, 0) shouldBe 4
    CharWidth.clusterEnd(three, 4) shouldBe 6
  }

  it should "agree with ICU's grapheme boundaries for emoji sequences and marks, alone and joined together" in {
    val breaks = BreakIterator.getCharacterInstance
    def icu(text: String): Vector[Int] =
      breaks.setText(text)
      Iterator.iterate(breaks.first())(_ => breaks.next()).takeWhile(_ != BreakIterator.DONE).toVector
    def ours(text: String): Vector[Int] =
      Iterator.iterate(0)(CharWidth.clusterEnd(text, _)).takeWhile(_ < text.length).toVector :+ text.length
    val corpus = everything ++ (for left <- everything; right <- everything yield left + right) ++
      everything.map("x" + _ + "y")
    corpus.foreach(text =>
      withClue(text.codePoints().toArray.toVector.map(_.toHexString))(ours(text) shouldBe icu(text))
    )
  }

  "hasClusterChars" should "be false for text that cannot hold an emoji cluster" in {
    CharWidth.hasClusterChars("plain ascii, tabs\tand all") shouldBe false
    CharWidth.hasClusterChars("你好，世界 こんにちは") shouldBe false
    CharWidth.hasClusterChars(eAcute) shouldBe false
  }

  it should "be true for text with a joiner, a variation selector, a keycap or an astral codepoint" in
    (emojiSeqs :+ thumbs :+ "a‍b").foreach(text => CharWidth.hasClusterChars(text) shouldBe true)
