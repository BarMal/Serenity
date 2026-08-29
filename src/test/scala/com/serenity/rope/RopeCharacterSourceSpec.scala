package com.serenity.rope

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers the `Rope` <-> `TextEditing.CharacterSource` adapter and the rope-native word/grapheme-boundary extensions
  * built on it, including across multi-leaf (`Node`) ropes where a naive adapter could get the leaf boundary wrong.
  */
class RopeCharacterSourceSpec extends AnyFlatSpec with Matchers:

  // Small leaves force multi-node ropes for short strings, so boundary scans cross leaf/`Node` splits.
  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 3)

  "RopeCharacterSource" should "expose the rope's weight as its length" in {
    RopeCharacterSource(Rope("hello world")).length shouldBe 11
    RopeCharacterSource(Rope.empty).length shouldBe 0
  }

  it should "read characters by index across leaf boundaries" in {
    val rope   = Rope("hello world") // leafChunkSize = 3 forces multiple leaves/nodes
    val source = RopeCharacterSource(rope)
    "hello world".indices.foreach(i => source.charAt(i) shouldBe "hello world".charAt(i))
  }

  it should "fall back to the NUL sentinel for an out-of-range index rather than throwing" in {
    RopeCharacterSource(Rope("ab")).charAt(5) shouldBe '\u0000'
  }

  "Rope word/grapheme extensions" should "match the String-based TextEditing results on a multi-leaf rope" in {
    val text = "hello, world"
    val rope = Rope(text)

    rope.nextWordBoundary(0) shouldBe com.serenity.text.TextEditing.nextWordBoundary(text, 0)
    rope.nextWordBoundary(5) shouldBe com.serenity.text.TextEditing.nextWordBoundary(text, 5)
    rope
      .previousWordBoundary(text.length) shouldBe com.serenity.text.TextEditing.previousWordBoundary(text, text.length)
  }

  it should "step over a surrogate-pair grapheme that straddles a leaf boundary" in {
    val text = "a🙂b" // a, emoji surrogate pair, b -- 4 UTF-16 code units
    val rope = Rope(text)

    rope.nextGraphemeBoundary(1) shouldBe 3
    rope.previousGraphemeBoundary(3) shouldBe 1
  }

  it should "treat the start of the rope as a grapheme boundary" in {
    val rope = Rope("hello")
    rope.previousGraphemeBoundary(0) shouldBe 0
    rope.graphemeBoundaryBeforeOrAt(0) shouldBe 0
  }

  it should "treat the end of the rope as a grapheme boundary" in {
    val rope = Rope("hello")
    rope.nextGraphemeBoundary(rope.weight) shouldBe rope.weight
    rope.graphemeBoundaryAfterOrAt(rope.weight) shouldBe rope.weight
  }

  it should "recognise a whole-grapheme match range and reject a split one" in {
    val text = "a🙂b"
    val rope = Rope(text)

    rope.isWholeGraphemeRange(1, 3) shouldBe true  // the whole emoji
    rope.isWholeGraphemeRange(1, 2) shouldBe false // half a surrogate pair
  }

  it should "operate correctly on an empty rope" in {
    val rope = Rope.empty
    rope.nextGraphemeBoundary(0) shouldBe 0
    rope.previousGraphemeBoundary(0) shouldBe 0
    rope.isWholeGraphemeRange(0, 0) shouldBe true
  }
