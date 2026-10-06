package com.serenity.text

import scala.annotation.tailrec

import com.serenity.rope.{Balance, Rope, RopeCharacterSource}
import com.serenity.text.TextEditing.{CharacterSource, StringCharacterSource}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Word motion over UAX#29 word boundaries (#1949). Lives in `com.serenity.text` for the whitebox cache specs, which
  * mirror `CharacterSourceIdentityAnchorSpec`'s for the grapheme iterator.
  */
class TextEditingWordSpec extends AnyFlatSpec with Matchers:

  private val spacelessChinese = "我们今天去公园"

  private val rainbowFlag = "🏳️‍🌈"

  private val family = "👨‍👩‍👧"

  private def forwardStops(source: CharacterSource): Vector[Int] =
    @tailrec
    def loop(offset: Int, acc: Vector[Int]): Vector[Int] =
      if offset >= source.length then acc
      else
        val next = TextEditing.nextWordBoundary(source, offset)
        loop(next, acc :+ next)

    loop(0, Vector.empty)

  private def backwardStops(source: CharacterSource): Vector[Int] =
    @tailrec
    def loop(offset: Int, acc: Vector[Int]): Vector[Int] =
      if offset <= 0 then acc
      else
        val previous = TextEditing.previousWordBoundary(source, offset)
        loop(previous, acc :+ previous)

    loop(source.length, Vector.empty)

  "Ctrl+Right over spaceless Chinese" should "stop at each dictionary word rather than jumping to the end of the run" in {
    // ICU4J 78.3's dictionary segments this as 我们 | 今天 | 去 | 公园.
    TextEditing.nextWordBoundary(spacelessChinese, 0) shouldBe 2
    forwardStops(StringCharacterSource(spacelessChinese)) shouldBe Vector(2, 4, 5, 7)
  }

  "Ctrl+Left over spaceless Chinese" should "stop at the same dictionary words walking backwards" in {
    backwardStops(StringCharacterSource(spacelessChinese)) shouldBe Vector(5, 4, 2, 0)
  }

  "Word motion over a multi-leaf rope" should "find the same dictionary boundaries as over the flat string" in {
    given Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 3)
    val rope      = Rope(s"go $spacelessChinese now")

    rope.weight should be > 3
    forwardStops(RopeCharacterSource(rope)) shouldBe Vector(3, 5, 7, 8, 11, 14)
  }

  "Latin word motion" should "keep its word, punctuation-run and whitespace stops" in {
    val text = "The quick, brown fox... jumps!"

    forwardStops(StringCharacterSource(text)) shouldBe Vector(4, 9, 11, 17, 20, 24, 29, 30)
    backwardStops(StringCharacterSource(text)) shouldBe Vector(29, 24, 20, 17, 11, 9, 4, 0)
  }

  "An emoji ZWJ sequence" should "be a single word-motion stop" in {
    val flagText = s"a $rainbowFlag b"
    val flagEnd  = 2 + rainbowFlag.length

    TextEditing.nextWordBoundary(flagText, 2) shouldBe flagEnd + 1
    TextEditing.previousWordBoundary(flagText, flagEnd) shouldBe 2

    val familyText = s"a${family}b"
    TextEditing.nextWordBoundary(familyText, 1) shouldBe 1 + family.length
    TextEditing.previousWordBoundary(familyText, 1 + family.length) shouldBe 1
  }

  it should "be one stop when it is the only thing in the text" in {
    forwardStops(StringCharacterSource(rainbowFlag)) shouldBe Vector(rainbowFlag.length)
    backwardStops(StringCharacterSource(rainbowFlag)) shouldBe Vector(0)
  }

  "The word BreakIterator cache" should "skip setText for a fresh wrapper sharing the previous call's identity anchor" in {
    val text = "hello world"

    val first              = TextEditing.wordBreakIterator(StringCharacterSource(text))
    val textAfterFirstCall = first.getText()
    val second             = TextEditing.wordBreakIterator(StringCharacterSource(text))

    second should be theSameInstanceAs first
    second.getText() should be theSameInstanceAs textAfterFirstCall
  }

  it should "re-set the text when the identity anchor changes" in {
    val textAfterFirstCall  = TextEditing.wordBreakIterator(StringCharacterSource("hello world")).getText()
    val textAfterSecondCall = TextEditing.wordBreakIterator(StringCharacterSource("goodbye world")).getText()

    textAfterSecondCall should not be theSameInstanceAs(textAfterFirstCall)
  }

  it should "be separate from the grapheme iterator, so interleaved grapheme queries do not evict it" in {
    val text = "hello world"

    val textAfterFirstCall = TextEditing.wordBreakIterator(StringCharacterSource(text)).getText()
    val grapheme           = TextEditing.graphemeBreakIterator(StringCharacterSource("unrelated"))
    val word               = TextEditing.wordBreakIterator(StringCharacterSource(text))

    word should not be theSameInstanceAs(grapheme)
    word.getText() should be theSameInstanceAs textAfterFirstCall
  }
