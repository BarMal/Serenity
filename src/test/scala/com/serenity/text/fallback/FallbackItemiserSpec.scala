package com.serenity.text.fallback

import com.serenity.text.AllocationProbe
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FallbackItemiserSpec extends AnyFlatSpec with Matchers:

  final private case class FakeGlyphCoverage(coveredBySlot: Vector[Int => Boolean], emojiSlot: Option[FontSlot])
      extends GlyphCoverage:
    override def slotCount: Int = coveredBySlot.length

    override def covers(slot: FontSlot, codePoint: Int): Boolean =
      slot.index >= 0 && slot.index < coveredBySlot.length && coveredBySlot(slot.index)(codePoint)

  private def isAsciiOrLatin1(codePoint: Int): Boolean = codePoint < 0x0250
  private def isCjk(codePoint: Int): Boolean           = codePoint >= 0x4e00 && codePoint <= 0x9fff
  private def isArabic(codePoint: Int): Boolean        = codePoint >= 0x0600 && codePoint <= 0x06ff
  private def isPictographic(codePoint: Int): Boolean  = codePoint >= 0x1f000 && codePoint <= 0x1faff

  private val Latin  = FontSlot(0)
  private val Cjk    = FontSlot(1)
  private val Arabic = FontSlot(2)
  private val Emoji  = FontSlot(3)
  private val System = FontSlot(4)

  private val HeavyBlackHeart = 0x2764

  /** Primary: Latin plus the dingbat heart as a text glyph. CJK faces carry ASCII too; this Arabic face does not carry
    * ASCII punctuation. The system resolver covers everything but the supplementary private-use planes.
    */
  private val coverage = FakeGlyphCoverage(
    Vector(
      cp => isAsciiOrLatin1(cp) || cp == HeavyBlackHeart,
      cp => isCjk(cp) || cp < 0x80,
      cp => isArabic(cp) || cp == ' ',
      cp => isPictographic(cp) || cp == HeavyBlackHeart,
      cp => cp < 0xf0000
    ),
    Some(Emoji)
  )

  private def itemiseAll(text: String, glyphs: GlyphCoverage = coverage): Vector[FallbackRun] =
    FallbackItemiser.itemise(text, 0, text.length, glyphs)

  private val family = "👨‍👩‍👧"

  "itemise" should "send CJK in a Latin-only primary to the first fallback that covers it" in {
    itemiseAll("abc 中文") shouldBe Vector(FallbackRun(0, 4, Latin), FallbackRun(4, 6, Cjk))
  }

  it should "keep spaces, punctuation and digits on the previous run's slot when that slot covers them" in {
    itemiseAll("中文, 123 ب") shouldBe Vector(FallbackRun(0, 8, Cjk), FallbackRun(8, 9, Arabic))
  }

  it should "move a neutral off the previous slot when that slot does not cover it" in {
    itemiseAll("ب!") shouldBe Vector(FallbackRun(0, 1, Arabic), FallbackRun(1, 2, Latin))
  }

  it should "give a neutral at the start of the range the first slot that covers it" in {
    itemiseAll(" 中") shouldBe Vector(FallbackRun(0, 1, Latin), FallbackRun(1, 2, Cjk))
  }

  it should "put a ZWJ emoji sequence in the emoji slot as one run" in {
    itemiseAll(s"a${family}b") shouldBe
      Vector(FallbackRun(0, 1, Latin), FallbackRun(1, 9, Emoji), FallbackRun(9, 10, Latin))
  }

  it should "never split a grapheme cluster, even when no single fallback but the last covers all of it" in {
    val emojiWithoutGirl = coverage.copy(
      coveredBySlot = coverage.coveredBySlot.updated(Emoji.index, cp => isPictographic(cp) && cp != 0x1f467)
    )
    itemiseAll(s"a${family}b", emojiWithoutGirl) shouldBe
      Vector(FallbackRun(0, 1, Latin), FallbackRun(1, 9, System), FallbackRun(9, 10, Latin))
  }

  it should "send a cluster with VS16 to the emoji slot although the primary has a text glyph for its base" in {
    itemiseAll("a❤️") shouldBe Vector(FallbackRun(0, 1, Latin), FallbackRun(1, 3, Emoji))
  }

  it should "leave a text-default emoji without a selector in the primary that covers it" in {
    itemiseAll("a❤") shouldBe Vector(FallbackRun(0, 2, Latin))
  }

  it should "send an Emoji_Presentation codepoint to the emoji slot" in {
    itemiseAll("a😀") shouldBe Vector(FallbackRun(0, 1, Latin), FallbackRun(1, 3, Emoji))
  }

  it should "prefer a text face over the emoji slot for a cluster with VS15" in {
    itemiseAll("a😀︎") shouldBe Vector(FallbackRun(0, 1, Latin), FallbackRun(1, 4, System))
  }

  it should "leave a cluster no slot covers in the primary, so its missing glyph does not split the run" in {
    itemiseAll("a󰀀b") shouldBe Vector(FallbackRun(0, 4, Latin))
  }

  it should "itemise only the requested range" in {
    FallbackItemiser.itemise("xx中文yy", 2, 4, coverage) shouldBe Vector(FallbackRun(2, 4, Cjk))
  }

  it should "give no runs for an empty range" in {
    FallbackItemiser.itemise("abc", 2, 2, coverage) shouldBe Vector.empty
  }

  it should "give exactly one primary run for a line the primary covers" in {
    itemiseAll("plain Latin text, with punctuation: 42!") shouldBe Vector(FallbackRun(0, 39, Latin))
  }

  it should "allocate nothing per character for a line the primary covers" in {
    val line = "The quick brown fox jumps over the lazy dog. " * 250
    AllocationProbe.allocatedBytes(itemiseAll(line)).foreach(_ should be < 1024L)
  }
