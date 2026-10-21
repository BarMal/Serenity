package com.serenity.ui.fonts

import java.awt.Font
import java.nio.file.Paths

import com.serenity.text.fallback.{FallbackItemiser, FallbackRun, FontSlot}
import com.serenity.ui.fonts.FontLoader.FontConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866 PR B: the emoji face is a slot of the fallback chain, after the configured families and before the system
  * resolver, and only clusters that ask for emoji presentation go to it ahead of the system resolver.
  */
class FontFallbackEmojiSpec extends AnyFlatSpec with Matchers:

  private val emoji  = "Noto Emoji"
  private val sparks = Character.toString(0x2728)
  private val grin   = Character.toString(0x1f600)

  private val emojiSelector = Character.toString(0xfe0f)

  private val codeFont =
    Font
      .createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile)
      .deriveFont(Font.PLAIN, 14.0f)

  private def coverage(chain: FontFallback) = Java2DGlyphCoverage.of(codeFont, chain)

  "A chain with an emoji family" should "order its slots as configured families, the emoji face, then Dialog" in {
    val chain = FontFallback(Vector("Configured"), Some(emoji))
    chain.slotCount shouldBe 4
    chain.slotFamilies shouldBe Vector("Configured", emoji, Font.DIALOG)
    chain.emojiSlot shouldBe Some(FontSlot(2))
  }

  "A chain without one" should "keep the configured families then Dialog, with no emoji slot" in {
    val chain = FontFallback(Vector("Configured"))
    chain.slotCount shouldBe 3
    chain.slotFamilies shouldBe Vector("Configured", Font.DIALOG)
    chain.emojiSlot shouldBe None
  }

  "The default font configuration" should "carry the bundled emoji face" in {
    FontConfig().fallback.emojiFamily shouldBe FontLoader.emojiFontFamily
    FontConfig().fallback.emojiFamily should not be empty
  }

  "Glyph coverage over such a chain" should "expose the emoji slot and cover emoji there" in {
    val chain = FontConfig().fallback
    val slot  = coverage(chain).emojiSlot.getOrElse(fail("no emoji slot"))
    slot shouldBe FontSlot(1)
    coverage(chain).covers(slot, 0x1f600) shouldBe true
    coverage(chain).fontFor(slot).map(_.getFamily) shouldBe FontLoader.emojiFontFamily
  }

  it should "send an emoji cluster the primary lacks to the emoji slot" in {
    val runs = FallbackItemiser.itemise(s"a${grin}b", 0, 4, coverage(FontConfig().fallback))
    runs shouldBe Vector(
      FallbackRun(0, 1, FontSlot.Primary),
      FallbackRun(1, 3, FontSlot(1)),
      FallbackRun(3, 4, FontSlot.Primary)
    )
  }

  it should "send a variation-selector-16 sparkle to the emoji slot" in {
    val text = "x" + sparks + emojiSelector
    FallbackItemiser.itemise(text, 0, text.length, coverage(FontConfig().fallback)).map(_.slot) should contain(
      FontSlot(1)
    )
  }

  it should "leave a text-presentation symbol to the earlier slots and the system resolver" in {
    val snowman = 0x2603
    val chain   = coverage(FontConfig().fallback)
    chain.covers(FontSlot.Primary, snowman) shouldBe false
    chain.covers(FontSlot(1), snowman) shouldBe true
    val text = Character.toString(snowman)
    FallbackItemiser.itemise(text, 0, text.length, chain).map(_.slot) should not contain FontSlot(1)
  }
