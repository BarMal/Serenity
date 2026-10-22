package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.TextAttribute

import com.serenity.ui.fonts.FontLoader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2372: the font a flag, a skin tone or a tag sequence is drawn in has ligatures on, which is what makes the JDK join
  * it into one glyph; the font a CJK character or a symbol falls back to keeps the setting the user chose for the
  * primary. The skin tone is covered here and not by pixels, since the two-cell clip hides what it draws wrongly.
  */
class StyledFontCacheEmojiClusterSpec extends AnyFlatSpec with Matchers:

  private val emoji = FontLoader.emojiFontFamily.getOrElse(fail("the bundled emoji family is not registered"))

  private def ligaturesOf(font: Font): Option[Integer] =
    Option(font.getAttributes.get(TextAttribute.LIGATURES)).collect { case setting: Integer => setting }

  private def withLigatures(font: Font, setting: Integer): Font =
    font.deriveFont(java.util.Map.of[TextAttribute, Integer](TextAttribute.LIGATURES, setting))

  private val plainPrimary = new Font(Font.MONOSPACED, Font.PLAIN, 14)
  private val onPrimary    = withLigatures(plainPrimary, TextAttribute.LIGATURES_ON)

  "The fallback font asked for with ligatures on" should "have ligatures on when the primary's are off" in {
    ligaturesOf(StyledFontCache().fallbackFontFor(plainPrimary, emoji, ligaturesOn = true)) shouldBe Some(
      TextAttribute.LIGATURES_ON
    )
  }

  it should "have ligatures on when the primary's are on" in {
    ligaturesOf(StyledFontCache().fallbackFontFor(onPrimary, emoji, ligaturesOn = true)) shouldBe Some(
      TextAttribute.LIGATURES_ON
    )
  }

  it should "keep the primary's family, size and style" in {
    val bold = plainPrimary.deriveFont(Font.BOLD, 20.0f)
    val font = StyledFontCache().fallbackFontFor(bold, emoji, ligaturesOn = true)
    font.getFamily shouldBe emoji
    font.getSize2D shouldBe 20.0f
    font.getStyle shouldBe Font.BOLD
  }

  "The fallback font asked for without" should "keep the primary's ligature setting" in {
    val cache = StyledFontCache()
    ligaturesOf(cache.fallbackFontFor(plainPrimary, emoji, ligaturesOn = false)) shouldBe None
    ligaturesOf(cache.fallbackFontFor(onPrimary, emoji, ligaturesOn = false)) shouldBe Some(TextAttribute.LIGATURES_ON)
  }

  "The cache" should "keep one font with ligatures on and another without for the same family" in {
    val cache   = StyledFontCache()
    val cluster = cache.fallbackFontFor(plainPrimary, emoji, ligaturesOn = true)
    val letter  = cache.fallbackFontFor(plainPrimary, emoji, ligaturesOn = false)

    cluster should not be letter
    cache.fallbackFontFor(plainPrimary, emoji, ligaturesOn = true) should be theSameInstanceAs cluster
    cache.fallbackFontFor(plainPrimary, emoji, ligaturesOn = false) should be theSameInstanceAs letter
  }
