package com.serenity.ui.fonts

import java.awt.Font

import com.serenity.text.fallback.FontSlot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Resolving a logical family to one physical face must not make a glyph undrawable: whatever the logical font could
  * show, directly or through its composite component fonts, the role's fallback chain still reaches, because its last
  * slot stays the composite `Dialog`.
  */
class RoleFontCoverageSpec extends AnyFlatSpec with Matchers:

  private val samples = Map(
    "CJK"     -> "日本語のテキスト 漢字 한국어",
    "emoji"   -> "😀🎉👍❤️🚀",
    "symbols" -> "→ ← ⇒ ∑ ∞ ≠ ≤ ┌─┐│└┘ ✓ ✗ ★ © ™"
  )

  private def codePoints(text: String): Vector[Int] = text.codePoints().toArray.toVector.filterNot(_ == ' '.toInt)

  private def chainCovers(font: Font, codePoint: Int): Boolean =
    val chain = Java2DGlyphCoverage.of(font, FontConfigFallback)
    (0 until chain.slotCount).exists(slot => chain.covers(FontSlot(slot), codePoint))

  private val FontConfigFallback = FontLoader.FontConfig().fallback

  List(Font.SANS_SERIF -> "prose", Font.SANS_SERIF -> "UI", Font.SERIF -> "serif prose").foreach { (logical, role) =>
    samples.foreach { (name, text) =>
      s"The $role font ($logical) chain" should s"still reach every $name glyph the logical font could show" in {
        val before = new Font(logical, Font.PLAIN, 12)
        val after  = new Font(LogicalFontFamilies.system.resolve(logical), Font.PLAIN, 12)
        val lost   = codePoints(text).filter(cp => chainCovers(before, cp) && !chainCovers(after, cp))
        withClue(s"$name codepoints covered before but not after (${after.getFamily}): ${lost.map(_.toHexString)}; ")(
          lost shouldBe empty
        )
      }
    }
  }
