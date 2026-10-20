package com.serenity.ui.fonts

import java.awt.Font
import java.awt.font.TextAttribute
import java.nio.file.Paths

import com.serenity.text.fallback.{FallbackItemiser, FallbackRun, FontSlot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Java2DGlyphCoverageSpec extends AnyFlatSpec with Matchers:

  private def bundled(resource: String): Font =
    Font
      .createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource(resource).toURI).toFile)
      .deriveFont(Font.BOLD, 14.0f)

  private val codeFont  = bundled("/fonts/MonaspaceNeon-Regular.otf")
  private val iconFont  = bundled("/fonts/MaterialIconsRound-Regular.otf").deriveFont(Font.PLAIN, 9.0f)
  private val closeIcon = 0xe5cd

  "Java2DGlyphCoverage" should "report that the bundled code font has no CJK or Arabic glyphs (#1866)" in {
    val coverage = Java2DGlyphCoverage(codeFont, Vector.empty, None)
    coverage.covers(FontSlot.Primary, 'a') shouldBe true
    coverage.covers(FontSlot.Primary, '中') shouldBe false
    coverage.covers(FontSlot.Primary, 'ب') shouldBe false
  }

  it should "end the chain in the Dialog logical composite at the primary's style and size" in {
    val coverage = Java2DGlyphCoverage(codeFont, Vector(iconFont), None)
    coverage.slotCount shouldBe 3
    val system = coverage.fontFor(FontSlot(2))
    system.map(_.getName) shouldBe Some(Font.DIALOG)
    system.map(_.getStyle) shouldBe Some(Font.BOLD)
    system.map(_.getSize2D) shouldBe Some(14.0f)
  }

  it should "derive each fallback to the primary's style and size" in {
    val coverage = Java2DGlyphCoverage(codeFont, Vector(iconFont), None)
    coverage.fontFor(FontSlot(1)).map(_.getStyle) shouldBe Some(Font.BOLD)
    coverage.fontFor(FontSlot(1)).map(_.getSize2D) shouldBe Some(14.0f)
  }

  it should "carry the primary's ligature setting onto the fallbacks" in {
    val ligatures = java.util.Map.of[TextAttribute, AnyRef](TextAttribute.LIGATURES, TextAttribute.LIGATURES_ON)
    val coverage  = Java2DGlyphCoverage(codeFont.deriveFont(ligatures), Vector(iconFont), None)
    coverage.fontFor(FontSlot(1)).map(_.getAttributes.get(TextAttribute.LIGATURES)) shouldBe
      Some(TextAttribute.LIGATURES_ON)
  }

  it should "place the emoji face after the configured fallbacks and before the system resolver" in {
    val coverage = Java2DGlyphCoverage(codeFont, Vector(iconFont), Some(iconFont))
    coverage.slotCount shouldBe 4
    coverage.emojiSlot shouldBe Some(FontSlot(2))
    Java2DGlyphCoverage(codeFont, Vector.empty, None).emojiSlot shouldBe None
  }

  it should "send a codepoint only a fallback covers to that fallback's slot" in {
    val coverage = Java2DGlyphCoverage(codeFont, Vector(iconFont), None)
    val text     = s"a${Character.toString(closeIcon)}b"
    FallbackItemiser.itemise(text, 0, text.length, coverage) shouldBe
      Vector(FallbackRun(0, 1, FontSlot.Primary), FallbackRun(1, 2, FontSlot(1)), FallbackRun(2, 3, FontSlot.Primary))
  }

  it should "answer outside the chain with no coverage" in {
    val coverage = Java2DGlyphCoverage(codeFont, Vector.empty, None)
    coverage.covers(FontSlot(5), 'a') shouldBe false
    coverage.fontFor(FontSlot(5)) shouldBe None
  }
