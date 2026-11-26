package com.serenity.ui.skia

import java.awt.Font

import com.serenity.testkit.TestMeasure
import com.serenity.text.fallback.{FallbackItemiser, FallbackRun, FontSlot}
import com.serenity.ui.fonts.FontFallback
import com.serenity.ui.renderer.FontSpec
import org.jetbrains.skia.Typeface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Which slots of a fallback chain have a glyph for a codepoint, read from the Skia typefaces the shaper draws with, so
  * the runs layout measures are the runs painting shapes (what [[com.serenity.text.fallback.GlyphCoverage]] is for).
  */
class SkiaGlyphCoverageSpec extends AnyFlatSpec with Matchers:

  private def bundled(family: String): Typeface =
    SkiaTypefaces.bundledTypefaces
      .find(_.getFamilyName.equalsIgnoreCase(family))
      .getOrElse(fail(s"no bundled $family"))

  private val code      = bundled("Monaspace Neon")
  private val icons     = bundled("Material Icons Round")
  private val emoji     = bundled("Noto Emoji")
  private val closeIcon = 0xe5cd

  private def plain(typeface: Typeface) = SkiaGlyphCoverage.Slot(typeface, composite = false)

  "SkiaGlyphCoverage" should "report that the bundled code font has no CJK or Arabic glyphs (#1866)" in {
    val coverage = SkiaGlyphCoverage.of(Vector(plain(code)), None, FallbackFonts.unavailable)
    coverage.covers(FontSlot.Primary, 'a') shouldBe true
    coverage.covers(FontSlot.Primary, '中') shouldBe false
    coverage.covers(FontSlot.Primary, 'ب') shouldBe false
  }

  it should "cover the controls and format characters that draw nothing in every slot, so a tab does not cut a line" in {
    val coverage = SkiaGlyphCoverage.of(Vector(plain(code), plain(icons)), None, FallbackFonts.unavailable)
    List('\t'.toInt, 0xad, 0x200e, 0x85).foreach { codePoint =>
      withClue(f"U+$codePoint%04X: ")(
        (0 until 2).forall(slot => coverage.covers(FontSlot(slot), codePoint)) shouldBe true
      )
    }
    FallbackItemiser.itemise("a\tb", 0, 3, coverage) shouldBe Vector(FallbackRun(0, 3, FontSlot.Primary))
  }

  it should "send a codepoint only a fallback covers to that fallback's slot" in {
    val coverage = SkiaGlyphCoverage.of(Vector(plain(code), plain(icons)), None, FallbackFonts.unavailable)
    val text     = s"a${Character.toString(closeIcon)}b"
    FallbackItemiser.itemise(text, 0, text.length, coverage) shouldBe
      Vector(FallbackRun(0, 1, FontSlot.Primary), FallbackRun(1, 2, FontSlot(1)), FallbackRun(2, 3, FontSlot.Primary))
  }

  it should "answer outside the chain with no coverage" in {
    val coverage = SkiaGlyphCoverage.of(Vector(plain(code)), None, FallbackFonts.unavailable)
    coverage.slotCount shouldBe 1
    coverage.covers(FontSlot(5), 'a') shouldBe false
    coverage.covers(FontSlot(-1), 'a') shouldBe false
  }

  it should "keep the emoji slot it is given" in {
    SkiaGlyphCoverage
      .of(Vector(plain(code), plain(emoji), plain(icons)), Some(FontSlot(1)), FallbackFonts.unavailable)
      .emojiSlot shouldBe
      Some(FontSlot(1))
    SkiaGlyphCoverage.of(Vector(plain(code), plain(icons)), None, FallbackFonts.unavailable).emojiSlot shouldBe None
  }

  it should "reach whatever the font manager matches for a codepoint a composite slot's own font lacks, and in no other slot" in {
    val manager = FallbackFonts.matching((_, codePoint) => Option.when(codePoint == '中'.toInt)(icons))
    val coverage =
      SkiaGlyphCoverage.of(Vector(plain(code), SkiaGlyphCoverage.Slot(icons, composite = true)), None, manager)
    coverage.covers(FontSlot(1), '中') shouldBe true
    coverage.covers(FontSlot(1), 'ب') shouldBe false
    coverage.covers(FontSlot.Primary, '中') shouldBe false
  }

  it should "answer a codepoint it was asked before the same, whatever the page memo holds" in {
    val coverage = SkiaGlyphCoverage.of(Vector(plain(code), plain(icons)), None, FallbackFonts.unavailable)
    val asked    = (0x20 to 0x2000).map(cp => coverage.covers(FontSlot.Primary, cp))
    (0x20 to 0x2000).map(cp => coverage.covers(FontSlot.Primary, cp)) shouldBe asked
    asked.count(identity) should be > 100
  }

  "The measurer's glyph coverage" should "chain the primary, the configured families, the emoji face and Dialog" in {
    val primary  = FontSpec.fromAwt(Font(Font.MONOSPACED, Font.PLAIN, 14))
    val coverage = TestMeasure.measurer.glyphCoverage(primary, FontFallback(Vector.empty, Some("Noto Emoji")))
    coverage.slotCount shouldBe 3
    coverage.emojiSlot shouldBe Some(FontSlot(1))
    coverage.covers(FontSlot(1), 0x1f600) shouldBe true
  }

  it should "let a logical-family primary reach the system's fonts, as its AWT composite did" in {
    val logical =
      TestMeasure.measurer.glyphCoverage(FontSpec.fromAwt(Font(Font.SANS_SERIF, Font.PLAIN, 14)), FontFallback.Default)
    val bundledCode = Font.createFont(
      Font.TRUETYPE_FONT,
      java.nio.file.Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile
    )
    val physical =
      TestMeasure.measurer.glyphCoverage(FontSpec.fromAwt(bundledCode.deriveFont(14.0f)), FontFallback.Default)
    List(0x41, 0xe9).foreach(codePoint => logical.covers(FontSlot.Primary, codePoint) shouldBe true)
    logical.covers(FontSlot.Primary, 0x1f600) shouldBe logical.covers(FontSlot(1), 0x1f600)
    physical.covers(FontSlot.Primary, 0x4e2d) shouldBe false
  }
