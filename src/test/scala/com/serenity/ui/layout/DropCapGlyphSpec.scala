package com.serenity.ui.layout

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DropCapGlyphSpec extends AnyFlatSpec with Matchers:

  private val frc  = TextLayoutSnapshot.defaultFontRenderContext()
  private val font = BundledInterFont.plain(14.0f)

  private def glyphOf(text: String, lines: Int, proseScale: Float = 1.0f): Option[DropCapGlyph] =
    val document = RichTextDocument(List(RichTextParagraph.plain(text, role = ParagraphRole.dropCap(lines))))
    DropCapGlyph.forParagraph(font, frc, Some(document), 0, ParagraphRole.dropCap(lines), proseScale)

  private def extent = FontLineExtents.of(font, frc)

  "DropCapGlyph.forParagraph" should "single out the paragraph's first character" in {
    val glyph = glyphOf("Quiet morning", 2).getOrElse(fail("expected a glyph"))

    glyph.text shouldBe "Q"
    glyph.charCount shouldBe 1
    glyph.widthPx should be > 0.0f
  }

  it should "take a character outside the basic plane whole" in {
    val glyph = glyphOf("𝔸lgebra", 2).getOrElse(fail("expected a glyph"))

    glyph.text shouldBe "𝔸"
    glyph.charCount shouldBe 2
  }

  it should "find no glyph for a paragraph that is not a drop cap, has no text, or has no document" in {
    val body = RichTextDocument(List(RichTextParagraph.plain("Quiet")))

    DropCapGlyph.forParagraph(font, frc, Some(body), 0, ParagraphRole.Body, 1.0f) shouldBe None
    glyphOf("", 2) shouldBe None
    DropCapGlyph.forParagraph(font, frc, None, 0, ParagraphRole.dropCap(2), 1.0f) shouldBe None
  }

  it should "put the baseline on the last spanned line's, whatever the letter" in
    List(2, 3, 4).foreach { lines =>
      glyphOf("Tale", lines).map(_.baselinePx) shouldBe Some(extent.ascentPx + (lines - 1) * extent.heightPx)
    }

  it should "grow the glyph with the lines it spans, a two-line one landing between two and three body fonts" in {
    val sizes = List(1, 2, 3, 4).flatMap(lines => glyphOf("Tale", lines).map(_.fontSizePx))

    sizes.zip(sizes.tail).foreach((smaller, larger) => larger should be > smaller)
    sizes(1) should be > 2.0f * font.getSize2D
    sizes(1) should be < 3.0f * font.getSize2D
  }

  it should "reserve at least the room of the cap's lines" in
    List(2, 3).foreach { lines =>
      glyphOf("Tale", lines).map(_.heightPx).getOrElse(0) should be >= lines * extent.heightPx
    }

  it should "reserve the room a descender hangs into below the last line" in {
    val glyph = glyphOf("Quiet", 2).getOrElse(fail("expected a glyph"))
    val tail  = glyph.font.createGlyphVector(frc, "Q").getVisualBounds.getMaxY

    glyph.heightPx.toDouble should be >= glyph.baselinePx + tail
  }

  it should "keep the indent at the glyph's width plus the gap in em of the body font" in {
    val glyph = glyphOf("Tale", 2).getOrElse(fail("expected a glyph"))

    glyph.indentPx(0.0f) shouldBe glyph.widthPx
    glyph.indentPx(0.5f) shouldBe (glyph.widthPx + 7.0f) +- 0.01f
    glyph.indentPx(2.0f) shouldBe (glyph.widthPx + 28.0f) +- 0.01f
  }

  "DropCapLayout.reserve" should "be the glyph's width plus the gap and its height for a drop cap, and nothing otherwise" in {
    val document = RichTextDocument(List(RichTextParagraph.plain("Tale", role = ParagraphRole.dropCap(2))))
    val glyph    = glyphOf("Tale", 2).getOrElse(fail("expected a glyph"))

    val reserved =
      DropCapLayout.reserve(font, frc, Some(document), 0, ParagraphRole.dropCap(2), 1.0f, gapEm = 1.0f)

    reserved.indentPx shouldBe (glyph.widthPx + 14.0f) +- 0.01f
    reserved.heightPx shouldBe glyph.heightPx
    DropCapLayout.reserve(font, frc, Some(document), 0, ParagraphRole.Body, 1.0f, gapEm = 1.0f) shouldBe
      DropCapLayout.Reserve.none
  }
