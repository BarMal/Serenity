package com.serenity.ui.layout

import java.awt.Font

import com.serenity.richtext.ParagraphRole
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DropCapLayoutSpec extends AnyFlatSpec with Matchers:

  "DropCapLayout.spansLine" should "hold for every line within the drop cap's span" in {
    val role = ParagraphRole.DropCap(3)
    DropCapLayout.spansLine(role, 0) shouldBe true
    DropCapLayout.spansLine(role, 1) shouldBe true
    DropCapLayout.spansLine(role, 2) shouldBe true
    DropCapLayout.spansLine(role, 3) shouldBe false
  }

  it should "never hold for a non-drop-cap role" in {
    DropCapLayout.spansLine(ParagraphRole.Body, 0) shouldBe false
    DropCapLayout.spansLine(ParagraphRole.Heading(1), 0) shouldBe false
  }

  it should "never hold for a negative line index" in {
    DropCapLayout.spansLine(ParagraphRole.DropCap(3), -1) shouldBe false
  }

  "DropCapLayout.leftInsetPx" should "reserve the one shared indent on every spanned line" in {
    val role = ParagraphRole.DropCap(2)
    DropCapLayout.leftInsetPx(role, 0, indentPx = 40.0f) shouldBe 40.0f
    DropCapLayout.leftInsetPx(role, 1, indentPx = 40.0f) shouldBe 40.0f
  }

  it should "reserve no inset past the drop cap's span" in {
    DropCapLayout.leftInsetPx(ParagraphRole.DropCap(2), 2, indentPx = 40.0f) shouldBe 0.0f
  }

  it should "reserve no inset for a non-drop-cap role" in {
    DropCapLayout.leftInsetPx(ParagraphRole.Body, 0, indentPx = 40.0f) shouldBe 0.0f
  }

  "DropCapWidths" should "measure a font's advance width for the glyph text" in {
    val font  = Font(Font.SERIF, Font.BOLD, 36)
    val frc   = TextLayoutSnapshot.defaultFontRenderContext()
    val width = DropCapWidths()(font, frc, "C")

    width should be > 0.0f
  }

  it should "measure zero width for empty glyph text" in {
    val font = Font(Font.SERIF, Font.BOLD, 36)
    val frc  = TextLayoutSnapshot.defaultFontRenderContext()

    DropCapWidths()(font, frc, "") shouldBe 0.0f
  }

  "DropCapLayout.glyphHeightPx" should "be the room of the drop cap's line count at the paragraph's normal line height" in {
    DropCapLayout.glyphHeightPx(ParagraphRole.DropCap(3), normalLineHeightPx = 16) shouldBe 48
  }

  it should "be zero for a non-drop-cap role" in {
    DropCapLayout.glyphHeightPx(ParagraphRole.Body, normalLineHeightPx = 16) shouldBe 0
  }

  "DropCapLayout.homeLineRole" should "return the drop cap role only at the paragraph's home line" in {
    DropCapLayout.homeLineRole(ParagraphRole.DropCap(3), startColumn = 0) shouldBe Some(ParagraphRole.DropCap(3))
  }

  it should "return None for a wrapped continuation line of the same paragraph" in {
    DropCapLayout.homeLineRole(ParagraphRole.DropCap(3), startColumn = 12) shouldBe None
  }

  it should "return None for a non-drop-cap role even at column 0" in {
    DropCapLayout.homeLineRole(ParagraphRole.Body, startColumn = 0) shouldBe None
    DropCapLayout.homeLineRole(ParagraphRole.Heading(1), startColumn = 0) shouldBe None
  }

  "DropCapLayout.glyphRowSpan" should "span the cap's lines from the row the glyph is painted from" in {
    val snapshot = DropCapSnapshots.snapshot(DropCapSnapshots.dropCap(3, rows = 5), DropCapSnapshots.body(2))

    DropCapLayout.glyphRowSpan(snapshot) shouldBe Map(0 -> (0 until 3))
  }

  it should "end at the paragraph's last row when it wraps to fewer rows than the cap" in {
    val snapshot = DropCapSnapshots.snapshot(DropCapSnapshots.dropCap(4, rows = 2), DropCapSnapshots.body(3))

    DropCapLayout.glyphRowSpan(snapshot) shouldBe Map(0 -> (0 until 2))
  }

  it should "key each glyph by its own home row" in {
    val snapshot = DropCapSnapshots.snapshot(
      DropCapSnapshots.body(1),
      DropCapSnapshots.dropCap(2, rows = 4),
      DropCapSnapshots.dropCap(3, rows = 3)
    )

    DropCapLayout.glyphRowSpan(snapshot) shouldBe Map(1 -> (1 until 3), 5 -> (5 until 8))
  }

  it should "find no glyph once the paragraph's first row has scrolled out of view" in {
    val snapshot =
      DropCapSnapshots.snapshot(DropCapSnapshots.Paragraph(ParagraphRole.dropCap(3), rows = 3, firstRow = 1))

    DropCapLayout.glyphRowSpan(snapshot) shouldBe empty
  }

  it should "reach as many rows as it takes to cover the glyph's height when rows differ in height" in {
    val tall = DropCapSnapshots.Paragraph(ParagraphRole.dropCap(3), rows = 4, heightsPx = Map(1 -> 40))

    DropCapLayout.glyphRowSpan(DropCapSnapshots.snapshot(tall)) shouldBe Map(0 -> (0 until 2))
  }

  it should "reach the row a glyph's descender hangs into, which the cap's own lines do not cover" in {
    val descending = DropCapSnapshots.Paragraph(ParagraphRole.dropCap(2), rows = 4, glyphHeightPx = Some(40))

    DropCapLayout.glyphRowSpan(DropCapSnapshots.snapshot(descending)) shouldBe Map(0 -> (0 until 3))
  }

  it should "find nothing in a cell layout, which never spans a glyph across rows" in {
    val snapshot = DropCapSnapshots.snapshot(DropCapSnapshots.dropCap(3, rows = 3)).copy(usesMeasuredLayout = false)

    DropCapLayout.glyphRowSpan(snapshot) shouldBe empty
  }

  "DropCapLayout.closedOverGlyphs" should "add every row of a span that has one dirty row" in {
    val snapshot = DropCapSnapshots.snapshot(DropCapSnapshots.dropCap(3, rows = 5), DropCapSnapshots.body(2))

    DropCapLayout.closedOverGlyphs(Set(2), snapshot) shouldBe Set(0, 1, 2)
  }

  it should "leave rows outside every span as they are" in {
    val snapshot = DropCapSnapshots.snapshot(DropCapSnapshots.dropCap(3, rows = 5), DropCapSnapshots.body(2))

    DropCapLayout.closedOverGlyphs(Set(3, 6), snapshot) shouldBe Set(3, 6)
    DropCapLayout.closedOverGlyphs(Set.empty, snapshot) shouldBe Set.empty[Int]
  }

  "DropCapLayout.reservingGlyphHeight" should "pad the last row of a paragraph shorter than its glyph" in {
    val snapshot = DropCapSnapshots.snapshot(DropCapSnapshots.dropCap(4, rows = 2))
    val padded   = DropCapLayout.reservingGlyphHeight(snapshot.visualLines, ParagraphRole.dropCap(4))

    padded.map(_.reservedBelowPx) shouldBe Vector(0, 2 * DropCapSnapshots.RowHeightPx)
  }

  it should "pad a paragraph shorter than its glyph by as much as a descender adds to the glyph" in {
    val descending = DropCapSnapshots.Paragraph(ParagraphRole.dropCap(2), rows = 2, glyphHeightPx = Some(44))
    val snapshot   = DropCapSnapshots.snapshot(descending)

    DropCapLayout.reservingGlyphHeight(snapshot.visualLines, ParagraphRole.dropCap(2)).map(_.reservedBelowPx) shouldBe
      Vector(0, 12)
  }

  it should "leave a paragraph at least as tall as its glyph as it is" in {
    val snapshot = DropCapSnapshots.snapshot(DropCapSnapshots.dropCap(3, rows = 3))

    DropCapLayout.reservingGlyphHeight(snapshot.visualLines, ParagraphRole.dropCap(3)) shouldBe snapshot.visualLines
  }

  it should "leave rows with no measured height as they are" in {
    val unmeasured = DropCapSnapshots.snapshot(DropCapSnapshots.dropCap(3, rows = 1)).visualLines.map { line =>
      line.copy(row = line.row.copy(heightPx = 0))
    }

    DropCapLayout.reservingGlyphHeight(unmeasured, ParagraphRole.dropCap(3)) shouldBe unmeasured
  }

  it should "leave every other role as it is" in {
    val snapshot = DropCapSnapshots.snapshot(DropCapSnapshots.body(1))

    DropCapLayout.reservingGlyphHeight(snapshot.visualLines, ParagraphRole.Body) shouldBe snapshot.visualLines
  }
