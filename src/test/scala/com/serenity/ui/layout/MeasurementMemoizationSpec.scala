package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.TextCaretMeasurement.{ColumnFontRun, LineFontResolver}
import com.serenity.ui.renderer.FontSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MeasurementMemoizationSpec extends AnyFlatSpec with Matchers:

  private val frc = FontRenderContext(AffineTransform(), true, true)

  "MeasurementMemo" should "measure each key once and bound its size" in {
    val memo = MeasurementMemo[Int, Int](maxEntries = 2)(_ * 2)
    (1 to 3).foreach(memo(_))
    memo.measurements shouldBe 3L
    memo(3) shouldBe 6
    memo(3) shouldBe 6
    memo.measurements shouldBe 3L
  }

  "LineFontResolver.lineMetrics" should "probe each font once however many rows ask" in {
    val base   = new Font(Font.MONOSPACED, Font.PLAIN, 37)
    val bigger = new Font(Font.MONOSPACED, Font.BOLD, 41)
    val resolver = LineFontResolver(
      FontSpec.fromAwt(base),
      Vector(ColumnFontRun(0, 5, FontSpec.fromAwt(bigger)), ColumnFontRun(5, 9, FontSpec.fromAwt(base)))
    )
    val extents = FontLineExtents(TestMeasure.measurer.cellMetrics)

    val rows = Vector.fill(50)(resolver.lineMetrics(extents, 0, 9))

    extents.measurements shouldBe 2L
    rows.distinct.size shouldBe 1
  }

  it should "report the ceiling of the font's line height over the ceiling of its ascent, as Skia gives them" in {
    val font                                   = new Font(Font.SERIF, Font.PLAIN, 23)
    val (skiaAscent, skiaDescent, skiaLeading) = TestMeasure.skiaMetrics(font)
    val metrics = LineFontResolver(FontSpec.fromAwt(font), Vector.empty)
      .lineMetrics(FontLineExtents(TestMeasure.measurer.cellMetrics), 0, 4)
    val ascent = math.ceil(-skiaAscent.toDouble).toInt
    val height = math.ceil((skiaDescent - skiaAscent + skiaLeading).toDouble).toInt
    metrics shouldBe (height, ascent)
  }

  it should "measure afresh at another render scale, which is another size of the font" in {
    val font    = new Font(Font.MONOSPACED, Font.PLAIN, 29)
    val scaled  = font.deriveFont(2.0f * font.getSize2D)
    val extents = FontLineExtents(TestMeasure.measurer.cellMetrics)
    LineFontResolver(FontSpec.fromAwt(font), Vector.empty).lineMetrics(extents, 0, 4)
    LineFontResolver(FontSpec.fromAwt(scaled), Vector.empty).lineMetrics(extents, 0, 4)
    LineFontResolver(FontSpec.fromAwt(scaled), Vector.empty).lineMetrics(extents, 0, 4)
    extents.measurements shouldBe 2L
  }

  "DropCapWidths" should "measure a glyph once per font and render context" in {
    val font     = new Font(Font.SERIF, Font.BOLD, 53)
    val measured = DropCapWidths(TestMeasure.measurer.glyphWidthPx)
    val widths   = Vector.fill(20)(measured(FontSpec.fromAwt(font), "W"))
    measured.measurements shouldBe 1L
    widths.distinct shouldBe Vector(font.getStringBounds("W", frc).getWidth.toFloat)
  }

  it should "stay zero for an empty glyph without measuring" in {
    val font     = new Font(Font.SERIF, Font.BOLD, 59)
    val measured = DropCapWidths(TestMeasure.measurer.glyphWidthPx)
    measured(FontSpec.fromAwt(font), "") shouldBe 0.0f
    measured.measurements shouldBe 0L
  }
