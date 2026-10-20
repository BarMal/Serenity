package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import com.serenity.ui.layout.TextCaretMeasurement.{ColumnFontRun, LineFontResolver}
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
    val base     = new Font(Font.MONOSPACED, Font.PLAIN, 37)
    val bigger   = new Font(Font.MONOSPACED, Font.BOLD, 41)
    val resolver = LineFontResolver(base, Vector(ColumnFontRun(0, 5, bigger), ColumnFontRun(5, 9, base)))
    val before   = FontLineExtents.shared.measurements

    val rows = Vector.fill(50)(resolver.lineMetrics(frc, 0, 9))

    (FontLineExtents.shared.measurements - before) shouldBe 2L
    rows.distinct.size shouldBe 1
  }

  it should "report the ceiling of the font's ascent over the ceiling of its descent and leading" in {
    val font    = new Font(Font.SERIF, Font.PLAIN, 23)
    val line    = font.getLineMetrics("Mg", frc)
    val metrics = LineFontResolver(font, Vector.empty).lineMetrics(frc, 0, 4)
    val ascent  = math.ceil(line.getAscent.toDouble).toInt
    val descent = math.ceil((line.getDescent + line.getLeading).toDouble).toInt
    metrics shouldBe (ascent + descent, ascent)
  }

  it should "measure afresh at another render scale" in {
    val font   = new Font(Font.MONOSPACED, Font.PLAIN, 29)
    val scaled = FontRenderContext(AffineTransform.getScaleInstance(2.0, 2.0), true, true)
    val before = FontLineExtents.shared.measurements
    LineFontResolver(font, Vector.empty).lineMetrics(frc, 0, 4)
    LineFontResolver(font, Vector.empty).lineMetrics(scaled, 0, 4)
    (FontLineExtents.shared.measurements - before) shouldBe 2L
  }

  "DropCapLayout.glyphWidthPx" should "measure a glyph once per font and render context" in {
    val font   = new Font(Font.SERIF, Font.BOLD, 53)
    val before = DropCapLayout.glyphWidthMeasurements
    val widths = Vector.fill(20)(DropCapLayout.glyphWidthPx(font, frc, "W"))
    (DropCapLayout.glyphWidthMeasurements - before) shouldBe 1L
    widths.distinct shouldBe Vector(font.getStringBounds("W", frc).getWidth.toFloat)
  }

  it should "stay zero for an empty glyph without measuring" in {
    val font   = new Font(Font.SERIF, Font.BOLD, 59)
    val before = DropCapLayout.glyphWidthMeasurements
    DropCapLayout.glyphWidthPx(font, frc, "") shouldBe 0.0f
    DropCapLayout.glyphWidthMeasurements shouldBe before
  }
