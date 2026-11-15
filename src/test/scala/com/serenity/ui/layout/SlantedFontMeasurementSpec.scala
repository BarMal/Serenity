package com.serenity.ui.layout

import java.awt.Font
import java.awt.geom.AffineTransform

import com.serenity.state.models.TextVisualLine
import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.TextCaretMeasurement.singleFontResolver
import com.serenity.ui.renderer.FontSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A slanted face whose `TextLayout` advance includes the slant overhang (a synthesised italic, as macOS draws for a
  * family with no italic face) must be measured like the per-row `TextLayout` oracle ([[LegacyLineMeasurement]]): the
  * overhang sits in the trailing edge of every row, so summed glyph advances would wrap rows differently.
  */
class SlantedFontMeasurementSpec extends AnyFlatSpec with Matchers:

  private val frc       = TextLayoutSnapshot.defaultFontRenderContext()
  private val tolerance = 0.02f

  private val upright = Font(Font.SANS_SERIF, Font.PLAIN, 13)
  private val slanted = upright.deriveFont(AffineTransform.getShearInstance(-0.2, 0.0))

  private val prose =
    "The quick brown fox jumps over the lazy dog, again and again, until the row finally has to wrap somewhere."

  private def legacyRows(text: String, widthPx: Int, font: Font): Vector[TextVisualLine] =
    LegacyLineMeasurement.wrap(
      text,
      3,
      widthPx,
      singleFontResolver(FontSpec.fromAwt(font)),
      frc,
      measuredLayout = true,
      TestMeasure.cellMetrics(font)
    )

  "A slanted font" should "have no context-free characters, since its layout advance differs from summed advances" in {
    val advances = GlyphAdvances.measure(
      prose,
      0,
      prose.length,
      0,
      singleFontResolver(FontSpec.fromAwt(slanted)),
      TestMeasure.measurer
    )
    advances.isContextFree(0, 1) shouldBe false
    val plain = GlyphAdvances.measure(
      prose,
      0,
      prose.length,
      0,
      singleFontResolver(FontSpec.fromAwt(upright)),
      TestMeasure.measurer
    )
    plain.isContextFree(0, prose.length) shouldBe true
  }

  it should "not be treated as cutting cleanly at any space" in {
    ShapingBarriers.cutAtOrAfter("a T", 2, FontSpec.fromAwt(upright), TestMeasure.measurer) shouldBe 2
    ShapingBarriers.cutAtOrAfter("a T", 2, FontSpec.fromAwt(slanted), TestMeasure.measurer) shouldBe 3
  }

  it should "wrap exactly where per-row measurement did, with the same widths and carets" in
    Seq(40, 97, 160, 233, 480).foreach { widthPx =>
      val rows =
        TextLayoutSnapshot.boundedVisualLinesForText(prose, 3, widthPx, slanted, wrapCache = TestMeasure.wrapCache())
      val legacy = legacyRows(prose, widthPx, slanted)
      withClue(s"at $widthPx px: ") {
        rows.map(_.text) shouldBe legacy.map(_.text)
        rows.zip(legacy).foreach {
          case (row, oracle) =>
            withClue(s"row '${row.text}': ") {
              row.widthPx shouldBe oracle.widthPx +- tolerance
              row.caretStops.map(_.xPx).zip(oracle.caretStops.map(_.xPx)).foreach {
                case (x, oracleX) =>
                  x shouldBe oracleX +- tolerance
              }
            }
        }
      }
    }

  it should "measure caret positions as per-row measurement did" in {
    val xs = TextLayoutSnapshot.caretXsForText(prose, slanted, TestMeasure.layoutMemos())
    val legacy =
      LegacyLineMeasurement.caretXs(
        prose,
        0,
        singleFontResolver(FontSpec.fromAwt(slanted)),
        frc,
        true,
        TestMeasure.cellMetrics(slanted)
      )
    xs.length shouldBe legacy.length
    xs.zip(legacy).foreach((x, expected) => x shouldBe expected +- tolerance)
  }
