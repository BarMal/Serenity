package com.serenity.ui.layout

import java.awt.Font

import com.serenity.ui.renderer.FontSpec
import com.serenity.ui.skia.SkiaTextMeasurer

/** [[SkiaTextMeasurer]] keeps the measurer contract: on the bundled fonts and on a system sans, over generated text
  * with kerning pairs, ligatures, combining marks, emoji clusters and right-to-left words.
  */
class SkiaTextMeasurerSpec extends TextMeasurerContract:

  // Two measurers with their own caches, so "alike" properties compare a warm one with a cold one at the same key.
  private val first  = new SkiaTextMeasurer()
  private val second = new SkiaTextMeasurer()

  protected def measurers: Vector[TextMeasurer] = Vector(first, second)

  protected def fonts: Vector[FontSpec] =
    TextMeasureCorpus.fonts.map(pinned => FontSpec.fromAwt(pinned.font)) :+
      FontSpec.fromAwt(Font(Font.SANS_SERIF, Font.PLAIN, 13))

  property(
    "Skiko's font metrics are still answered, and a 14 pt font's cell has an ascent inside a plausible line height"
  ) {
    SkiaTextMeasurer.hasVerticalMetrics shouldBe true
    TextMeasureCorpus.fonts.foreach { pinned =>
      val cell = first.cellMetrics(FontSpec.fromAwt(pinned.font))
      withClue(pinned.label) {
        cell.lineHeight should (be > 8 and be < 40)
        cell.ascent should (be >= 1 and be < cell.lineHeight)
        cell.charWidth should (be >= 5 and be < 25)
      }
    }
  }
