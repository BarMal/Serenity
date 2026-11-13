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
