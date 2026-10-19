package com.serenity.ui.layout

import java.awt.font.TextAttribute

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The terminal's carets are cells, so a kerned font must not move them: only the measured, pixel layout follows
  * glyphs.
  */
class CaretStopsTuiSpec extends AnyFlatSpec with Matchers:

  private val frc = TextLayoutSnapshot.defaultFontRenderContext()
  private val kernedInter =
    BundledInterFont.plain(14f).deriveFont(java.util.Map.of(TextAttribute.KERNING, TextAttribute.KERNING_ON))

  "A forced cell layout" should "place every caret stop on its cell, whatever the font kerns" in {
    val text = "AVAWAY To WWWW"
    val stops =
      TextLayoutSnapshot.caretXsForText(text, kernedInter, frc, Some(CellMetrics.cellUnit), forceCellLayout = true)

    stops shouldBe Vector.tabulate(text.length + 1)(_.toFloat)
  }
