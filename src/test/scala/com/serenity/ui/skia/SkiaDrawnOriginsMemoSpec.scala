package com.serenity.ui.skia

import java.awt.Font

import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.FontSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A wrap measures the same candidate row again and again, and text that `TextLine` draws (an emoji sequence, a mark on
  * a letter) costs milliseconds to shape, so the measurer shapes a given text in a given font once.
  */
class SkiaDrawnOriginsMemoSpec extends AnyFlatSpec with Matchers:

  private val faces     = new SkiaFontCache()
  private val stretches = new SkiaShapedTextCache(ligaturesOf = faces.ligaturesOf)
  private val measurer  = new SkiaTextMeasurer(faces, stretches)
  private val fonts     = LineFonts(FontSpec.fromAwt(Font(Font.SANS_SERIF, Font.PLAIN, 14)), Vector.empty)

  private val drawnByTextLine = "a 👍🏽 and é here"

  "Measuring text that TextLine draws" should "shape it once, however often its carets are asked for" in {
    val first  = measurer.exactCaretXs(drawnByTextLine, 0, fonts)
    val shaped = stretches.lineShapings
    val again  = measurer.exactCaretXs(drawnByTextLine, 0, fonts)

    shaped should be > 0L
    stretches.lineShapings shouldBe shaped
    again.toVector shouldBe first.toVector
  }

  it should "shape a different text afresh" in {
    val _      = measurer.exactCaretXs(drawnByTextLine, 0, fonts)
    val shaped = stretches.lineShapings
    val _      = measurer.exactCaretXs(drawnByTextLine + " more", 0, fonts)

    stretches.lineShapings should be > shaped
  }
