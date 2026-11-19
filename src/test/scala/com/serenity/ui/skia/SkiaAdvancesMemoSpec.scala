package com.serenity.ui.skia

import java.awt.Font
import java.lang.ref.WeakReference

import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.FontSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A paragraph's advances do not depend on the width it is wrapped at, so a wrap at another width, or of the same text
  * elsewhere, reads what was measured instead of shaping it again, for as long as anything holds the measurement: the
  * wrap that made it keeps it alive, so the memo adds nothing to what a runtime holds.
  */
class SkiaAdvancesMemoSpec extends AnyFlatSpec with Matchers:

  private val faces     = new SkiaFontCache()
  private val stretches = new SkiaShapedTextCache(ligaturesOf = faces.ligaturesOf)
  private val measurer  = new SkiaTextMeasurer(faces, stretches)
  private val plain     = LineFonts(FontSpec.fromAwt(Font(Font.SANS_SERIF, Font.PLAIN, 14)), Vector.empty)
  private val larger    = LineFonts(FontSpec.fromAwt(Font(Font.SANS_SERIF, Font.PLAIN, 18)), Vector.empty)
  private val text      = "The quick brown fox jumps over the lazy dog, again and again."

  "Measuring a paragraph's advances" should "give the same measurement for the same text and fonts while it is held" in {
    val first  = measurer.advances(text, 0, text.length, 0, plain)
    val second = measurer.advances(text, 0, text.length, 0, plain)

    (first eq second) shouldBe true
  }

  it should "give the same measurement wherever the text sits in a line, when its fonts are the same" in {
    val line   = s"prefix. $text"
    val first  = measurer.advances(text, 0, text.length, 0, plain)
    val second = measurer.advances(line, 8, line.length, 8, plain)

    (first eq second) shouldBe true
  }

  it should "measure another font or other text afresh" in {
    val first = measurer.advances(text, 0, text.length, 0, plain)

    (first eq measurer.advances(text, 0, text.length, 0, larger)) shouldBe false
    (first eq measurer.advances(text + "!", 0, text.length + 1, 0, plain)) shouldBe false
  }

  it should "keep nothing that nobody else holds" in {
    val released = new WeakReference(measurer.advances(s"$text unheld", 0, text.length + 7, 0, plain))

    @annotation.tailrec
    def collected(rounds: Int): Boolean =
      System.gc()
      if released.get == null then true
      else if rounds == 0 then false
      else
        Thread.sleep(20)
        collected(rounds - 1)

    collected(50) shouldBe true
  }
