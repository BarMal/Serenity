package com.serenity.ui.skia

import java.awt.Font

import com.serenity.SkiaPixels
import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.FontSpec
import org.jetbrains.skia.{Canvas, Paint, Surface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A line that mixes left-to-right and right-to-left text is measured where Skia draws it: the line drawn whole, and
  * then each cluster drawn alone at the x the measured caret gives it (the caret of a right-to-left cluster is its
  * right edge, so its glyphs begin a cluster's width to the left), land on the same pixels.
  */
final class SkiaBidiAgreementSpec extends AnyFlatSpec with Matchers:

  private val faces     = new SkiaFontCache()
  private val stretches = new SkiaShapedTextCache(ligaturesOf = faces.ligaturesOf)
  private val measurer  = new SkiaTextMeasurer(faces, stretches)

  private val Height     = 48
  private val Baseline   = 32f
  private val Margin     = 12
  private val AlphaSlack = 2

  private val lines = Vector(
    "mixed"         -> "abc שלום עולם def",
    "right to left" -> "שלום עולם",
    "arabic"        -> "ab مرحبا cd",
    "digits"        -> "שלום 123 abc"
  )

  private def alphaOf(width: Int)(draw: (Canvas, Paint) => Unit): Array[Int] =
    val surface = Surface.Companion.makeRasterN32Premul(width, Height)
    val paint   = new Paint()
    paint.setColor(0xff000000)
    draw(surface.getCanvas, paint)
    SkiaPixels.read(surface).map(_ >>> 24)

  private def worstAlphaDifference(text: String, shift: Float): Int =
    val spec    = FontSpec.fromAwt(new Font(Font.SERIF, Font.PLAIN, 14))
    val font    = faces.faceFor(spec).font
    val carets  = measurer.rawCaretXs(text, 0, LineFonts(spec, Vector.empty))
    val starts  = measurer.clusterStarts(text, spec)
    val ends    = starts.drop(1) :+ text.length
    val bidi    = java.text.Bidi(text, java.text.Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT)
    val stretch = stretches.stretchOf(text, font)
    val width   = math.ceil(carets.max).toInt + 2 * Margin
    val whole = alphaOf(width)((canvas, paint) => stretch.draw(canvas, 0, text.length, Margin.toFloat, Baseline, paint))
    val pieces = alphaOf(width) { (canvas, paint) =>
      starts.zip(ends).foreach { (from, until) =>
        val advance = measurer.glyphWidthPx(spec, text.substring(from, until))
        val left    = if bidi.getLevelAt(from) % 2 == 1 then carets(from) - advance else carets(from)
        stretch.draw(canvas, from, until, Margin + left + shift, Baseline, paint)
      }
    }
    whole.zip(pieces).foldLeft(0)((worst, pair) => math.max(worst, math.abs(pair._1 - pair._2)))

  "Each cluster of a mixed line, drawn at its measured caret" should "land where the line draws it" in {
    val off = lines.map((name, text) => name -> worstAlphaDifference(text, 0.0f)).filter(_._2 > AlphaSlack)
    withClue(off.map((name, worst) => s"$name differs by $worst").mkString("; ")) {
      off shouldBe empty
    }
  }

  "The same comparison" should "tell a cluster half a pixel from its caret" in {
    worstAlphaDifference(lines.head._2, 0.5f) should be > AlphaSlack
  }
