package com.serenity.ui.skia

import java.awt.Font
import java.text.BreakIterator

import com.serenity.SkiaPixels
import com.serenity.ui.renderer.FontSpec
import org.jetbrains.skia.{Paint, Surface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A line shaped once and drawn in two ranges draws every glyph exactly once: the ranges laid side by side, the second
  * at the x its first glyph has in the whole line, make the pixels of the whole line. (The check the AWT shaped-text
  * cache made by counting glyphs of its ranges; a Skia range is a text blob, which has no count to read.) Lines that
  * read right to left or reorder their clusters are compared with `TextLine` in `SkiaBidiAgreementSpec`, not here.
  */
class SkiaStretchSplitSpec extends AnyFlatSpec with Matchers:

  private val font = new SkiaFontCache().faceFor(FontSpec.fromAwt(new Font(Font.SERIF, Font.PLAIN, 14))).font

  private val leftToRightLines = List(
    "Latin"     -> "AVAWAY To the office, fi ffl waffle",
    "code"      -> "val x = a => b != c // note",
    "CJK"       -> "日本語のテキスト mixed 漢字 text",
    "combining" -> "é ạ̈ ô plain",
    "emoji"     -> "hello 😀 👨‍👩‍👧 x"
  )

  private def graphemeStarts(text: String): Vector[Int] =
    val breaks = BreakIterator.getCharacterInstance
    breaks.setText(text)
    Iterator.iterate(breaks.first())(_ => breaks.next()).takeWhile(_ != BreakIterator.DONE).toVector

  private def pixels(width: Int)(draw: (org.jetbrains.skia.Canvas, Paint) => Unit): Array[Int] =
    val surface = Surface.Companion.makeRasterN32Premul(width, 44)
    val paint   = new Paint()
    try
      val canvas = surface.getCanvas
      val _      = canvas.clear(0xffffffff)
      paint.setColor(0xff000000)
      draw(canvas, paint)
      SkiaPixels.read(surface)
    finally
      paint.close()
      surface.close()

  "A line drawn in two ranges" should "draw the pixels of the whole line, at every grapheme boundary a glyph starts at" in
    leftToRightLines.foreach { (name, text) =>
      val stretch = new SkiaShapedTextCache().stretchOf(text, font)
      val width   = 640
      val whole   = pixels(width)((canvas, paint) => stretch.draw(canvas, 0, text.length, 8f, 30f, paint))

      withClue(s"$name: ") {
        val compared = graphemeStarts(text).drop(1).dropRight(1).flatMap { split =>
          stretch.leftEdgeOf(split).map { edge =>
            val halves = pixels(width) { (canvas, paint) =>
              stretch.draw(canvas, 0, split, 8f, 30f, paint)
              stretch.draw(canvas, split, text.length, 8f + edge, 30f, paint)
            }
            withClue(s"split at $split: ")(java.util.Arrays.equals(halves, whole) shouldBe true)
          }
        }
        compared should not be empty
      }
    }
