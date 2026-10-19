package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.ui.renderer.ShapedStretch

/** Caret stops read off the glyphs the draw path paints: a character's stop is the x of the glyph shaped for it, so a
  * caret drawn there touches its glyph. Kerning a pair's second glyph is already in its x; a character inside a
  * ligature has no glyph of its own and takes the x of the next glyph, which `normalizeCollapsedCarets` then spreads
  * evenly across the ligature.
  */
private[layout] object ShapedCaretStops:

  /** `text[start, end)` measured in `font`. */
  final case class Span(start: Int, end: Int, font: Font)

  /** Each character's stop plus the trailing edge, for `text` cut into consecutive `spans` of one font each. Nothing
    * kerns across a change of font, so every span is shaped on its own and starts where the one before it ended.
    */
  def of(text: String, spans: Vector[Span], frc: FontRenderContext): IArray[Float] =
    val stops = new Array[Float](text.length + 1)
    val advance = spans.foldLeft(0.0f) { (offset, span) =>
      val stretch = ShapedStretch.shape(text.substring(span.start, span.end), span.font, frc)
      spanStops(stretch, span.end - span.start).zipWithIndex.foreach((x, char) => stops(span.start + char) = offset + x)
      offset + stretch.advance
    }
    stops(text.length) = advance
    IArray.unsafeFromArray(stops)

  private def spanStops(stretch: ShapedStretch, length: Int): Array[Float] =
    val own = Array.fill(length)(Float.NaN)
    (0 until stretch.glyphCount).foreach { glyph =>
      val char = stretch.charIndexAt(glyph)
      if char >= 0 && char < length && (own(char).isNaN || stretch.xAt(glyph) < own(char)) then
        own(char) = stretch.xAt(glyph)
    }
    val resolved = new Array[Float](length)
    @annotation.tailrec
    def fill(char: Int, next: Float): Unit =
      if char >= 0 then
        resolved(char) = if own(char).isNaN then next else own(char)
        fill(char - 1, resolved(char))
    fill(length - 1, stretch.advance)
    resolved
