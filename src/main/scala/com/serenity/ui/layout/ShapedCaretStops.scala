package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.ui.renderer.ShapedStretch

/** Caret stops read off the glyphs the draw path paints: a character's stop is where the glyph shaped for it begins in
  * reading direction, so a caret drawn there touches its glyph. That is the glyph's x for a left-to-right character and
  * its right edge for a right-to-left one. Kerning a pair's second glyph is already in its x; a character inside a
  * ligature has no glyph of its own and takes the stop of the next character, which `normalizeCollapsedCarets` then
  * spreads evenly across the ligature.
  */
private[layout] object ShapedCaretStops:

  /** The stops of a row. `trailing`, when the row reads left to right, holds for each boundary the right edge of the
    * characters before it shaped alone, which the stop itself cannot say: a stop carries the kerning of the pair it
    * ends, and a row that ends there feels none.
    */
  final case class Stops(carets: IArray[Float], trailing: Option[IArray[Float]])

  /** `text[start, end)` measured in `font`. */
  final case class Span(start: Int, end: Int, font: Font)

  /** Each character's stop plus the trailing edge, for `text` cut into consecutive `spans` of one font each. Nothing
    * kerns across a change of font, so every span is shaped on its own and starts where the one before it ended.
    */
  def of(text: String, spans: Vector[Span], frc: FontRenderContext): Stops =
    val carets   = new Array[Float](text.length + 1)
    val trailing = new Array[Float](text.length + 1)
    val (advance, readsRightToLeft) = spans.foldLeft((0.0f, false)) {
      case ((offset, rightToLeft), span) =>
        val stretch = ShapedStretch.shape(text.substring(span.start, span.end), span.font, frc)
        val length  = span.end - span.start
        val stops   = spanStops(stretch, length)
        val pulls   = leftToRightPulls(stretch, length)
        (0 until length).foreach { char =>
          carets(span.start + char) = offset + stops(char)
          trailing(span.start + char) = offset + stops(char) - pulls(char)
        }
        (offset + stretch.advance, rightToLeft || (0 until stretch.glyphCount).exists(stretch.isRightToLeft))
    }
    carets(text.length) = advance
    trailing(text.length) = advance
    Stops(IArray.unsafeFromArray(carets), Option.unless(readsRightToLeft)(IArray.unsafeFromArray(trailing)))

  private def spanStops(stretch: ShapedStretch, length: Int): Array[Float] =
    val own = Array.fill(length)(Float.NaN)
    (0 until stretch.glyphCount).foreach { glyph =>
      val char = stretch.charIndexAt(glyph)
      if char >= 0 && char < length then
        val edge =
          if stretch.isRightToLeft(glyph) then stretch.xAt(glyph) + stretch.advanceAt(glyph) else stretch.xAt(glyph)
        val keep =
          own(char).isNaN || (if stretch.isRightToLeft(glyph) then edge > own(char) else edge < own(char))
        if keep then own(char) = edge
    }
    val resolved = new Array[Float](length)
    @annotation.tailrec
    def fill(char: Int, next: Float): Unit =
      if char >= 0 then
        resolved(char) = if own(char).isNaN then next else own(char)
        fill(char - 1, resolved(char))
    fill(length - 1, stretch.advance)
    resolved

  /** How far past the glyphs before it each character's own glyph starts: the kerning of the pair it ends, which a row
    * that ends before it does not feel. A character with no glyph of its own is inside a ligature, and a glyph with no
    * advance is a mark that sits on its base; neither is pulled by what comes before it.
    */
  private def leftToRightPulls(stretch: ShapedStretch, length: Int): Array[Float] =
    val pulls = new Array[Float](length)
    val seen  = new Array[Boolean](length)
    val reach = (0 until stretch.glyphCount).scanLeft(Float.NegativeInfinity) { (furthest, glyph) =>
      math.max(furthest, stretch.xAt(glyph) + stretch.advanceAt(glyph))
    }
    (0 until stretch.glyphCount).foreach { glyph =>
      val char = stretch.charIndexAt(glyph)
      if char >= 0 && char < length && !seen(char) then
        seen(char) = true
        if glyph > 0 && char > 0 && !stretch.isRightToLeft(glyph) && stretch.advanceAt(glyph) > 0.0f then
          pulls(char) = stretch.xAt(glyph) - reach(glyph)
    }
    pulls
