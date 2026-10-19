package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.ui.renderer.FontFirstUse

/** The two-column rule of a code line. A character the terminal counts as two cells ([[CharWidth]]) takes exactly two
  * cells of the primary font's float advance, whichever font draws its glyph, so a wide character in a code buffer
  * lines up with the rest of the grid and with what the terminal paints.
  */
private[layout] object WideCells:

  /** The first codepoint with a wide East Asian Width. */
  private val FirstWide = 0x1100

  def hasWide(text: String): Boolean =
    text.codePoints().anyMatch(codePoint => codePoint >= FirstWide && CharWidth.of(codePoint) == 2)

  /** One cell: the advance of `0` in `font`, as a float so a row of cells does not drift from the integer charWidth. */
  def cellAdvancePx(font: Font, frc: FontRenderContext): Float =
    FontFirstUse.settle(font, frc)
    font.createGlyphVector(frc, "0").getGlyphMetrics(0).getAdvance

  /** `advances` with each wide character of `chars` set to two cells. A surrogate pair is left to [[snapped]]. */
  def snapAdvances(chars: Array[Char], advances: Array[Float], cellPx: Float): Unit =
    if cellPx > 0.0f then
      chars.indices.foreach { index =>
        if !Character.isSurrogate(chars(index)) && isWide(chars(index).toInt) then advances(index) = 2 * cellPx
      }

  /** `carets` (a leading stop per character plus the trailing edge) with each wide character widened or narrowed to
    * two cells and every stop after it moved to match.
    */
  def snapped(carets: IArray[Float], text: String, cellPx: Float): IArray[Float] =
    if cellPx <= 0.0f || !hasWide(text) then carets
    else
      val shifted = new Array[Float](carets.length)
      @annotation.tailrec
      def walk(index: Int, shift: Float): Unit =
        shifted(index) = carets(index) + shift
        if index < text.length then
          val codePoint = text.codePointAt(index)
          val end       = index + Character.charCount(codePoint)
          val delta     = if isWide(codePoint) then 2 * cellPx - (carets(end) - carets(index)) else 0.0f
          (index + 1 until end).foreach(inner => shifted(inner) = carets(inner) + shift + delta)
          walk(end, shift + delta)
      walk(0, 0.0f)
      IArray.unsafeFromArray(shifted)

  private def isWide(codePoint: Int): Boolean = codePoint >= FirstWide && CharWidth.of(codePoint) == 2
