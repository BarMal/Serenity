package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.ui.renderer.FontFirstUse

/** The cell rule of a code line. A character the terminal counts as two cells ([[CharWidth]]) takes exactly two cells
  * of the primary font's float advance, whichever font draws its glyph, and a one-cell character that a fallback font
  * draws takes exactly one, so what follows either stays on the primary's grid and lines up with what the terminal
  * paints.
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

  /** Whether the code line sets `codePoint` on the cell grid: a wide character, or a one-cell character drawn in a
    * fallback font, which would otherwise keep its glyph's own advance and push the columns after it off the grid.
    */
  def isSnapped(codePoint: Int, inFallback: Boolean): Boolean =
    isWide(codePoint) || (inFallback && CharWidth.of(codePoint) == 1)

  private def cellsOf(codePoint: Int): Int = if isWide(codePoint) then 2 else 1

  /** `advances` with each snapped character of `chars` set to its cells, `inFallback` saying whether the character at
    * an index is drawn in a fallback font. A surrogate pair is left to [[snapped]].
    */
  def snapAdvances(chars: Array[Char], advances: Array[Float], cellPx: Float, inFallback: Int => Boolean): Unit =
    if cellPx > 0.0f then
      chars.indices.foreach { index =>
        val char = chars(index)
        if !Character.isSurrogate(char) && isSnapped(char.toInt, inFallback(index)) then
          advances(index) = cellsOf(char.toInt) * cellPx
      }

  /** `carets` (a leading stop per character plus the trailing edge) with each snapped character widened or narrowed to
    * its cells and every stop after it moved to match. `inFallback` says whether the character at an index of `text` is
    * drawn in a fallback font.
    */
  def snapped(carets: IArray[Float], text: String, cellPx: Float, inFallback: Int => Boolean): IArray[Float] =
    if cellPx <= 0.0f then carets
    else
      val shifted = new Array[Float](carets.length)
      @annotation.tailrec
      def walk(index: Int, shift: Float): Unit =
        shifted(index) = carets(index) + shift
        if index < text.length then
          val codePoint = text.codePointAt(index)
          val end       = index + Character.charCount(codePoint)
          val delta =
            if isSnapped(codePoint, inFallback(index)) then cellsOf(codePoint) * cellPx - (carets(end) - carets(index))
            else 0.0f
          (index + 1 until end).foreach(inner => shifted(inner) = carets(inner) + shift + delta)
          walk(end, shift + delta)
      walk(0, 0.0f)
      IArray.unsafeFromArray(shifted)

  def isWide(codePoint: Int): Boolean = codePoint >= FirstWide && CharWidth.of(codePoint) == 2
