package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.ui.renderer.FontFirstUse

/** The cell rule of a code line. A grapheme cluster the terminal counts as two cells ([[CharWidth.ofCluster]]: a wide
  * character, an emoji sequence) takes exactly two cells of the primary font's float advance, whichever font draws its
  * glyph, and a one-cell cluster that a fallback font draws takes exactly one, so what follows either stays on the
  * primary's grid and lines up with what the terminal paints. The cluster's first codepoint carries the cells; its
  * other codepoints take none.
  */
private[layout] object WideCells:

  /** The first codepoint with a wide East Asian Width. */
  private val FirstWide = 0x1100

  def hasWide(text: String): Boolean =
    text.codePoints().anyMatch(codePoint => codePoint >= FirstWide && CharWidth.of(codePoint) == 2) ||
      (CharWidth.hasClusterChars(text) && hasEmojiCluster(text))

  @annotation.tailrec
  private def hasEmojiCluster(text: String, from: Int = 0): Boolean =
    from < text.length && {
      val end = CharWidth.clusterEnd(text, from)
      CharWidth.isEmojiCluster(text, from, end) || hasEmojiCluster(text, end)
    }

  /** One cell: the advance of `0` in `font`, as a float so a row of cells does not drift from the integer charWidth. */
  def cellAdvancePx(font: Font, frc: FontRenderContext): Float =
    FontFirstUse.settle(font, frc)
    font.createGlyphVector(frc, "0").getGlyphMetrics(0).getAdvance

  /** The cells the code line sets the cluster `text[from, until)` on, or 0 when it keeps its own advance: a wide
    * cluster, or a one-cell cluster drawn in a fallback font, which would otherwise keep its glyph's own advance and
    * push the columns after it off the grid.
    */
  def snapCells(text: String, from: Int, until: Int, inFallback: Boolean): Int =
    val cells = CharWidth.ofCluster(text, from, until)
    if cells == 2 || (inFallback && cells == 1) then cells else 0

  /** `advances` with each snapped cluster of `chars` set to its cells on its first character and nothing on the rest,
    * `inFallback` saying whether the character at an index is drawn in a fallback font. A cluster with a surrogate pair
    * is left to [[snapped]].
    */
  def snapAdvances(chars: Array[Char], advances: Array[Float], cellPx: Float, inFallback: Int => Boolean): Unit =
    if cellPx > 0.0f then
      val text = String(chars)
      @annotation.tailrec
      def walk(index: Int): Unit =
        if index < text.length then
          val end = CharWidth.clusterEnd(text, index)
          if !(index until end).exists(at => Character.isSurrogate(chars(at))) then
            val cells = snapCells(text, index, end, inFallback(index))
            if cells > 0 then
              advances(index) = cells * cellPx
              (index + 1 until end).foreach(inner => advances(inner) = 0.0f)
          walk(end)
      walk(0)

  /** `carets` (a leading stop per character plus the trailing edge) with each snapped cluster widened or narrowed to
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
          val end   = CharWidth.clusterEnd(text, index)
          val cells = snapCells(text, index, end, inFallback(index))
          val delta = if cells > 0 then cells * cellPx - (carets(end) - carets(index)) else 0.0f
          (index + 1 until end).foreach(inner => shifted(inner) = carets(inner) + shift + delta)
          walk(end, shift + delta)
      walk(0, 0.0f)
      IArray.unsafeFromArray(shifted)
