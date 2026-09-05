package com.serenity.ui.tui

import java.awt.Color
import java.awt.image.BufferedImage

/** Converts a decoded image into a grid of half-block terminal cells: each cell is an upper-half-block glyph (`▀`)
  * whose foreground paints its top sub-pixel and whose background paints its bottom one, doubling a terminal's
  * effective vertical resolution -- the technique notcurses, ratatui-image, and Jexer all use for pixel art in a
  * plain ANSI-color terminal (out of scope here: Sixel/Kitty/iTerm2 true-pixel protocols, which need a different
  * capability entirely).
  *
  * Pure given an already-decoded `BufferedImage` -- no terminal, no IO -- so the mapping from source pixels to
  * glyph/fg/bg triples can be asserted directly.
  */
object HalfBlockImageRenderer:

  val UpperHalfBlock: Char = '▀'

  final case class HalfBlockCell(foreground: Color, background: Color)

  /** Nearest-neighbor downsamples `image` to `cellsWide` columns and `cellsHigh * 2` sub-pixel rows, then pairs each
    * vertically-adjacent sub-pixel pair into one cell. A cell whose both sub-pixels are fully transparent (alpha 0)
    * comes back as `None`, telling the caller to leave whatever was already on screen there instead of painting over
    * it with an arbitrary color; a cell with exactly one transparent half substitutes `fallback` for that half only.
    */
  def render(
    image: BufferedImage,
    cellsWide: Int,
    cellsHigh: Int,
    fallback: Color
  ): Vector[Vector[Option[HalfBlockCell]]] =
    if cellsWide <= 0 || cellsHigh <= 0 then Vector.empty
    else
      val subRows = cellsHigh * 2
      Vector.tabulate(cellsHigh) { row =>
        Vector.tabulate(cellsWide) { col =>
          val topArgb    = samplePixel(image, cellsWide, subRows, col, row * 2)
          val bottomArgb = samplePixel(image, cellsWide, subRows, col, row * 2 + 1)
          val topTransparent    = isTransparent(topArgb)
          val bottomTransparent = isTransparent(bottomArgb)
          if topTransparent && bottomTransparent then None
          else
            Some(
              HalfBlockCell(
                foreground = if topTransparent then fallback else new Color(topArgb, true),
                background = if bottomTransparent then fallback else new Color(bottomArgb, true)
              )
            )
        }
      }

  private def isTransparent(argb: Int): Boolean = (argb >>> 24) == 0

  private def samplePixel(image: BufferedImage, targetWidth: Int, targetHeight: Int, col: Int, row: Int): Int =
    val sourceX = (col * image.getWidth) / targetWidth
    val sourceY = (row * image.getHeight) / targetHeight
    image.getRGB(sourceX.min(image.getWidth - 1), sourceY.min(image.getHeight - 1))
