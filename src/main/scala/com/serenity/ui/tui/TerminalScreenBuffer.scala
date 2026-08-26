package com.serenity.ui.tui

import java.awt.Color
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.theme.TextStyle

/** A cell grid for the terminal backend: `(codepoint, fg, bg, style)` per cell, filled via the same `putString`/
  * `fillRect`/colour-and-style-state shape `RenderSurface` already exposes, so `TerminalRenderSurface` (a later issue)
  * can forward straight onto it. Internally mutable, encapsulated behind [[snapshot]] -- callers never see or hold a
  * reference to the backing grid.
  */
final class TerminalScreenBuffer(val width: Int, val height: Int):
  import TerminalScreenBuffer.Clip

  private val fgColorRef     = AtomicReference[Color](Color.WHITE)
  private val bgColorRef     = AtomicReference[Color](Color.BLACK)
  private val activeStyleRef = AtomicReference[TextStyle](TextStyle.normal)
  private val clipRef        = AtomicReference[Clip](Clip(0, 0, width, height))

  private val grid: Array[Array[TerminalCell]] =
    Array.fill(height, width)(TerminalCell.blank(fgColorRef.get(), bgColorRef.get()))

  def setForegroundColor(color: Color): Unit = fgColorRef.set(color)
  def setBackgroundColor(color: Color): Unit = bgColorRef.set(color)
  def getBackgroundColor: Color              = bgColorRef.get()

  def enableStyle(style: TextStyle): Unit = activeStyleRef.set(activeStyleRef.get().combine(style))

  def disableStyle(style: TextStyle): Unit =
    val current = activeStyleRef.get()
    activeStyleRef.set(
      TextStyle(
        isBold = current.isBold && !style.isBold,
        isItalic = current.isItalic && !style.isItalic,
        isUnderlined = current.isUnderlined && !style.isUnderlined,
        fontFamily = current.fontFamily,
        fontSize = current.fontSize
      )
    )

  /** Restrict [[putString]] and [[fillRect]] to a rectangular region in cell coordinates for the duration of `body`. */
  def withClip[A](x: Int, y: Int, w: Int, h: Int)(body: => A): A =
    val previous = clipRef.get()
    clipRef.set(Clip(x, y, x + w, y + h).intersect(previous))
    try body
    finally clipRef.set(previous)

  def putString(x: Int, y: Int, s: String): Unit =
    val _ = s.codePoints().toArray.foldLeft(x) { (col, codePoint) =>
      val glyphWidth = CharWidth.of(codePoint)
      val span       = if glyphWidth == 2 then CellSpan.Wide else CellSpan.Narrow
      writeCell(col, y, TerminalCell(codePoint, fgColorRef.get(), bgColorRef.get(), activeStyleRef.get(), span))
      if glyphWidth == 2 then
        writeCell(col + 1, y, TerminalCell.continuation(fgColorRef.get(), bgColorRef.get(), activeStyleRef.get()))
      col + glyphWidth
    }

  def fillRect(x: Int, y: Int, w: Int, h: Int, char: Char): Unit =
    for
      row <- y until y + h
      col <- x until x + w
    do
      writeCell(
        col,
        row,
        TerminalCell(char.toInt, fgColorRef.get(), bgColorRef.get(), activeStyleRef.get(), CellSpan.Narrow)
      )

  def snapshot: TerminalFrame = TerminalFrame(width, height, grid.map(_.toVector).toVector)

  private def inBounds(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height

  /** Overwrite one cell, then repair whichever half of a wide glyph the overwrite just orphaned: if the cell replaced
    * was a [[CellSpan.Wide]] leader, its continuation to the right no longer has a leader; if it was a
    * [[CellSpan.Continuation]], its leader to the left no longer has a continuation. Either way the leftover half is
    * blanked rather than left to render a dangling glyph fragment.
    */
  private def writeCell(x: Int, y: Int, cell: TerminalCell): Unit =
    if inBounds(x, y) && clipRef.get().contains(x, y) then
      val previous = grid(y)(x)
      grid(y)(x) = cell
      if previous.span == CellSpan.Wide && cell.span != CellSpan.Wide then blankIfContinuation(x + 1, y)
      if previous.span == CellSpan.Continuation && cell.span != CellSpan.Continuation then blankIfWideLeader(x - 1, y)

  private def blankIfContinuation(x: Int, y: Int): Unit =
    if inBounds(x, y) && grid(y)(x).span == CellSpan.Continuation then
      grid(y)(x) = TerminalCell.blank(fgColorRef.get(), bgColorRef.get())

  private def blankIfWideLeader(x: Int, y: Int): Unit =
    if inBounds(x, y) && grid(y)(x).span == CellSpan.Wide then
      grid(y)(x) = TerminalCell.blank(fgColorRef.get(), bgColorRef.get())

object TerminalScreenBuffer:

  final private case class Clip(x0: Int, y0: Int, x1: Int, y1: Int):
    def contains(x: Int, y: Int): Boolean = x >= x0 && x < x1 && y >= y0 && y < y1
    def intersect(other: Clip): Clip =
      Clip(math.max(x0, other.x0), math.max(y0, other.y0), math.min(x1, other.x1), math.min(y1, other.y1))
