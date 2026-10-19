package com.serenity.ui.layout

import scala.annotation.tailrec

import com.serenity.state.models.MeasuredRow
import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver

/** The stretches of a measured row that paint must draw apart from their neighbours, as the `fontRuns` a
  * [[MeasuredRow]] carries. Read from the resolver that measured the row, not worked out again from the row's text: the
  * itemiser lets neutral characters follow the cluster before them, which a row cut out of the line cannot see.
  */
private[layout] object RowFontRuns:

  def of(resolver: LineFontResolver, text: String, startColumn: Int, endColumn: Int): IArray[Int] =
    val slotRuns = resolver.slotRuns(startColumn, endColumn)
    val snapping = resolver.wideCellPx > 0.0f && WideCells.hasWide(text)
    if slotRuns.isEmpty && !snapping then MeasuredRow.NoFontRuns
    else
      val slots = new Array[Int](text.length)
      slotRuns.foreach((from, until, slot) =>
        java.util.Arrays.fill(slots, from - startColumn, until - startColumn, slot.index)
      )

      @tailrec
      def walk(index: Int, openStart: Int, openSlot: Int, found: Vector[Int]): Vector[Int] =
        def closed(end: Int): Vector[Int] =
          if openStart >= 0 then found ++ Vector(openStart, end, openSlot) else found
        if index >= text.length then closed(index)
        else
          val codePoint = text.codePointAt(index)
          val next      = index + Character.charCount(codePoint)
          val slot      = slots(index)
          if snapping && WideCells.isWide(codePoint) then walk(next, -1, 0, closed(index) ++ Vector(index, next, slot))
          else if slot == 0 then walk(next, -1, 0, closed(index))
          else if openStart >= 0 && openSlot == slot then walk(next, openStart, openSlot, found)
          else walk(next, index, slot, closed(index))

      val runs = walk(0, -1, 0, Vector.empty)
      if runs.isEmpty then MeasuredRow.NoFontRuns else IArray.from(runs)
