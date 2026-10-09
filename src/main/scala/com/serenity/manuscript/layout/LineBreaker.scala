package com.serenity.manuscript.layout

import java.util.Locale

import scala.annotation.tailrec

import com.ibm.icu.text.BreakIterator

/** Greedy line breaking over a paragraph's cumulative widths. Offsets are UTF-16 indices into the paragraph text, and a
  * line's trailing whitespace hangs: it stays in the line's text but does not count towards its width.
  */
private[layout] object LineBreaker:

  final case class Span(start: Int, end: Int)

  private val Tolerance = 1e-3

  /** Where a line may end: the Unicode line-break opportunities of `text`, including its end. */
  def lineBoundaries(text: String, locale: Locale): Vector[Int] =
    val iterator = BreakIterator.getLineInstance(locale)
    iterator.setText(text)
    Iterator.iterate(iterator.first())(_ => iterator.next()).takeWhile(_ != BreakIterator.DONE).toVector

  /** Where a line may end when it has no word to respect, as in code: after any code point. */
  def codePointBoundaries(text: String): Vector[Int] =
    Iterator.iterate(0)(nextCodePoint(text, _)).takeWhile(_ <= text.length).toVector

  /** `result(i)` is the width of `text` before UTF-16 offset `i`. A surrogate pair's advance lands on its second half,
    * so the offset between the two halves, which is never a break, has the width of the start.
    */
  def prefixWidths(text: String, advances: Vector[Float]): Vector[Double] =
    text.codePoints.toArray.iterator
      .zip(advances.iterator)
      .foldLeft(Vector(0.0)) {
        case (widths, (codePoint, advance)) =>
          val before = widths.lastOption.getOrElse(0.0)
          val after  = before + advance
          if Character.charCount(codePoint) == 2 then widths :+ before :+ after else widths :+ after
      }

  def visibleWidth(text: String, widths: Vector[Double], span: Span): Double =
    widthAt(widths, visibleEnd(text, span)) - widthAt(widths, span.start)

  def breakLines(
    text: String,
    widths: Vector[Double],
    boundaries: Vector[Int],
    firstWidth: Double,
    width: Double
  ): Vector[Span] =
    @tailrec def loop(start: Int, available: Double, lines: Vector[Span]): Vector[Span] =
      if start >= text.length then lines
      else
        val end = lineEnd(text, widths, boundaries, start, available)
        loop(end, width, lines :+ Span(start, end))
    loop(0, firstWidth, Vector.empty)

  private def lineEnd(
    text: String,
    widths: Vector[Double],
    boundaries: Vector[Int],
    start: Int,
    available: Double
  ): Int =
    def fits(end: Int): Boolean = visibleWidth(text, widths, Span(start, end)) <= available + Tolerance
    val ahead                   = boundaries.dropWhile(_ <= start)
    ahead.takeWhile(fits).lastOption.getOrElse(forcedEnd(text, start, ahead.headOption.getOrElse(text.length), fits))

  /** A word wider than the line is broken at the last code point that fits, and always after at least one. */
  private def forcedEnd(text: String, start: Int, wordEnd: Int, fits: Int => Boolean): Int =
    val first = nextCodePoint(text, start)
    Iterator
      .iterate(first)(nextCodePoint(text, _))
      .takeWhile(_ <= wordEnd)
      .takeWhile(fits)
      .foldLeft(first)((_, end) => end)

  @tailrec private def visibleEnd(text: String, span: Span): Int =
    if span.end > span.start && Character.isWhitespace(text.charAt(span.end - 1)) then
      visibleEnd(text, Span(span.start, span.end - 1))
    else span.end

  private def nextCodePoint(text: String, offset: Int): Int =
    if offset >= text.length then Int.MaxValue else offset + Character.charCount(text.codePointAt(offset))

  private def widthAt(widths: Vector[Double], offset: Int): Double =
    widths.lift(offset).orElse(widths.lastOption).getOrElse(0.0)
