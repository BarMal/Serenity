package com.serenity.text

import scala.annotation.tailrec

/** How many of each line terminator a file held before `Rope` normalised them all to `\n`.
  *
  * Mixed files are real -- a merge or a generator can leave several -- and have no single correct answer. Saving writes
  * one terminator for the whole file, so a mixed file is rewritten by an ordinary save; [[isMixed]] is what lets the
  * editor say so beforehand.
  */
final case class LineEndingCounts(lf: Int, crlf: Int, cr: Int):

  def count(ending: LineEnding): Int =
    ending match
      case LineEnding.Lf   => lf
      case LineEnding.Crlf => crlf
      case LineEnding.Cr   => cr

  def isMixed: Boolean = LineEnding.values.count(ending => count(ending) > 0) > 1

  /** The majority terminator; a tie goes to the earliest of `Lf`, `Crlf`, `Cr`, so an empty or single-line file is
    * `Lf`.
    */
  def dominant: LineEnding = LineEnding.values.foldLeft(LineEnding.default)((best, ending) =>
    if count(ending) > count(best) then ending else best
  )

object LineEndingCounts:

  val empty: LineEndingCounts = LineEndingCounts(0, 0, 0)

  /** One pass over `rawContent`, counting a `\r\n` pair as a single terminator. */
  def of(rawContent: String): LineEndingCounts =
    @tailrec
    def loop(index: Int, lf: Int, crlf: Int, cr: Int): LineEndingCounts =
      if index >= rawContent.length then LineEndingCounts(lf, crlf, cr)
      else
        rawContent.charAt(index) match
          case '\n' => loop(index + 1, lf + 1, crlf, cr)
          case '\r' if index + 1 < rawContent.length && rawContent.charAt(index + 1) == '\n' =>
            loop(index + 2, lf, crlf + 1, cr)
          case '\r' => loop(index + 1, lf, crlf, cr + 1)
          case _    => loop(index + 1, lf, crlf, cr)
    loop(0, 0, 0, 0)
