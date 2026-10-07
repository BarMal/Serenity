package com.serenity.manuscript.layout

import com.serenity.manuscript.typography.{FontSpec, LineAlignment, MeasureError}
import com.serenity.richtext.RichTextStyle

/** Why a page holds what it does; the painter and the running head both key off it. */
enum PageKind:
  case Title, Dedication, SectionStart, Body

  /** Front matter is unnumbered and carries no running head; see [[com.serenity.manuscript.ManuscriptPageNumbering]].
    */
  def isFront: Boolean = this == Title || this == Dedication

/** A piece of text in one face, with its left edge `x` in points from the page's left. `style` keeps the marks (an
  * underline, say) that the font face alone cannot express.
  */
final case class PlacedRun(text: String, font: FontSpec, x: Float, style: RichTextStyle)

/** `baselineY` is in points from the top of the page. A line's text keeps any trailing space it was broken after, so
  * the lines of a paragraph concatenate back to the paragraph.
  */
final case class PlacedLine(baselineY: Float, runs: Vector[PlacedRun]):
  def text: String = runs.map(_.text).mkString

/** `number` is the page's position in the document from 1, title page included. `printedNumber` is what the page shows:
  * none on front matter, and 1 on the first page of body text.
  */
final case class Page(
    number: Int,
    printedNumber: Option[Int],
    kind: PageKind,
    head: Option[PlacedLine],
    lines: Vector[PlacedLine]
)

final case class PagedDocument(pages: Vector[Page])

enum PaginationError:
  case Measure(error: MeasureError)
  case UnsupportedAlignment(alignment: LineAlignment)

  def message: String = this match
    case Measure(error)                  => error.message
    case UnsupportedAlignment(alignment) => s"$alignment text is not supported by the paginator yet"
