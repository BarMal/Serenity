package com.serenity.manuscript.layout

import java.util.Locale

import cats.syntax.all.*
import com.serenity.manuscript.typography.{LineAlignment, LineMetrics, PageTypography, TextMeasurer}
import com.serenity.manuscript.{Manuscript, ManuscriptMeta, ManuscriptPageNumbering}

/** Lays a [[Manuscript]] out on pages, in points, using nothing but the [[TextMeasurer]] port. The result is plain
  * data: a PDF painter, a print job and a page preview can all draw the same [[PagedDocument]] and so agree on where
  * every line falls.
  *
  * Body text is numbered from 1 and every body page carries the running head; the title page and dedication carry
  * neither. That rule is [[ManuscriptPageNumbering]], shared with the DOCX writer.
  */
object Paginator:

  def paginate(
    manuscript: Manuscript,
    typography: PageTypography,
    measure: TextMeasurer
  ): Either[PaginationError, PagedDocument] =
    for
      _ <- Either.cond(
        typography.alignment == LineAlignment.Ragged,
        (),
        PaginationError.UnsupportedAlignment(typography.alignment)
      )
      metrics <- measure.lineMetrics(typography.body).leftMap(PaginationError.Measure(_))
      setter = ParagraphSetter(typography, measure, Locale.forLanguageTag(manuscript.meta.language))
      groups <- FlowBuilder(typography, setter, metrics).groups(manuscript)
      geometry = Geometry(
        typography.margins.top,
        typography.textHeight,
        metrics.descent,
        typography.widows,
        typography.orphans
      )
      filled = PageFiller.fill(groups, geometry)
      pages <- filled
        .zip(bodyPagesBefore(filled))
        .zipWithIndex
        .traverse {
          case ((page, bodyIndex), index) =>
            numbered(page, index + 1, bodyIndex, manuscript.meta, typography, setter, metrics)
        }
    yield PagedDocument(pages)

  /** For each page, how many body pages come before it: front matter is not counted. */
  private def bodyPagesBefore(pages: Vector[FilledPage]): Vector[Int] =
    pages.scanLeft(0)((count, page) => if page.kind.isFront then count else count + 1).take(pages.size)

  private def numbered(
    page: FilledPage,
    number: Int,
    bodyIndex: Int,
    meta: ManuscriptMeta,
    typography: PageTypography,
    setter: ParagraphSetter,
    metrics: LineMetrics
  ): Either[PaginationError, Page] =
    val printed = Option.unless(page.kind.isFront)(ManuscriptPageNumbering.printedNumber(bodyIndex))
    val head = printed.fold(Right(None))(shown =>
      runningHead(ManuscriptPageNumbering.runningHead(typography.runningHead, meta, shown), typography, setter, metrics)
    )
    head.map(Page(number, printed, page.kind, _, page.lines))

  /** Top right, half way up the top margin. */
  private def runningHead(
    text: String,
    typography: PageTypography,
    setter: ParagraphSetter,
    metrics: LineMetrics
  ): Either[PaginationError, Option[PlacedLine]] =
    if text.isEmpty then Right(None)
    else
      setter.width(text).map { width =>
        val x = (typography.margins.left + typography.textWidth - width).toFloat
        Some(PlacedLine(typography.margins.top / 2f + metrics.ascent, Vector(setter.run(text, x))))
      }
