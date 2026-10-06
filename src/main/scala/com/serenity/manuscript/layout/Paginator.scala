package com.serenity.manuscript.layout

import java.util.Locale

import cats.syntax.all.*
import com.serenity.manuscript.typography.{LineAlignment, LineMetrics, PageTypography, TextMeasurer}
import com.serenity.manuscript.{Manuscript, ManuscriptMeta}

/** Lays a [[Manuscript]] out on pages, in points, using nothing but the [[TextMeasurer]] port. The result is plain
  * data: a PDF painter, a print job and a page preview can all draw the same [[PagedDocument]] and so agree on where
  * every line falls.
  *
  * Pages are numbered from 1 including the title page. Running heads appear on body pages only.
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
      pages <- PageFiller
        .fill(groups, geometry)
        .zipWithIndex
        .traverse((page, index) => numbered(page, index + 1, manuscript.meta, typography, setter, metrics))
    yield PagedDocument(pages)

  private def numbered(
    page: FilledPage,
    number: Int,
    meta: ManuscriptMeta,
    typography: PageTypography,
    setter: ParagraphSetter,
    metrics: LineMetrics
  ): Either[PaginationError, Page] =
    val head =
      if page.kind.hasRunningHead then
        runningHead(headText(typography.runningHead, meta, number), typography, setter, metrics)
      else Right(None)
    head.map(Page(number, page.kind, _, page.lines))

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

  /** A slash-separated segment left empty (a manuscript with no surname) is dropped along with its separator. */
  private[layout] def headText(template: String, meta: ManuscriptMeta, number: Int): String =
    template
      .split(" / ", -1)
      .toList
      .map(
        _.replace("<$surname>", meta.author.surname)
          .replace("<$keyword>", meta.shortTitle)
          .replace("<$p>", number.toString)
          .trim
      )
      .filter(_.nonEmpty)
      .mkString(" / ")
