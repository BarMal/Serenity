package com.serenity.manuscript.layout

import java.util.Locale

import cats.syntax.all.*
import com.serenity.manuscript.typography.{FaceStyle, FontSpec, PageTypography, TextMeasurer}
import com.serenity.richtext.{InlineMark, RichTextRun, RichTextStyle}

/** How the lines of one paragraph sit between the margins. `inset` pushes every line in from the left; `firstIndent`
  * only the first. `anywhere` lets a line end after any code point, which is what code wants.
  */
final private[layout] case class Placement(
    firstIndent: Float,
    inset: Float,
    centred: Boolean,
    anywhere: Boolean = false
)

private[layout] object Placement:
  val flush: Placement  = Placement(0f, 0f, centred = false)
  val centre: Placement = Placement(0f, 0f, centred = true)

/** Turns runs of rich text into lines of positioned runs, measuring through the [[TextMeasurer]] port only. A
  * manuscript is set in one face and size, so only a run's marks pick the face, never its own font.
  */
final private[layout] class ParagraphSetter(t: PageTypography, measure: TextMeasurer, locale: Locale):

  def font(style: RichTextStyle): FontSpec =
    val bold   = style.marks.contains(InlineMark.Bold)
    val italic = style.marks.contains(InlineMark.Italic)
    val face = (bold, italic) match
      case (true, true)   => FaceStyle.BoldItalic
      case (true, false)  => FaceStyle.Bold
      case (false, true)  => FaceStyle.Italic
      case (false, false) => FaceStyle.Regular
    t.body.copy(style = face)

  def width(text: String): Either[PaginationError, Double] =
    advances(ProseText.clean(text), t.body).map(_.map(_.toDouble).sum)

  def run(text: String, x: Float): PlacedRun =
    PlacedRun(ProseText.clean(text), t.body, x, RichTextStyle.empty)

  /** Sets one paragraph. A line break in the text starts a new line, which takes no first-line indent. */
  def set(runs: List[RichTextRun], placement: Placement): Either[PaginationError, Vector[Vector[PlacedRun]]] =
    val prepared = ProseText.prepare(runs)
    val first =
      if prepared.indented && !placement.centred then
        placement.copy(firstIndent = placement.firstIndent.max(t.firstLineIndent))
      else placement
    prepared.lines.zipWithIndex.flatTraverse { (line, index) =>
      setLine(line, if index == 0 then first else first.copy(firstIndent = 0f))
    }

  private def setLine(
    runs: List[RichTextRun],
    placement: Placement
  ): Either[PaginationError, Vector[Vector[PlacedRun]]] =
    val live = runs.filter(_.text.nonEmpty).toVector
    if live.isEmpty then Right(Vector(Vector.empty)) else setRuns(live, placement)

  private def setRuns(
    live: Vector[RichTextRun],
    placement: Placement
  ): Either[PaginationError, Vector[Vector[PlacedRun]]] =
    live.traverse(run => advances(run.text, font(run.style))).map { measured =>
      val text   = live.map(_.text).mkString
      val widths = LineBreaker.prefixWidths(text, measured.flatten)
      val boundaries =
        if placement.anywhere then LineBreaker.codePointBoundaries(text) else LineBreaker.lineBoundaries(text, locale)
      val available = (t.textWidth - placement.inset).toDouble
      val spans =
        LineBreaker.breakLines(text, widths, boundaries, available - placement.firstIndent, available)
      val starts = live.scanLeft(0)(_ + _.text.length)
      spans.zipWithIndex.map { (span, index) =>
        placeLine(live.zip(starts), text, widths, span, placement, first = index == 0)
      }
    }

  private def placeLine(
    runs: Vector[(RichTextRun, Int)],
    text: String,
    widths: Vector[Double],
    span: LineBreaker.Span,
    placement: Placement,
    first: Boolean
  ): Vector[PlacedRun] =
    val visible = LineBreaker.visibleWidth(text, widths, span)
    val origin: Double =
      if placement.centred then t.margins.left + (t.textWidth - visible) / 2.0
      else (t.margins.left + placement.inset + (if first then placement.firstIndent else 0f)).toDouble
    runs.flatMap { (run, runStart) =>
      val from = span.start.max(runStart)
      val to   = span.end.min(runStart + run.text.length)
      Option.when(from < to)(
        PlacedRun(
          text.substring(from, to),
          font(run.style),
          (origin + widths(from) - widths(span.start)).toFloat,
          run.style
        )
      )
    }

  private def advances(text: String, face: FontSpec): Either[PaginationError, Vector[Float]] =
    measure.advances(text, face).map(_.toVector).leftMap(PaginationError.Measure(_))
