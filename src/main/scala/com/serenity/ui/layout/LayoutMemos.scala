package com.serenity.ui.layout

import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicLong

import com.serenity.state.manager.ScrollCountExecutor

/** The font-keyed measurements layout and painting share, kept by whoever owns the wraps they were made for: the ascent
  * and descent of a font, the width and fit of a drop cap glyph, and the break iterators text is wrapped with. Each is
  * a pure function of its key and of the owner's one measurer, so an owner only ever pays to measure a font once, and
  * two owners share nothing.
  */
final private[serenity] class LayoutMemos(
    certificateBuilder: Executor = ScrollCountExecutor.shared,
    measurerSource: () => Option[TextMeasurer] = () => None
):

  /** What layout measures with for the owner of these memos, asked for only when something measures. None is the cell
    * grid.
    */
  def measurer: Option[TextMeasurer] = measurerSource()

  /** What a layout that `measured` measures with: None for the cell grid, which a layout with no measurer always is. */
  private[layout] def measuring(measured: Boolean): Option[MeasuredBy] =
    if measured then measurer.map(MeasuredBy(_, this)) else None

  private val exactCount = AtomicLong(0L)

  /** How many rows were measured by shaping them whole, because their glyph advances could not answer. */
  def exactMeasurements: Long = exactCount.get()

  private[layout] def countExactMeasurement(): Unit =
    val _ = exactCount.incrementAndGet()

  /** Nothing measures a line's extents without a measurer; the cell unit stands for them if anything did. */
  lazy val lineExtents: FontLineExtents =
    FontLineExtents(font => measurer.fold(CellMetrics.cellUnit)(_.cellMetrics(font)))

  val boundaries: BoundaryScratch = BoundaryScratch()

  lazy val dropCapWidths: DropCapWidths =
    DropCapWidths((font, text) => measurer.fold(0.0f)(_.glyphWidthPx(font, text)))

  private[serenity] val rowCertificates: RowCertificates = RowCertificates(certificateBuilder, () => measurer)

  private[layout] lazy val dropCapFits: MeasurementMemo[DropCapGlyph.FitKey, Option[DropCapGlyph]] =
    MeasurementMemo[DropCapGlyph.FitKey, Option[DropCapGlyph]](maxEntries = 256)(key =>
      measurer.map(DropCapGlyph.measure(key, _, lineExtents, dropCapWidths))
    )

/** The measurer a measured layout lays text out with, and the memos of the owner it measures for. */
final private[layout] case class MeasuredBy(measurer: TextMeasurer, memos: LayoutMemos)
