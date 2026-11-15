package com.serenity.ui.layout

import java.util.concurrent.Executor

import com.serenity.state.manager.ScrollCountExecutor

/** The font-keyed measurements layout and painting share, kept by whoever owns the wraps they were made for: the ascent
  * and descent of a font, the width and fit of a drop cap glyph, and the break iterators text is wrapped with. Each is
  * a pure function of its key, so an owner only ever pays to measure a font once, and two owners share nothing.
  */
final private[serenity] class LayoutMemos(
    certificateBuilder: Executor = ScrollCountExecutor.shared,
    measurerSource: () => Option[TextMeasurer] = () => None
):

  /** What layout measures with for the owner of these memos, asked for only when something measures. None is the cell
    * grid.
    */
  def measurer: Option[TextMeasurer] = measurerSource()

  val lineExtents: FontLineExtents = FontLineExtents()

  val boundaries: BoundaryScratch = BoundaryScratch()

  val dropCapWidths: DropCapWidths = DropCapWidths()

  private[serenity] val rowCertificates: RowCertificates = RowCertificates(certificateBuilder)

  private[layout] val dropCapFits: MeasurementMemo[DropCapGlyph.FitKey, DropCapGlyph] =
    MeasurementMemo[DropCapGlyph.FitKey, DropCapGlyph](maxEntries = 256)(
      DropCapGlyph.measure(_, lineExtents, dropCapWidths)
    )
