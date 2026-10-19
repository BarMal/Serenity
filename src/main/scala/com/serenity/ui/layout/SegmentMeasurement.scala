package com.serenity.ui.layout

import java.awt.font.FontRenderContext

import com.serenity.state.models.MeasuredRow
import com.serenity.ui.layout.TextCaretMeasurement.{LineFontResolver, caretXs, landableStops}

/** One wrapped row's geometry, measured from its text and the resolver that gives its columns their fonts. */
private[layout] object SegmentMeasurement:

  def measureSegment(
    text: String,
    startColumn: Int,
    endColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    measuredXs: Option[IArray[Float]] = None,
    graphemeOffsets: Option[IArray[Int]] = None
  ): MeasuredRow =
    val xs      = measuredXs.getOrElse(caretXs(text, startColumn, resolver, frc, measuredLayout, cellMetrics))
    val offsets = graphemeOffsets.getOrElse(ParagraphMeasurement.graphemeBoundaryOffsets(text))
    val stopXs  = IArray.tabulate(offsets.length)(index => xs(math.min(offsets(index), xs.length - 1)))
    val (landingXs, landingOffsets) =
      if resolver.hasHidden then landableStops(stopXs, offsets, startColumn, resolver)
      else MeasuredRow.orderedByX(stopXs, offsets)
    val (heightPx, ascentPx) =
      if measuredLayout then resolver.lineMetrics(frc, startColumn, endColumn) else (0, 0)
    val fontRuns = RowFontRuns.of(resolver, text, startColumn, endColumn)
    MeasuredRow(
      startColumn = startColumn,
      endColumn = endColumn,
      text = text,
      widthPx = xs(xs.length - 1),
      caretXs = stopXs,
      caretOffsets = offsets,
      landingXs = landingXs,
      landingOffsets = landingOffsets,
      heightPx = heightPx,
      ascentPx = ascentPx,
      fontRuns = fontRuns,
      fallbackFamilies = if fontRuns.isEmpty then Vector.empty else resolver.fallbackFamilies
    )
