package com.serenity.ui.layout

/** The font-keyed measurements layout and painting share, kept by whoever owns the wraps they were made for: the ascent
  * and descent of a font, the width and fit of a drop cap glyph, and the break iterators text is wrapped with. Each is
  * a pure function of its key, so an owner only ever pays to measure a font once, and two owners share nothing.
  */
final private[serenity] class LayoutMemos:

  val lineExtents: FontLineExtents = FontLineExtents()

  val boundaries: BoundaryScratch = BoundaryScratch()

  val dropCapWidths: DropCapWidths = DropCapWidths()

  private[layout] val dropCapFits: MeasurementMemo[DropCapGlyph.FitKey, DropCapGlyph] =
    MeasurementMemo[DropCapGlyph.FitKey, DropCapGlyph](maxEntries = 256)(
      DropCapGlyph.measure(_, lineExtents, dropCapWidths)
    )
