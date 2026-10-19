package com.serenity.ui.layout

import com.serenity.text.fallback.{FallbackItemiser, FallbackRun, FontSlot}
import com.serenity.ui.fonts.{FontFallback, Java2DGlyphCoverage}
import com.serenity.ui.layout.TextCaretMeasurement.{ColumnFontRun, LineFontResolver}

/** Cuts a line's [[LineFontResolver]] where its own fonts have no glyph, so the missing glyphs are measured, and by the
  * same runs painted, in the fallback fonts: fallback becomes more [[ColumnFontRun]]s, each in the font its column
  * already had, with the fallback family swapped in.
  */
private[layout] object FallbackResolution:

  /** `resolver` with fallback runs for `text`, whose first character is at `firstColumn`. A line the primary font
    * covers throughout returns `resolver` itself, so it keeps its identity and its cache entries.
    */
  def apply(
    resolver: LineFontResolver,
    text: String,
    firstColumn: Int,
    coverage: Java2DGlyphCoverage,
    fallback: FontFallback,
    stickyNeutrals: Boolean
  ): LineFontResolver =
    val itemised = FallbackItemiser.itemise(text, 0, text.length, coverage, stickyNeutrals)
    if itemised.forall(_.slot == FontSlot.Primary) then resolver
    else resolver.copy(runs = cut(resolver, itemised, firstColumn, fallback), fallbackFamilies = fallback.slotFamilies)

  private def cut(
    resolver: LineFontResolver,
    itemised: Vector[FallbackRun],
    firstColumn: Int,
    fallback: FontFallback
  ): Vector[ColumnFontRun] =
    val fallbackRuns = itemised.collect {
      case FallbackRun(from, until, slot) if slot != FontSlot.Primary => (firstColumn + from, firstColumn + until, slot)
    }
    val cuts =
      (resolver.runs.flatMap(run => Vector(run.startColumn, run.endColumn)) ++
        fallbackRuns.flatMap((from, until, _) => Vector(from, until))).distinct.sorted

    def originalAt(column: Int): Option[ColumnFontRun] =
      resolver.runs.find(run => column >= run.startColumn && column < run.endColumn)

    def slotAt(column: Int): FontSlot =
      fallbackRuns
        .collectFirst { case (from, until, slot) if column >= from && column < until => slot }
        .getOrElse(FontSlot.Primary)

    cuts.zip(cuts.drop(1)).flatMap { (from, until) =>
      val original = originalAt(from)
      val slot     = slotAt(from)
      if slot == FontSlot.Primary then original.map(_.copy(startColumn = from, endColumn = until))
      else
        val styled = original.map(_.font).getOrElse(resolver.baseFont)
        Some(ColumnFontRun(from, until, fallback.fontFor(slot, styled), original.exists(_.hidden), slot))
    }
