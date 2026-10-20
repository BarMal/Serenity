package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.text.fallback.FontSlot
import com.serenity.ui.fonts.FontFallback
import com.serenity.ui.layout.WrappedLineCache

/** What the GUI cell grid needs to draw text its primary font has no glyph for: the primary, the chain behind it and
  * the cache that itemises (and remembers) a string's runs, so a frame does not itemise every line again. A terminal
  * has no fonts, so it passes none.
  */
final private[renderer] case class CellFallback(primary: Font, chain: FontFallback, cache: WrappedLineCache):

  /** `text` as `(from, until, family)` stretches, `family` being the fallback family that draws the stretch, empty when
    * the primary draws it. Empty when the primary draws all of `text`. Neutrals the primary covers stay on it, as every
    * one of them belongs in its own cell on a grid.
    */
  def stretchesOf(text: String): Vector[(Int, Int, Option[String])] =
    if text.isEmpty || cache.isPlainAscii(text) then Vector.empty
    else
      val runs = cache.fallbackRuns(primary, chain, text, stickyNeutrals = false)
      if runs.forall(_.slot == FontSlot.Primary) then Vector.empty
      else
        runs.map(run =>
          (run.from, run.until, Option.when(run.slot != FontSlot.Primary)(chain.slotFamilies(run.slot.index - 1)))
        )
