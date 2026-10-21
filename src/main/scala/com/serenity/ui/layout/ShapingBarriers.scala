package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

import com.serenity.ui.layout.TextCaretMeasurement.singleFontResolver
import com.serenity.ui.renderer.FontProbes.BarrierProbe
import com.serenity.ui.renderer.ShapedTextCache

/** Where a run of plain text can be cut without changing how either side is shaped. A font with kerning or ligatures
  * shapes a whole paragraph as one run, so the glyph a character gets may depend on its neighbours; cutting between a
  * space and the character after it is harmless exactly when the font has no pair or ligature across that boundary.
  * Whether it has is read off the font by shaping the two characters alone, once per font and character.
  *
  * Fonts differ here: DejaVu Sans never kerns against a space, Liberation Serif does ahead of A, T, V, W and Y.
  */
private[layout] object ShapingBarriers:

  /** The smallest cut at or after `from` that is a barrier, or `text.length` when there is none. */
  def cutAtOrAfter(text: String, from: Int, font: Font, frc: FontRenderContext, shapedText: ShapedTextCache): Int =
    @annotation.tailrec
    def search(cut: Int): Int =
      if cut >= text.length then text.length
      else if isCut(text, cut, font, frc, shapedText) then cut
      else search(cut + 1)
    search(math.max(1, from))

  /** The largest cut at or before `limit` that is a barrier, or 0 (the start of the text) when there is none. */
  def cutAtOrBefore(text: String, limit: Int, font: Font, frc: FontRenderContext, shapedText: ShapedTextCache): Int =
    @annotation.tailrec
    def search(cut: Int): Int =
      if cut <= 0 then 0
      else if isCut(text, cut, font, frc, shapedText) then cut
      else search(cut - 1)
    search(math.min(limit, text.length - 1))

  private def isCut(text: String, cut: Int, font: Font, frc: FontRenderContext, shapedText: ShapedTextCache): Boolean =
    text.charAt(cut - 1) == ' ' && cutsCleanly(font, frc, text.charAt(cut), shapedText) &&
      !substitutesAcross(text, cut, font, frc, shapedText)

  private val ContextChars = 8

  private val AdvanceTolerancePx = 0.01f

  /** A font whose own tables substitute glyphs, with no ligatures or kerning requested, can do so by rules that reach
    * past the space and the one character `cutsCleanly` probes, so the cut is checked in the text around it.
    */
  private def substitutesAcross(
    text: String,
    cut: Int,
    font: Font,
    frc: FontRenderContext,
    shapedText: ShapedTextCache
  ): Boolean =
    !font.hasLayoutAttributes && GlyphAdvances.shapingMaySubstitute(font, frc, shapedText.probes) && {
      val from                           = math.max(0, cut - ContextChars)
      val until                          = math.min(text.length, cut + ContextChars)
      val resolver                       = singleFontResolver(font)
      def measured(start: Int, end: Int) = GlyphAdvances.measure(text, start, end, 0, resolver, frc, shapedText)
      val whole                          = measured(from, until)
      val left                           = measured(from, cut)
      val right                          = measured(cut, until)
      !(from until until).forall { index =>
        val (part, at) = if index < cut then (left, index - from) else (right, index - cut)
        whole.isContextFree(index - from, index - from + 1) == part.isContextFree(at, at + 1) &&
        math.abs(whole.caretsFrom(index - from, 1)(1) - part.caretsFrom(at, 1)(1)) <= AdvanceTolerancePx
      }
    }

  private[layout] def cutsCleanly(
    font: Font,
    frc: FontRenderContext,
    next: Char,
    shapedText: ShapedTextCache
  ): Boolean =
    val probes = shapedText.probes
    (!GlyphAdvances
      .shapingMaySubstitute(font, frc, probes) && GlyphAdvances.layoutAdvanceMatchesSum(font, frc, probes)) ||
    probes.barrier(BarrierProbe(font, frc, next)) { probe =>
      GlyphAdvances
        .measure(s" ${probe.next}", 0, 2, 0, singleFontResolver(probe.font), probe.frc, shapedText)
        .isContextFree(0, 2)
    }
