package com.serenity.ui.layout

import com.serenity.ui.renderer.FontSpec

/** Where a run of plain text can be cut without changing how either side is shaped. A font with kerning or ligatures
  * shapes a whole paragraph as one run, so the glyph a character gets may depend on its neighbours; cutting between a
  * space and the character after it is harmless exactly when the font has no pair, ligature or contextual rule across
  * that boundary. The measurer decides that by shaping the text around the cut whole and in two.
  *
  * Fonts differ here: DejaVu Sans never kerns against a space, Liberation Serif does ahead of A, T, V, W and Y.
  */
private[layout] object ShapingBarriers:

  /** The smallest cut at or after `from` that is a barrier, or `text.length` when there is none. */
  def cutAtOrAfter(text: String, from: Int, font: FontSpec, measurer: TextMeasurer): Int =
    @annotation.tailrec
    def search(cut: Int): Int =
      if cut >= text.length then text.length
      else if isCut(text, cut, font, measurer) then cut
      else search(cut + 1)
    search(math.max(1, from))

  /** The largest cut at or before `limit` that is a barrier, or 0 (the start of the text) when there is none. */
  def cutAtOrBefore(text: String, limit: Int, font: FontSpec, measurer: TextMeasurer): Int =
    @annotation.tailrec
    def search(cut: Int): Int =
      if cut <= 0 then 0
      else if isCut(text, cut, font, measurer) then cut
      else search(cut - 1)
    search(math.min(limit, text.length - 1))

  /** Shaping rules reach a few characters either way, so the text this far around a cut decides it. */
  private val ContextChars = 8

  private def isCut(text: String, cut: Int, font: FontSpec, measurer: TextMeasurer): Boolean =
    text.charAt(cut - 1) == ' ' && {
      val from = math.max(0, cut - ContextChars)
      val near = text.substring(from, math.min(text.length, cut + ContextChars))
      measurer.isShapingCut(near, cut - from, font)
    }
