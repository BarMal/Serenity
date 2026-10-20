package com.serenity.ui.theme

/** The room between the ink of a drop cap's glyph and the text beside it, in em of the body font
  * (`document.drop_cap_gap`).
  */
object DropCapGap:

  val Default: Float = 0.25f

  val Range: (Float, Float) = (0.0f, 2.0f)

  def clamped(gapEm: Float): Float = gapEm.max(Range._1).min(Range._2)
