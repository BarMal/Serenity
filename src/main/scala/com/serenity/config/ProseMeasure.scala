package com.serenity.config

import java.util.Locale

/** The prose text column's width in characters (`66ch`, as CSS spells it), so line length holds steady as the window
  * changes size rather than tracking it the way percentage insets do (#1880).
  */
object ProseMeasure:

  val Default: Int = 66
  val Min: Int     = 20
  val Max: Int     = 200

  /** `66ch` or a bare `66`; a count outside [[Min]]..[[Max]] is refused rather than clamped. */
  def parse(text: String): Option[Int] =
    text.trim
      .toLowerCase(Locale.ROOT)
      .stripSuffix("ch")
      .trim
      .toIntOption
      .filter(chars => chars >= Min && chars <= Max)

  def render(chars: Int): String = s"${chars}ch"

  def clamp(chars: Int): Int = chars.max(Min).min(Max)
