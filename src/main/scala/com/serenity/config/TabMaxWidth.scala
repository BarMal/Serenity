package com.serenity.config

/** The widest a tab in the tab strip may be, in cells. */
object TabMaxWidth:

  val Default: Int = 28
  val Min: Int     = 8
  val Max: Int     = 60

  def clamp(width: Int): Int = width.max(Min).min(Max)
