package com.serenity.ui.renderer

/** What a shaped-text cache has done since it was made, for a surface to attribute to the frame that caused it. */
trait ShapeTraffic:

  /** Lookups no generation answered, so the text was shaped. */
  def shapings: Long

  /** Lookups the older generation answered, so a stretch shaped earlier was kept through one rotation. */
  def promotions: Long
