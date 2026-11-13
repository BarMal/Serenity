package com.serenity.app

import com.serenity.app.RendererChoice.Candidate

/** Which modes `serenity --optimize-renderer` tries, and in what order (#2618).
  *
  * Every mode runs twice, once on the way out and once on the way back, so a machine that warms up or throttles during
  * the run favours none of them, and each mode has two runs to take a median and a spread from.
  */
object RendererRunOrder:

  private val MinutesPerTrial = 2

  def candidates(thorough: Boolean): List[Candidate] =
    List(Candidate.LayerImage, Candidate.SkiaRaster) ++ Option.when(thorough)(Candidate.LayerPicture)

  def order(thorough: Boolean): List[Candidate] =
    val modes = candidates(thorough)
    modes ++ modes.reverse

  /** A window start, a warm-up and the workload take about two minutes. */
  def minutes(thorough: Boolean): Int = order(thorough).size * MinutesPerTrial
