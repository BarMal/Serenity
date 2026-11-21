package com.serenity.ui.renderer

/** Where a label's ink lands in device pixels: its origin on the pixel grid plus the sixteenth of a pixel it starts
  * past it, and the integral device scale it was drawn at.
  */
final private[ui] case class LabelAnchor(
    deviceX: Int,
    deviceBaseline: Int,
    phaseSixteenths: Int,
    scaleX: Int,
    scaleY: Int
)

private[ui] object LabelAnchor:

  def isWhole(value: Double): Boolean = math.abs(value - math.rint(value)) < 1e-6
