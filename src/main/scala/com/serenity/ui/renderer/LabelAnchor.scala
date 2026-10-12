package com.serenity.ui.renderer

import java.awt.geom.AffineTransform

/** Where a label's ink lands in device pixels: its origin on the pixel grid plus the sixteenth of a pixel it starts
  * past it, and the integral device scale it was drawn at.
  */
final private[serenity] case class LabelAnchor(
    deviceX: Int,
    deviceBaseline: Int,
    phaseSixteenths: Int,
    scaleX: Int,
    scaleY: Int
)

private[serenity] object LabelAnchor:

  /** Only an integral device scale puts a label's baseline on a whole device pixel, which a sprite needs: it is blitted
    * 1:1, never resampled.
    */
  def of(transform: AffineTransform, xPx: Float, baselinePx: Int): Option[LabelAnchor] =
    val deviceBaseline = transform.getTranslateY + transform.getScaleY * baselinePx
    val axisAligned    = transform.getShearX == 0.0 && transform.getShearY == 0.0
    Option.when(
      axisAligned && isWhole(transform.getScaleX) && isWhole(transform.getScaleY) && transform.getScaleX >= 1.0 &&
        transform.getScaleY >= 1.0 && isWhole(deviceBaseline)
    ) {
      val sixteenths = math.round((transform.getTranslateX + transform.getScaleX * xPx) * 16.0)
      LabelAnchor(
        Math.floorDiv(sixteenths, 16L).toInt,
        math.rint(deviceBaseline).toInt,
        Math.floorMod(sixteenths, 16L).toInt,
        transform.getScaleX.toInt,
        transform.getScaleY.toInt
      )
    }

  def isWhole(value: Double): Boolean = math.abs(value - math.rint(value)) < 1e-6
