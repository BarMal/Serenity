package com.serenity.ui.renderer

import java.awt.geom.Path2D

import com.serenity.ui.layout.RoundedCorners

/** The outline of a rectangle with some corners rounded, as a Java2D path: a quarter circle (as a cubic) at each
  * rounded corner and a square corner at the others.
  */
object RoundedPath:

  private val Kappa = 0.5522847498

  def of(x: Double, y: Double, width: Double, height: Double, radius: Double, corners: RoundedCorners): Path2D.Double =
    val k                           = Kappa * radius
    val path                        = new Path2D.Double()
    def r(rounded: Boolean): Double = if rounded then radius else 0.0
    path.moveTo(x + r(corners.topLeft), y)
    path.lineTo(x + width - r(corners.topRight), y)
    if corners.topRight then path.curveTo(x + width - radius + k, y, x + width, y + radius - k, x + width, y + radius)
    path.lineTo(x + width, y + height - r(corners.bottomRight))
    if corners.bottomRight then
      path.curveTo(
        x + width,
        y + height - radius + k,
        x + width - radius + k,
        y + height,
        x + width - radius,
        y + height
      )
    path.lineTo(x + r(corners.bottomLeft), y + height)
    if corners.bottomLeft then
      path.curveTo(x + radius - k, y + height, x, y + height - radius + k, x, y + height - radius)
    path.lineTo(x, y + r(corners.topLeft))
    if corners.topLeft then path.curveTo(x, y + radius - k, x + radius - k, y, x + radius, y)
    path.closePath()
    path
