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

  enum Corner:
    case TopLeft, TopRight, BottomRight, BottomLeft

  /** The part of the `radius`-sized square at (`x`, `y`) that lies inside the rounded rectangle whose corner it is. */
  def cornerFill(corner: Corner, x: Int, y: Int, radius: Int): Path2D.Double =
    val r    = radius.toDouble
    val k    = Kappa * r
    val left = x.toDouble
    val top  = y.toDouble
    val path = new Path2D.Double()
    corner match
      case Corner.TopLeft =>
        path.moveTo(left, top + r)
        path.curveTo(left, top + r - k, left + r - k, top, left + r, top)
        path.lineTo(left + r, top + r)
      case Corner.TopRight =>
        path.moveTo(left, top)
        path.curveTo(left + k, top, left + r, top + r - k, left + r, top + r)
        path.lineTo(left, top + r)
      case Corner.BottomRight =>
        path.moveTo(left, top)
        path.lineTo(left + r, top)
        path.curveTo(left + r, top + k, left + k, top + r, left, top + r)
      case Corner.BottomLeft =>
        path.moveTo(left, top)
        path.lineTo(left + r, top)
        path.lineTo(left + r, top + r)
        path.curveTo(left + r - k, top + r, left, top + k, left, top)
    path.closePath()
    path

  /** The centre line of a border `thickness` wide round the same corner: a quarter arc from one straight edge to the
    * other, ending exactly where the edges' rectangles begin.
    */
  def cornerArc(corner: Corner, x: Int, y: Int, radius: Int, thickness: Int): Path2D.Double =
    val half = thickness / 2.0
    val r    = radius - half
    val k    = Kappa * r
    val left = x.toDouble
    val top  = y.toDouble
    val full = radius.toDouble
    val path = new Path2D.Double()
    corner match
      case Corner.TopLeft =>
        path.moveTo(left + half, top + full)
        path.curveTo(left + half, top + full - k, left + full - k, top + half, left + full, top + half)
      case Corner.TopRight =>
        path.moveTo(left, top + half)
        path.curveTo(left + k, top + half, left + full - half, top + full - k, left + full - half, top + full)
      case Corner.BottomRight =>
        path.moveTo(left + full - half, top)
        path.curveTo(left + full - half, top + k, left + k, top + full - half, left, top + full - half)
      case Corner.BottomLeft =>
        path.moveTo(left + full, top + full - half)
        path.curveTo(left + full - k, top + full - half, left + half, top + k, left + half, top)
    path
