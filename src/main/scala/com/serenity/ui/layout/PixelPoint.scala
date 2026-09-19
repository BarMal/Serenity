package com.serenity.ui.layout

/** A single pixel position -- caret glide's (issue #1085 phase 2) `Tween[PixelPoint]` endpoint, distinct from
  * [[PixelRect]] because a glide animates a single point, not a rectangle.
  */
final case class PixelPoint(xPx: Int, yPx: Int)
