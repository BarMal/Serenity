package com.serenity.ui.layout

enum SurfaceLayoutKind:
  case Horizontal
  case Vertical
  case Square
  case Compact

object SurfaceLayoutKind:

  def classify(rect: LayoutRect): SurfaceLayoutKind =
    val width  = math.max(1, rect.width)
    val height = math.max(1, rect.height)

    if width <= 16 || height <= 4 then Compact
    else if width >= height * 2 then Horizontal
    else if height >= width * 2 then Vertical
    else Square
