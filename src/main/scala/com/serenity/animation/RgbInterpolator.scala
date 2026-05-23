package com.serenity.animation

import com.googlecode.lanterna.TextColor

object RgbInterpolator:

  /** Interpolate between two colors with the specified number of steps */
  def interpolate(startColor: TextColor, endColor: TextColor, steps: Int): List[TextColor] =
    if steps <= 0 then List.empty
    else if steps == 1 then List(endColor)
    else if startColor == endColor then List.fill(steps)(startColor)
    else
      // Convert all colors to RGB for consistent interpolation
      val startRgb = toRgb(startColor)
      val endRgb   = toRgb(endColor)
      interpolateRgb(startRgb, endRgb, steps, startColor, endColor)

  /** Interpolate between RGB colors, preserving original start/end if they weren't RGB */
  private def interpolateRgb(
    startRgb: TextColor.RGB,
    endRgb: TextColor.RGB,
    steps: Int,
    originalStart: TextColor,
    originalEnd: TextColor
  ): List[TextColor] =
    if steps == 2 then List(originalStart, originalEnd)
    else
      val stepSize = 1.0 / (steps - 1)
      (0 until steps).map { step =>
        if step == 0 then originalStart
        else if step == steps - 1 then originalEnd
        else
          val t = step * stepSize
          val r = interpolateComponent(startRgb.getRed, endRgb.getRed, t)
          val g = interpolateComponent(startRgb.getGreen, endRgb.getGreen, t)
          val b = interpolateComponent(startRgb.getBlue, endRgb.getBlue, t)
          new TextColor.RGB(r, g, b)
      }.toList

  /** Interpolate a single color component (R, G, or B) */
  private def interpolateComponent(start: Int, end: Int, t: Double): Int =
    math.round(start + (end - start) * t).toInt.max(0).min(255)

  /** Convert any TextColor to RGB approximation */
  def toRgb(color: TextColor): TextColor.RGB =
    color match
      case rgb: TextColor.RGB => rgb
      case ansi               => ansiToRgb(ansi)

  /** Convert ANSI colors to RGB approximations */
  private def ansiToRgb(color: TextColor): TextColor.RGB =
    color match
      case TextColor.ANSI.BLACK   => new TextColor.RGB(0, 0, 0)
      case TextColor.ANSI.RED     => new TextColor.RGB(128, 0, 0)
      case TextColor.ANSI.GREEN   => new TextColor.RGB(0, 128, 0)
      case TextColor.ANSI.YELLOW  => new TextColor.RGB(128, 128, 0)
      case TextColor.ANSI.BLUE    => new TextColor.RGB(0, 0, 128)
      case TextColor.ANSI.MAGENTA => new TextColor.RGB(128, 0, 128)
      case TextColor.ANSI.CYAN    => new TextColor.RGB(0, 128, 128)
      case TextColor.ANSI.WHITE   => new TextColor.RGB(255, 255, 255)

      case TextColor.ANSI.BLACK_BRIGHT   => new TextColor.RGB(128, 128, 128)
      case TextColor.ANSI.RED_BRIGHT     => new TextColor.RGB(255, 0, 0)
      case TextColor.ANSI.GREEN_BRIGHT   => new TextColor.RGB(0, 255, 0)
      case TextColor.ANSI.YELLOW_BRIGHT  => new TextColor.RGB(255, 255, 0)
      case TextColor.ANSI.BLUE_BRIGHT    => new TextColor.RGB(0, 0, 255)
      case TextColor.ANSI.MAGENTA_BRIGHT => new TextColor.RGB(255, 0, 255)
      case TextColor.ANSI.CYAN_BRIGHT    => new TextColor.RGB(0, 255, 255)
      case TextColor.ANSI.WHITE_BRIGHT   => new TextColor.RGB(255, 255, 255)

      case TextColor.ANSI.DEFAULT => new TextColor.RGB(192, 192, 192) // Default to light gray
      case _                      => new TextColor.RGB(192, 192, 192) // Fallback to light gray
