package com.serenity.animation

import java.awt.Color

object RgbInterpolator:

  def interpolateRgba(startColor: Color, endColor: Color, steps: Int): List[Color] =
    if steps <= 0 then List.empty
    else if steps == 1 then List(endColor)
    else if startColor == endColor then List.fill(steps)(startColor)
    else
      val stepSize = 1.0 / (steps - 1)
      (0 until steps).map { step =>
        if step == 0 then startColor
        else if step == steps - 1 then endColor
        else
          val t = step * stepSize
          val r = interpolateComponent(startColor.getRed, endColor.getRed, t)
          val g = interpolateComponent(startColor.getGreen, endColor.getGreen, t)
          val b = interpolateComponent(startColor.getBlue, endColor.getBlue, t)
          val a = interpolateComponent(startColor.getAlpha, endColor.getAlpha, t)
          new Color(r, g, b, a)
      }.toList

  def interpolateRgbaAt(startColor: Color, endColor: Color, steps: Int, step: Int): Option[Color] =
    if steps <= 0 || step < 0 || step >= steps then None
    else if steps == 1 then Some(endColor)
    else if startColor == endColor then Some(startColor)
    else if step == 0 then Some(startColor)
    else if step == steps - 1 then Some(endColor)
    else
      val t = step.toDouble / (steps - 1).toDouble
      Some(
        new Color(
          interpolateComponent(startColor.getRed, endColor.getRed, t),
          interpolateComponent(startColor.getGreen, endColor.getGreen, t),
          interpolateComponent(startColor.getBlue, endColor.getBlue, t),
          interpolateComponent(startColor.getAlpha, endColor.getAlpha, t)
        )
      )

  private def interpolateComponent(start: Int, end: Int, t: Double): Int =
    math.round(start + (end - start) * t).toInt.max(0).min(255)
