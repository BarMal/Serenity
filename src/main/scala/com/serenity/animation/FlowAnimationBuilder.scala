package com.serenity.animation

import com.googlecode.lanterna.TextColor

enum FlowDirection:
  case ByColumn, ByRow

enum SweepDirection:
  case Forward, Backward

case class CellAnimation(char: Char, startColor: TextColor, endColor: TextColor)

object FlowAnimationBuilder:

  def build(
    cells: Map[CharacterKey, CellAnimation],
    direction: FlowDirection,
    sweep: SweepDirection,
    steps: Int
  ): Map[CharacterKey, AnimatedCharacter] =
    if cells.isEmpty then Map.empty
    else
      val minCol = cells.keys.map(_.column).min
      val maxCol = cells.keys.map(_.column).max
      val minRow = cells.keys.map(_.line).min
      val maxRow = cells.keys.map(_.line).max

      cells.map { (key, cell) =>
        val offset = (direction, sweep) match
          case (FlowDirection.ByColumn, SweepDirection.Forward)  => key.column - minCol
          case (FlowDirection.ByColumn, SweepDirection.Backward) => maxCol - key.column
          case (FlowDirection.ByRow,    SweepDirection.Forward)  => key.line - minRow
          case (FlowDirection.ByRow,    SweepDirection.Backward) => maxRow - key.line

        val fade    = RgbInterpolator.interpolate(cell.startColor, cell.endColor, steps)
        val padding = List.fill(offset)(cell.startColor)
        key -> AnimatedCharacter(cell.char, padding ++ fade)
      }
