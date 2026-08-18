package com.serenity.animation

import java.awt.Color

enum FlowDirection:
  case ByColumn, ByRow

enum SweepDirection:
  case Forward, Backward

final case class CellAnimation(char: Char, startColor: Color, endColor: Color)

object FlowAnimationBuilder:

  def build(
    cells: Map[CharacterKey, CellAnimation],
    direction: FlowDirection,
    sweep: SweepDirection,
    steps: Int,
    staggerFrames: Int = 1,
    delayFrames: Int = 0
  ): Map[CharacterKey, AnimatedCell] =
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
          case (FlowDirection.ByRow, SweepDirection.Forward)     => key.line - minRow
          case (FlowDirection.ByRow, SweepDirection.Backward)    => maxRow - key.line

        key -> AnimatedCell.parametricForeground(
          char = cell.char,
          startColor = cell.startColor,
          endColor = cell.endColor,
          steps = steps,
          delayFrames = delayFrames.max(0) + offset * staggerFrames.max(0)
        )
      }
