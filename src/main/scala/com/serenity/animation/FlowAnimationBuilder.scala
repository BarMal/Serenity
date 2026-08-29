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
      // cells is non-empty here (guarded by the isEmpty check above), so these folds always see
      // at least one element and the Int.MaxValue/MinValue seeds are never the reported result.
      val minCol = cells.keys.map(_.column).foldLeft(Int.MaxValue)(_ min _)
      val maxCol = cells.keys.map(_.column).foldLeft(Int.MinValue)(_ max _)
      val minRow = cells.keys.map(_.line).foldLeft(Int.MaxValue)(_ min _)
      val maxRow = cells.keys.map(_.line).foldLeft(Int.MinValue)(_ max _)

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
