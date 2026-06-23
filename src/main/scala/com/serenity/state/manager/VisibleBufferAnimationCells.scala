package com.serenity.state.manager

import java.awt.Color

import com.serenity.animation.{CellAnimation, CharacterKey}
import com.serenity.state.models.Buffer

private[serenity] object VisibleBufferAnimationCells:

  def fromBuffer(
    buffer: Buffer,
    wordWrapEnabled: Boolean,
    startColor: Color,
    endColor: Color
  ): Map[CharacterKey, CellAnimation] =
    val viewport       = buffer.viewport
    val startColumn    = if wordWrapEnabled then 0 else math.max(0, viewport.leftColumn)
    val visibleColumns = math.max(0, viewport.visibleColumns)

    (viewport.topLine until (viewport.topLine + viewport.visibleLines)).flatMap { lineIndex =>
      buffer.content.getLine(lineIndex).toList.flatMap { line =>
        val visibleText = line.slice(startColumn, startColumn + visibleColumns)
        visibleText.zipWithIndex.map { (char, offset) =>
          CharacterKey(startColumn + offset, lineIndex) -> CellAnimation(char, startColor, endColor)
        }
      }
    }.toMap
