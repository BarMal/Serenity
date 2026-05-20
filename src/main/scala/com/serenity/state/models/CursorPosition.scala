package com.serenity.state.models

case class CursorPosition(line: Int, column: Int):
  def moveRight: CursorPosition = copy(column = column + 1)
  def moveLeft: CursorPosition  = copy(column = Math.max(0, column - 1))
  def moveDown: CursorPosition  = copy(line = line + 1)
  def moveUp: CursorPosition    = copy(line = Math.max(0, line - 1))
