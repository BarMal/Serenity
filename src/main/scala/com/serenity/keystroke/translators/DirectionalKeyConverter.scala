package com.serenity.keystroke.translators

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Direction

object DirectionalKeyConverter:

  def arrowKeys[E](wrap: Direction => E): PartialFunction[KeyStrokeInfo, E] = {
    case KeyStrokeInfo(KeyType.ArrowUp, _, _)    => wrap(Direction.Up)
    case KeyStrokeInfo(KeyType.ArrowDown, _, _)  => wrap(Direction.Down)
    case KeyStrokeInfo(KeyType.ArrowLeft, _, _)  => wrap(Direction.Left)
    case KeyStrokeInfo(KeyType.ArrowRight, _, _) => wrap(Direction.Right)
  }
