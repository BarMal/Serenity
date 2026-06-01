package com.serenity.keystroke.translators

import com.serenity.keystroke.events.Direction
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}

object DirectionalKeyConverter:

  def arrowKeys[E](wrap: Direction => E): PartialFunction[KeyStrokeInfo, E] = {
    case KeyStrokeInfo(InputKey.ArrowUp, _, _)    => wrap(Direction.Up)
    case KeyStrokeInfo(InputKey.ArrowDown, _, _)  => wrap(Direction.Down)
    case KeyStrokeInfo(InputKey.ArrowLeft, _, _)  => wrap(Direction.Left)
    case KeyStrokeInfo(InputKey.ArrowRight, _, _) => wrap(Direction.Right)
  }
