package com.serenity.keystroke.translators

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.events.*
import com.serenity.keystroke.KeyStrokeInfo

object TextNavigationConverters:

  val navigationConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(KeyType.ArrowLeft, _, _)  => MoveLeft
    case KeyStrokeInfo(KeyType.ArrowRight, _, _) => MoveRight
    case KeyStrokeInfo(KeyType.ArrowUp, _, _)    => MoveUp
    case KeyStrokeInfo(KeyType.ArrowDown, _, _)  => MoveDown
    case KeyStrokeInfo(KeyType.Home, _, _)       => MoveToStart
    case KeyStrokeInfo(KeyType.End, _, _)        => MoveToEnd
  }
