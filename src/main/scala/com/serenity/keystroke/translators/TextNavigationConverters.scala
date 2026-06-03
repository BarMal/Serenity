package com.serenity.keystroke.translators

import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}

object TextNavigationConverters:

  val navigationConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(InputKey.ArrowLeft, _, _)  => MoveLeft
    case KeyStrokeInfo(InputKey.ArrowRight, _, _) => MoveRight
    case KeyStrokeInfo(InputKey.ArrowUp, _, _)    => MoveUp
    case KeyStrokeInfo(InputKey.ArrowDown, _, _)  => MoveDown
    case KeyStrokeInfo(InputKey.Home, _, _)       => MoveToStart
    case KeyStrokeInfo(InputKey.End, _, _)        => MoveToEnd
  }
