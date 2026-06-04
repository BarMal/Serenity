package com.serenity.keystroke.translators

import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

object TextNavigationConverters:

  val navigationConverter: PartialFunction[KeyStrokeInfo, TextEntryEvent] = {
    case KeyStrokeInfo(InputKey.Home, _, modifiers) if modifiers.contains(Modifier.Ctrl) => MoveToStartOfFile
    case KeyStrokeInfo(InputKey.End, _, modifiers) if modifiers.contains(Modifier.Ctrl)  => MoveToEndOfFile
    case KeyStrokeInfo(InputKey.ArrowLeft, _, _)  => MoveLeft
    case KeyStrokeInfo(InputKey.ArrowRight, _, _) => MoveRight
    case KeyStrokeInfo(InputKey.ArrowUp, _, _)    => MoveUp
    case KeyStrokeInfo(InputKey.ArrowDown, _, _)  => MoveDown
    case KeyStrokeInfo(InputKey.PageUp, _, _)     => PageUp
    case KeyStrokeInfo(InputKey.PageDown, _, _)   => PageDown
    case KeyStrokeInfo(InputKey.Home, _, _)       => MoveToStart
    case KeyStrokeInfo(InputKey.End, _, _)        => MoveToEnd
  }
