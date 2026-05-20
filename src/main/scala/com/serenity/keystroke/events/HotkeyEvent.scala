package com.serenity.keystroke.events

sealed trait HotkeyEvent extends TextEntryEvent

case object Save  extends HotkeyEvent
case object Quit  extends HotkeyEvent
case object Undo  extends HotkeyEvent
case object Redo  extends HotkeyEvent
case object Copy  extends HotkeyEvent
case object Paste extends HotkeyEvent
case object Cut   extends HotkeyEvent
case object ToggleSyntaxHighlighting extends HotkeyEvent
