package com.serenity.keystroke.events

trait TextEntryEvent extends Event

case class InsertChar(char: Char) extends TextEntryEvent
case object DeleteBackward        extends TextEntryEvent
case object DeleteForward         extends TextEntryEvent
case object MoveLeft              extends TextEntryEvent
case object MoveRight             extends TextEntryEvent
case object MoveUp                extends TextEntryEvent
case object MoveDown              extends TextEntryEvent
case object MoveToStart           extends TextEntryEvent
case object MoveToEnd             extends TextEntryEvent
case object NewLine               extends TextEntryEvent
case object PageDown              extends TextEntryEvent
case object PageUp                extends TextEntryEvent
case object MoveToEndOfFile       extends TextEntryEvent
case object MoveToStartOfFile     extends TextEntryEvent
case class ScrollDown(lines: Int) extends TextEntryEvent
case class ScrollUp(lines: Int)   extends TextEntryEvent
case object OpenGotoLine          extends TextEntryEvent
case object FindNext              extends TextEntryEvent
case object Enter                 extends TextEntryEvent
case object OpenFind              extends TextEntryEvent
