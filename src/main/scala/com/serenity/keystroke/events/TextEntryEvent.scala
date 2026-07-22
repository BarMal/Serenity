package com.serenity.keystroke.events

trait TextEntryEvent extends EditorEvent

trait TextInputEvent extends TextEntryEvent

trait DeletionEvent extends TextEntryEvent

trait NavigationEvent extends TextEntryEvent

trait ScrollEvent extends TextEntryEvent

trait ModalRequestEvent extends TextEntryEvent

case class InsertChar(char: Char) extends TextInputEvent
case object DeleteBackward        extends DeletionEvent
case object DeleteForward         extends DeletionEvent
case object DeleteWordBackward    extends DeletionEvent
case object DeleteWordForward     extends DeletionEvent
case object MoveLeft              extends NavigationEvent
case object MoveRight             extends NavigationEvent
case object MoveWordLeft          extends NavigationEvent
case object MoveWordRight         extends NavigationEvent
case object MoveUp                extends NavigationEvent
case object MoveDown              extends NavigationEvent
case object ExtendSelectionLeft   extends NavigationEvent
case object ExtendSelectionRight  extends NavigationEvent
case object ExtendSelectionUp     extends NavigationEvent
case object ExtendSelectionDown   extends NavigationEvent
case object MoveToStart           extends NavigationEvent
case object MoveToEnd             extends NavigationEvent
case object SelectAll             extends TextEntryEvent
case object NewLine               extends TextEntryEvent
case object PageDown              extends NavigationEvent
case object PageUp                extends NavigationEvent
case object MoveToEndOfFile       extends NavigationEvent
case object MoveToStartOfFile     extends NavigationEvent
case class ScrollDown(lines: Int) extends ScrollEvent
case class ScrollUp(lines: Int)   extends ScrollEvent
case object OpenGotoLine          extends ModalRequestEvent
case object OpenReplace           extends ModalRequestEvent
case object FindNext              extends TextEntryEvent
case object Enter                 extends TextEntryEvent
case object OpenFind              extends ModalRequestEvent
case object Escape                extends TextEntryEvent
case object TabKey                extends TextEntryEvent
case object ReverseTabKey         extends TextEntryEvent
