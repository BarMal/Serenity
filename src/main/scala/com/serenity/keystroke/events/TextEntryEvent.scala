package com.serenity.keystroke.events

sealed trait TextEntryEvent

sealed trait TextInputEvent extends TextEntryEvent

sealed trait DeletionEvent extends TextEntryEvent

sealed trait NavigationEvent extends TextEntryEvent

sealed trait ScrollEvent extends TextEntryEvent

sealed trait ModalRequestEvent extends TextEntryEvent

final case class InsertChar(char: Char) extends TextInputEvent
case object DeleteBackward              extends DeletionEvent
case object DeleteForward               extends DeletionEvent
case object DeleteWordBackward          extends DeletionEvent
case object DeleteWordForward           extends DeletionEvent
case object MoveLeft                    extends NavigationEvent
case object MoveRight                   extends NavigationEvent
case object MoveWordLeft                extends NavigationEvent
case object MoveWordRight               extends NavigationEvent
case object ExtendSelectionLeft         extends NavigationEvent
case object ExtendSelectionRight        extends NavigationEvent
case object MoveToStart                 extends NavigationEvent
case object MoveToEnd                   extends NavigationEvent
case object SelectAll                   extends TextEntryEvent
case object NewLine                     extends TextEntryEvent
case object PageDown                    extends NavigationEvent
case object PageUp                      extends NavigationEvent
case object MoveToEndOfFile             extends NavigationEvent
case object MoveToStartOfFile           extends NavigationEvent
final case class ScrollDown(lines: Int) extends ScrollEvent
final case class ScrollUp(lines: Int)   extends ScrollEvent
case object OpenGotoLine                extends ModalRequestEvent
case object OpenReplace                 extends ModalRequestEvent
case object FindNext                    extends TextEntryEvent
case object Enter                       extends TextEntryEvent
case object OpenFind                    extends ModalRequestEvent
case object Escape                      extends TextEntryEvent
case object TabKey                      extends TextEntryEvent
case object ReverseTabKey               extends TextEntryEvent
case object Copy                        extends TextEntryEvent
case object Paste                       extends TextEntryEvent
case object Cut                         extends TextEntryEvent
case object Undo                        extends TextEntryEvent
case object Redo                        extends TextEntryEvent
case object ToggleSyntaxHighlighting    extends TextEntryEvent
