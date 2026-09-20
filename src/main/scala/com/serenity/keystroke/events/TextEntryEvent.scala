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
case object ExtendSelectionWordLeft     extends NavigationEvent
case object ExtendSelectionWordRight    extends NavigationEvent
case object ExtendSelectionToLineStart  extends NavigationEvent
case object ExtendSelectionToLineEnd    extends NavigationEvent
case object ExtendSelectionPageUp       extends NavigationEvent
case object ExtendSelectionPageDown     extends NavigationEvent
case object MoveToStart                 extends NavigationEvent
case object MoveToEnd                   extends NavigationEvent
case object SelectAll                   extends TextEntryEvent
case object NewLine                     extends TextEntryEvent
case object PageDown                    extends NavigationEvent
case object PageUp                      extends NavigationEvent
case object MoveToEndOfFile             extends NavigationEvent
case object MoveToStartOfFile           extends NavigationEvent
// Column-based document layout (issue #1338, Phase 1): PageUp/PageDown's column-mode counterparts, jumping exactly
// one column's worth of visual rows in the given direction. Only reached while column mode and word wrap are both on
// -- see the key-resolution site.
case object ColumnLeft                  extends NavigationEvent
case object ColumnRight                 extends NavigationEvent
final case class ScrollDown(lines: Int) extends ScrollEvent
final case class ScrollUp(lines: Int)   extends ScrollEvent
// Horizontal scroll gestures (issue #1568): shift+wheel or a trackpad's own horizontal notches, the same input-layer
// counterpart to ScrollDown/ScrollUp above. `EditorEventReducer` pans `leftColumn` with these while word wrap is off,
// or -- while column mode and word wrap are both on -- reduces them exactly as `ColumnLeft`/`ColumnRight` already are.
final case class ScrollLeft(columns: Int)  extends ScrollEvent
final case class ScrollRight(columns: Int) extends ScrollEvent
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
