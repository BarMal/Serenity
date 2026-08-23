package com.serenity.keystroke.events

/** Vertical cursor movement is the one editor event family whose outcome depends on measured text geometry (wrapped
  * visual lines and proportional caret positions). It is carved out of [[TextEntryEvent]] so the type system routes it
  * through the geometry-carrying reducer entry and keeps the geometry-free `reduce` unable to receive it.
  */
sealed trait VerticalNavigationEvent

case object MoveUp              extends VerticalNavigationEvent
case object MoveDown            extends VerticalNavigationEvent
case object ExtendSelectionUp   extends VerticalNavigationEvent
case object ExtendSelectionDown extends VerticalNavigationEvent
