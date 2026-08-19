package com.serenity.keystroke.events

/** What a focused surface was asked to do, independent of which surface is focused.
  *
  * Covers only vocabulary more than one surface shares; anything specific to a single surface stays that surface's own
  * event. Translation lives in [[SurfaceInput]], not this companion -- see the note there.
  */
enum FocusIntent:
  case Insert(char: Char)
  case DeleteBackward
  case DeleteForward
  case DeleteWordBackward
  case DeleteWordForward
  case Paste
  case Navigate(direction: Direction)
  case NextGroup
  case PreviousGroup
  case Submit
  case Dismiss
