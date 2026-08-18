package com.serenity.keystroke.events

/** What a focused surface was asked to do, independent of which surface is focused.
  *
  * Raw input events say what the keyboard did; a `FocusIntent` says what it meant. Splitting the two is what lets one
  * translation table serve every surface: before this, each of the five focused surfaces carried its own `fromEvent`
  * restating the same fifteen keys, so adding a navigation key to four of them and forgetting the fifth was a silent
  * omission rather than a compile error.
  *
  * Deliberately narrow: it covers only the vocabulary more than one surface shares. Anything genuinely specific to a
  * single surface -- selecting a command category, recording a binding, clicking a modal control -- stays that
  * surface's own event and never becomes an intent.
  *
  * Several cases share a name with the event they are translated from. The translation itself therefore lives in
  * [[SurfaceInput]] rather than in this companion, where these cases would shadow the same-named events and silently
  * capture the patterns meant to match them.
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
