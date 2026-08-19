package com.serenity.keystroke.events

/** Application-wide actions: window chrome, tab lifecycle and the command runner.
  *
  * These are handled by `AppEventReducer` and are deliberately *not* editor events -- routing is decided by this parent
  * alone, never by where a case happens to sit in a match.
  */
sealed trait GlobalAppEvent

case object Quit                    extends GlobalAppEvent
case object ToggleCommandRunner     extends GlobalAppEvent
case object ToggleContextualToolbar extends GlobalAppEvent
case object NewTab                  extends GlobalAppEvent // Ctrl+T
case object CloseTab                extends GlobalAppEvent // Ctrl+W
case object NextTab                 extends GlobalAppEvent // Ctrl+Tab
case object PreviousTab             extends GlobalAppEvent // Ctrl+Shift+Tab
case object FileSearch              extends GlobalAppEvent // Ctrl+Shift+F
