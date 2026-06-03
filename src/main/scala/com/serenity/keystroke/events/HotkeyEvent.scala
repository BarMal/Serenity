package com.serenity.keystroke.events

sealed trait HotkeyEvent extends TextEntryEvent with AppEvent

case object Save                     extends HotkeyEvent
case object Quit                     extends HotkeyEvent with GlobalAppEvent
case object Undo                     extends HotkeyEvent
case object Redo                     extends HotkeyEvent
case object Copy                     extends HotkeyEvent
case object Paste                    extends HotkeyEvent
case object Cut                      extends HotkeyEvent
case object ToggleSyntaxHighlighting extends HotkeyEvent
case object OpenFile                 extends HotkeyEvent with FileEvent
case object SaveFile                 extends HotkeyEvent with FileEvent
case object ToggleCommandRunner      extends HotkeyEvent with GlobalAppEvent
case object NewTab                   extends HotkeyEvent with GlobalAppEvent // Ctrl+T
case object CloseTab                 extends HotkeyEvent with GlobalAppEvent // Ctrl+W
case object NextTab                  extends HotkeyEvent with GlobalAppEvent // Ctrl+Tab
case object PreviousTab              extends HotkeyEvent with GlobalAppEvent // Ctrl+Shift+Tab
case object FileSearch               extends HotkeyEvent with GlobalAppEvent // Ctrl+Shift+F
