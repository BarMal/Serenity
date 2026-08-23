package com.serenity.keystroke.events

/** Unions rather than a nominal root: `sealed` is file-scoped, so a sealed `Event` would force every case into one
  * file. The compiler decomposes a union when checking a match, so dispatch stays exhaustiveness-checked either way.
  *
  * Consequence: nothing can extend these. A new family must be added here or its values are not events at all.
  */
type Event = EditorEvent | AppEvent | SystemEvent | SurfaceEvent | MouseInputEvent

type EditorEvent = TextEntryEvent | VerticalNavigationEvent

type AppEvent = GlobalAppEvent | FileEvent | ThemeEvent

type SystemEvent = LspEvent | ExplorerEvent | ResizeEvent | UnhandledEvent[?]

type SurfaceEvent = CommandRunnerEvent | ModalInputEvent | PanelInputEvent | PeekInputEvent | StartupPageEvent
