package com.serenity.keystroke.events

/** The event algebra, expressed as unions of the per-family sealed traits rather than as a nominal root they all
  * inherit.
  *
  * Scala scopes `sealed` to a single file, so making `Event` a sealed trait would have meant collapsing every event
  * into one file -- roughly a hundred cases -- because a sealed trait's children must sit beside it. A union asks for
  * nothing of the sort: each family stays sealed in its own file, and the compiler still decomposes the union when
  * checking a match, so dispatch sites are exhaustiveness-checked.
  *
  * `StateManagerEventPipeline.dispatchEvent` is the site this exists for: it names every family instead of ending in a
  * catch-all, so the compiler reports the exact events a missing route would swallow. That check is a warning until
  * `-Werror` lands with #1015, but it names the culprit rather than staying silent.
  *
  * The trade is that these are no longer types anything can extend. Adding a family means adding it here, and until it
  * appears in `Event` its values are not events at all -- so an unrouted family fails to compile at the call site
  * rather than compiling cleanly and being swallowed by a catch-all at runtime, which is what the nominal hierarchy
  * allowed. The same closure applies to tests: a component test cannot mint its own throwaway event and must use a real
  * one.
  */
type Event = EditorEvent | AppEvent | SystemEvent | SurfaceEvent | MouseInputEvent

/** Input aimed at the text being edited. */
type EditorEvent = TextEntryEvent

/** Things the application does to itself: window chrome, tabs, files, theming. */
type AppEvent = GlobalAppEvent | FileEvent | ThemeEvent

/** Things that happened elsewhere and the application is being told about. */
type SystemEvent = LspEvent | ExplorerEvent | ResizeEvent | UnhandledEvent[?]

/** Input aimed at whichever surface currently holds focus. */
type SurfaceEvent = CommandRunnerEvent | ModalInputEvent | PanelInputEvent | PeekInputEvent | StartupPageEvent
