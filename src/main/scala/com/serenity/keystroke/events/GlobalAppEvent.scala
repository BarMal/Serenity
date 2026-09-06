package com.serenity.keystroke.events

import com.serenity.keystroke.Modifier
import com.serenity.state.models.PanelId

/** Handled by `AppEventReducer`. Deliberately not editor events: routing is decided by this parent, never by case order
  * in a match.
  */
sealed trait GlobalAppEvent

case object Quit                    extends GlobalAppEvent
case object ToggleCommandRunner     extends GlobalAppEvent
case object ToggleContextualToolbar extends GlobalAppEvent
case object ToggleShortcutsHelp     extends GlobalAppEvent // F1 (issue #1247)
case object ToggleTabList           extends GlobalAppEvent // issue #1307
case object ToggleRecentFilesInMode extends GlobalAppEvent // issue #1307

/** Toggles a registered panel's floating (command-palette) presentation open or closed (issue #1310) -- the parametric
  * counterpart to `ToggleTabList`/`ToggleRecentFilesInMode` above, driven by `PanelRegistry` instead of a new case per
  * panel.
  */
final case class TogglePanel(id: PanelId) extends GlobalAppEvent
case object NewTab                        extends GlobalAppEvent // Ctrl+T
case object CloseTab                      extends GlobalAppEvent // Ctrl+W
case object NextTab                       extends GlobalAppEvent // Ctrl+Tab
case object PreviousTab                   extends GlobalAppEvent // Ctrl+Shift+Tab
case object FileSearch                    extends GlobalAppEvent // Ctrl+Shift+F

/** Raw bare-modifier press/release, always emitted by `SwingInputHandler` for every modifier key regardless of the
  * cursor-peek prototype's `commandRunnerCursorPeekEnabled` flag -- like mouse-move events, the translator emits
  * unconditionally and `AppEventReducer` decides whether the flag makes them relevant. Entirely independent of
  * `SwingInputHandler`'s existing `pendingModifierTap` (`ctrl+ctrl`-style hotkey) tracking, which these do not affect.
  */
final case class CursorPeekModifierPressed(modifier: Modifier, atMillis: Long)  extends GlobalAppEvent
final case class CursorPeekModifierReleased(modifier: Modifier, atMillis: Long) extends GlobalAppEvent

/** Emitted alongside every non-modifier key press, matching `ModifierTapDetector.otherKeyPressed`'s existing
  * cancellation trigger point in `SwingInputHandler.translatePressed`; cancels a pending cursor-peek gesture the same
  * way a real key already cancels the existing bare-modifier hotkey tap.
  */
case object CursorPeekOtherKeyPressed extends GlobalAppEvent
