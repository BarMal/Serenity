package com.serenity.keystroke.events

import com.serenity.keystroke.Modifier
import com.serenity.state.models.{BufferId, PanelId}

/** Handled by `AppEventReducer`. Deliberately not editor events: routing is decided by this parent, never by case order
  * in a match.
  */
sealed trait GlobalAppEvent

case object Quit                    extends GlobalAppEvent
case object ToggleCommandRunner     extends GlobalAppEvent
case object ToggleContextualToolbar extends GlobalAppEvent
case object ToggleShortcutsHelp     extends GlobalAppEvent // F1 (issue #1247)
case object ToggleTabList           extends GlobalAppEvent // issue #1307
case object ToggleChapterGhosts     extends GlobalAppEvent // Ctrl+Shift+G
case object OpenChapterNote         extends GlobalAppEvent // Ctrl+Shift+N
case object ToggleNotesPin          extends GlobalAppEvent // Ctrl+Shift+L
case object ToggleRecentFilesInMode extends GlobalAppEvent // issue #1307

/** A settings preview was put back because the command runner went away without committing it. */
case object SettingsPreviewAbandoned extends GlobalAppEvent

/** Toggles a registered panel's floating (command-palette) presentation open or closed (issue #1310) -- the parametric
  * counterpart to `ToggleTabList`/`ToggleRecentFilesInMode` above, driven by `PanelRegistry` instead of a new case per
  * panel.
  */
final case class TogglePanel(id: PanelId) extends GlobalAppEvent
case object NewTab                        extends GlobalAppEvent // Ctrl+T
case object CloseTab                      extends GlobalAppEvent // Ctrl+W
case object SplitPaneHorizontal           extends GlobalAppEvent // Ctrl+D
case object SplitPaneVertical             extends GlobalAppEvent // Ctrl+Shift+D
case object ClosePane                     extends GlobalAppEvent // Ctrl+Shift+W
case object NextTab                       extends GlobalAppEvent // Ctrl+Tab
case object PreviousTab                   extends GlobalAppEvent // Ctrl+Shift+Tab
case object MoveTabLeft                   extends GlobalAppEvent // Ctrl+Shift+PageUp (issue #1610)
case object MoveTabRight                  extends GlobalAppEvent // Ctrl+Shift+PageDown (issue #1610)
case object FileSearch                    extends GlobalAppEvent // Ctrl+Shift+F
case object GoToFile                      extends GlobalAppEvent // Ctrl+E

/** Runs the registry command with this id (`Command.name`), as choosing it in the palette would: a global key bound to
  * a command rather than to a `HotkeyAction` (issue #1922).
  */
final case class RunCommand(commandId: String) extends GlobalAppEvent

/** Moves focus to the editor pane or docked panel next to the focused one on screen (Alt+Arrow by default). */
final case class FocusInDirection(direction: Direction) extends GlobalAppEvent

/** Close-by-id (issue #1078): a tab-bar close-affordance click closing a specific tab, whether or not it is focused --
  * `CloseTab`'s mouse counterpart, sharing `EditorState.closeBuffer` with it rather than requiring a focus switch
  * first.
  */
final case class CloseTabById(bufferId: BufferId) extends GlobalAppEvent

/** Raw bare-modifier press/release, emitted by `SwingInputHandler` for every modifier key while the cursor-peek
  * prototype's `commandRunnerCursorPeekEnabled` flag is on (#1845); `AppEventReducer` decides what they do. Entirely
  * independent of `SwingInputHandler`'s existing `pendingModifierTap` (`ctrl+ctrl`-style hotkey) tracking, which these
  * do not affect.
  */
final case class CursorPeekModifierPressed(modifier: Modifier, atMillis: Long)  extends GlobalAppEvent
final case class CursorPeekModifierReleased(modifier: Modifier, atMillis: Long) extends GlobalAppEvent

/** Emitted alongside every non-modifier key press while cursor peek is on, matching
  * `ModifierTapDetector.otherKeyPressed`'s existing cancellation trigger point in `SwingInputHandler.translatePressed`;
  * cancels a pending cursor-peek gesture the same way a real key already cancels the existing bare-modifier hotkey tap.
  */
case object CursorPeekOtherKeyPressed extends GlobalAppEvent
