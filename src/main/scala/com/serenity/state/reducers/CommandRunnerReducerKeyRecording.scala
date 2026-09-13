package com.serenity.state.reducers

import com.serenity.command.*
import com.serenity.config.HotkeyTrigger
import com.serenity.keystroke.KeyboardFidelityTier
import com.serenity.state.models.*

/** Keybinding capture, conflict resolution, and double-tap recording for a settings row of `InputKind.Binding` -- split
  * out of `CommandRunnerReducer` to keep both under the architecture size targets, see that object's own doc.
  */
private[reducers] object CommandRunnerReducerKeyRecording:
  private val DoubleTapWindowMillis = 200L

  def beginBindingCapture(state: AppState, item: CommandSurfaceItem.InputItem): ReducerResult =
    ReducerResult.noEffects(
      CommandRunnerReducer.replaceRunner(
        state,
        r => r.beginSubmenuRecording(item.id).copy(statusMessage = Some("Press a key or shortcut to assign"))
      )
    )

  def resolveGlobalHotkeyConflict(
    state: AppState,
    item: CommandSurfaceItem.InputItem,
    page: SettingsPage
  ): ReducerResult =
    page.recording.flatMap(_.pendingGlobalHotkeyConflict).fold(ReducerResult.noEffects(state)) {
      case (action, binding) =>
        ReducerResult(
          state = CommandRunnerReducer.replaceRunner(
            state,
            r => r.clearSubmenuEditingAndRecording.copy(statusMessage = None)
          ),
          effects = List(
            AppEffect.ExecuteCommand(
              Command.typed(
                item.id,
                item.label,
                CommandIntent.Keybindings(KeybindingsIntent.ResolveGlobalHotkeyConflict(action, binding)),
                item.category
              )
            )
          )
        )
    }

  def resolveFocusedKeymapConflict(
    state: AppState,
    item: CommandSurfaceItem.InputItem,
    page: SettingsPage
  ): ReducerResult =
    page.recording.flatMap(_.pendingFocusedKeymapConflict).fold(ReducerResult.noEffects(state)) {
      case (itemId, binding) =>
        ReducerResult(
          state = CommandRunnerReducer.replaceRunner(
            state,
            r => r.clearSubmenuEditingAndRecording.copy(statusMessage = None)
          ),
          effects = List(
            AppEffect.ExecuteCommand(
              Command.typed(
                item.id,
                item.label,
                CommandIntent.Keybindings(KeybindingsIntent.ResolveFocusedKeymapConflict(itemId, binding)),
                item.category
              )
            )
          )
        )
    }

  def recordBinding(
    state: AppState,
    info: com.serenity.keystroke.KeyStrokeInfo,
    recordedAtMillis: Long
  ): ReducerResult =
    CommandRunnerReducer.currentRunner(state) match
      case None =>
        ReducerResult.noEffects(state)
      case Some(runner) =>
        runner.activeSettingsSurface match
          case None =>
            ReducerResult.noEffects(state)
          case Some(surface) =>
            surface.current.recording match
              case None =>
                ReducerResult.noEffects(state)
              case Some(recording) =>
                runner.submenuItems(surface.current.groupId).find(_.id == recording.itemId) match
                  case Some(item: CommandSurfaceItem.InputItem) =>
                    recording.pendingRecordedBinding match
                      case None =>
                        ReducerResult(
                          CommandRunnerReducer.replaceRunner(
                            state,
                            current =>
                              current
                                .withPendingRecordedBinding(info, recordedAtMillis)
                                .copy(statusMessage =
                                  Some("Press the same key again within 200ms to record a double tap")
                                )
                          ),
                          List(AppEffect.ScheduleCommandRunnerBindingExpiry(recordedAtMillis))
                        )
                      case Some((first, firstAt))
                          if recordedAtMillis >= firstAt &&
                            recordedAtMillis - firstAt <= DoubleTapWindowMillis &&
                            sameKeyStroke(first, info) =>
                        assignRecordedBinding(state, item, first)
                      case Some((first, _)) =>
                        assignRecordedBinding(state, item, first)
                  case _ => ReducerResult.noEffects(state)

  def expireRecordedBinding(state: AppState, recordedAtMillis: Long): ReducerResult =
    CommandRunnerReducer
      .currentRunner(state)
      .flatMap { runner =>
        runner.activeSettingsSurface.flatMap { surface =>
          surface.current.recording.flatMap { recording =>
            recording.pendingRecordedBinding match
              case Some((first, pendingAt)) if pendingAt == recordedAtMillis =>
                runner.submenuItems(surface.current.groupId).find(_.id == recording.itemId) match
                  case Some(item: CommandSurfaceItem.InputItem) => Some(assignRecordedBinding(state, item, first))
                  case _                                        => None
              case _ => None
          }
        }
      }
      .getOrElse(ReducerResult.noEffects(state))

  private def sameKeyStroke(
    left: com.serenity.keystroke.KeyStrokeInfo,
    right: com.serenity.keystroke.KeyStrokeInfo
  ): Boolean =
    left.keyType == right.keyType && left.character == right.character && left.modifiers == right.modifiers

  private def assignRecordedBinding(
    state: AppState,
    item: CommandSurfaceItem.InputItem,
    first: com.serenity.keystroke.KeyStrokeInfo
  ): ReducerResult =
    val trigger = HotkeyTrigger(first.keyType, first.character, first.modifiers)
    val binding = trigger.render
    item.parse(binding) match
      case Some(intent) =>
        ReducerResult(
          state = CommandRunnerReducer.replaceRunner(
            state,
            current =>
              current.clearSubmenuEditingAndRecording
                .copy(statusMessage = bareModifierFidelityWarning(current, trigger, binding))
          ),
          effects = List(AppEffect.ExecuteCommand(Command.typed(item.id, item.label, intent, item.category)))
        )
      case None =>
        ReducerResult.noEffects(
          CommandRunnerReducer.replaceRunner(
            state,
            _.copy(statusMessage = Some(CommandRunnerReducer.invalidInputMessage(item, binding)))
          )
        )

  /** Issue #1194: a bare-modifier double tap (`ctrl+ctrl`, ...) has no representation in xterm's `modifyOtherKeys` wire
    * format -- there is no bare press/release event for a lone modifier in that protocol, only in the kitty keyboard
    * protocol's flags -- so recording one on a TUI session capped at [[KeyboardFidelityTier.ModifyOtherKeys]] would
    * otherwise silently record a binding that can never fire. GUI mode and a kitty-tier TUI session are always
    * [[KeyboardFidelityTier.Full]], so this never fires there.
    */
  private def bareModifierFidelityWarning(
    runner: CommandRunner,
    trigger: HotkeyTrigger,
    binding: String
  ): Option[String] =
    Option.when(
      runner.isTuiMode &&
        runner.keyboardFidelityTier == KeyboardFidelityTier.ModifyOtherKeys &&
        trigger.isBareModifierChord
    )(
      s"\"$binding\" recorded, but won't fire -- this terminal can't send a bare-modifier key event " +
        "at its negotiated keyboard protocol tier"
    )

  def submenuRecording(state: AppState): Boolean =
    CommandRunnerReducer.activeSubmenu(state).exists(_.current.recording.nonEmpty)

  def clearSubmenuRecording(state: AppState): AppState =
    CommandRunnerReducer.replaceRunner(
      state,
      runner => runner.clearSubmenuEditingAndRecording.copy(statusMessage = None)
    )
