package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.command.*
import com.serenity.config.{
  AppConfig,
  CommandRunnerKeyAction,
  EditorKeyAction,
  FocusedKeymapConfig,
  HotkeyTrigger,
  KeymapEventAction,
  KeymapGroup,
  ModalKeyAction,
  PanelKeyAction,
  PeekKeyAction
}
import com.serenity.keystroke.events.Event
import com.serenity.state.models.*

/** Global-hotkey and focused-keymap binding effects: recording a new binding, and surfacing the "already assigned"
  * conflict prompt on the one live `CommandPalette` surface rather than a second floating one.
  */
final private[manager] class StateManagerKeybindingEffects(
    stateRef: Ref[IO, AppState],
    updateConfig: (AppConfig => AppConfig) => IO[AppConfig]
):

  private[manager] def interpret(intent: KeybindingsIntent): IO[Unit] =
    intent match
      case KeybindingsIntent.SetGlobalHotkey(action, binding) =>
        updateGlobalHotkeyBinding(action, binding)
      case KeybindingsIntent.ResolveGlobalHotkeyConflict(action, binding) =>
        updateConfig(_.withHotkeyOverrideUnbindingConflicts(action, binding)).void
      case KeybindingsIntent.ResolveFocusedKeymapConflict(itemId, binding) =>
        updateConfig(resolveFocusedKeymapConflict(itemId, binding)).void
      case KeybindingsIntent.SetEditorKeyBinding(action, binding) =>
        setKeymapBinding("keymap-editor-", KeymapGroup.Editor)(action, binding)
      case KeybindingsIntent.SetCommandRunnerKeyBinding(action, binding) =>
        setKeymapBinding("keymap-command-runner-", KeymapGroup.CommandRunner)(action, binding)
      case KeybindingsIntent.SetModalKeyBinding(action, binding) =>
        setKeymapBinding("keymap-modal-", KeymapGroup.Modal)(action, binding)
      case KeybindingsIntent.SetPanelKeyBinding(action, binding) =>
        setKeymapBinding("keymap-panel-", KeymapGroup.Panel)(action, binding)
      case KeybindingsIntent.SetPeekKeyBinding(action, binding) =>
        setKeymapBinding("keymap-peek-", KeymapGroup.Peek)(action, binding)
      case KeybindingsIntent.ResetGlobalHotkey(action) =>
        updateConfig(_.resetHotkeyOverride(action)).void
      case KeybindingsIntent.ResetEditorKeyBinding(action) =>
        updateConfig(_.resetKeymapBinding(KeymapGroup.Editor)(action)).void
      case KeybindingsIntent.ResetCommandRunnerKeyBinding(action) =>
        updateConfig(_.resetKeymapBinding(KeymapGroup.CommandRunner)(action)).void
      case KeybindingsIntent.ResetModalKeyBinding(action) =>
        updateConfig(_.resetKeymapBinding(KeymapGroup.Modal)(action)).void
      case KeybindingsIntent.ResetPanelKeyBinding(action) =>
        updateConfig(_.resetKeymapBinding(KeymapGroup.Panel)(action)).void
      case KeybindingsIntent.ResetPeekKeyBinding(action) =>
        updateConfig(_.resetKeymapBinding(KeymapGroup.Peek)(action)).void

  private def setKeymapBinding[A <: KeymapEventAction[E], E <: Event](
    prefix: String,
    group: KeymapGroup[A, E]
  )(action: A, binding: String): IO[Unit] =
    updateKeyBinding(s"$prefix${action.configKey}", binding, _.withKeymapBinding(group)(action, binding))

  private def updateKeyBinding(itemId: String, binding: String, update: AppConfig => AppConfig): IO[Unit] =
    stateRef.get.flatMap { state =>
      val updatedConfig = update(state.persisted.config)
      if updatedConfig == state.persisted.config then
        if currentFocusedKeymapOwnsBinding(state.persisted.config, itemId, binding) then IO.unit
        else stateRef.update(withFocusedKeymapConflictMessage(itemId, binding))
      else updateConfig(_ => updatedConfig).void
    }

  private def currentFocusedKeymapOwnsBinding(config: AppConfig, itemId: String, binding: String): Boolean =
    HotkeyTrigger
      .parse(binding)
      .exists(trigger =>
        StateManagerKeybindingEffects.keymapGroupBindings
          .exists(_.ownsBinding(config.inputConfig.focusedKeymapConfig, itemId, trigger))
      )

  private def resolveFocusedKeymapConflict(itemId: String, binding: String)(config: AppConfig): AppConfig =
    StateManagerKeybindingEffects.keymapGroupBindings
      .flatMap(_.resolveConflict(config, itemId, binding))
      .headOption
      .getOrElse(config)

  private def updateGlobalHotkeyBinding(action: com.serenity.config.HotkeyAction, binding: String): IO[Unit] =
    stateRef.get.flatMap { state =>
      val updatedConfig = state.persisted.config.withHotkeyOverride(action, binding)
      if updatedConfig == state.persisted.config then
        stateRef.update(
          withGlobalKeymapConflictMessage(action, binding)
        )
      else updateConfig(_ => updatedConfig).void
    }

  private def withFocusedKeymapConflictMessage(itemId: String, binding: String)(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
      case current @ UiSurface(_, SurfaceContent.CommandPalette(runner), _, _) =>
        current.copy(content = SurfaceContent.CommandPalette(withFocusedKeymapConflict(runner, itemId, binding)))
      case current => current
    }))

  private def withFocusedKeymapConflict(runner: CommandRunner, itemId: String, binding: String): CommandRunner =
    runner.activeSettingsSurface match
      case Some(surface) =>
        runner
          .withDrilledSettingsSurface(
            surface.copy(current =
              SettingsPage.Editing(
                groupId = surface.current.groupId,
                itemId = itemId,
                draftText = binding,
                searchTerm = surface.current.searchTerm,
                recording = Some(RecordingState(itemId, pendingFocusedKeymapConflict = Some(itemId -> binding)))
              )
            )
          )
          .copy(statusMessage =
            Some("Binding is already assigned. Enter to unbind the other action, or Escape to preserve it.")
          )
      case None =>
        runner

  private def withGlobalKeymapConflictMessage(
    action: com.serenity.config.HotkeyAction,
    binding: String
  )(state: AppState): AppState =
    state.commandRunnerSurface match
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            runner.activeSettingsSurface match
              case Some(drilled) =>
                val updatedRunner = runner
                  .withDrilledSettingsSurface(
                    drilled.copy(current =
                      SettingsPage.Editing(
                        groupId = drilled.current.groupId,
                        itemId = s"keymap-global-${action.configKey}",
                        draftText = binding,
                        searchTerm = drilled.current.searchTerm,
                        recording = Some(
                          RecordingState(
                            s"keymap-global-${action.configKey}",
                            pendingGlobalHotkeyConflict = Some(action -> binding)
                          )
                        )
                      )
                    )
                  )
                  .copy(statusMessage =
                    Some("Binding is already assigned. Enter to unbind the other action, or Escape to preserve it.")
                  )
                state.copy(runtime =
                  state.runtime.copy(uiSurfaces =
                    state.runtime.uiSurfaces.replacedWhere(_.id == surface.id)(
                      _.copy(content = SurfaceContent.CommandPalette(updatedRunner))
                    )
                  )
                )
              case None => state
          case _ => state
      case None => state

private[manager] object StateManagerKeybindingEffects:

  /** One keymap group's item-id prefix and action set, bundled with its [[KeymapGroup]] lens. */
  final private case class KeymapGroupBinding[A <: KeymapEventAction[E], E <: Event](
      prefix: String,
      group: KeymapGroup[A, E],
      values: Array[A]
  ):
    def actionFor(itemId: String): Option[A] =
      Option.when(itemId.startsWith(prefix))(itemId.stripPrefix(prefix)).flatMap(key => values.find(_.configKey == key))

    def ownsBinding(config: FocusedKeymapConfig, itemId: String, trigger: HotkeyTrigger): Boolean =
      actionFor(itemId).exists(action => group.get(config).bindingsFor(action).contains(trigger))

    def resolveConflict(config: AppConfig, itemId: String, binding: String): Option[AppConfig] =
      actionFor(itemId).map(action => config.withKeymapBindingUnbindingConflicts(group)(action, binding))

  private val keymapGroupBindings: List[KeymapGroupBinding[?, ?]] = List(
    KeymapGroupBinding("keymap-editor-", KeymapGroup.Editor, EditorKeyAction.values),
    KeymapGroupBinding("keymap-command-runner-", KeymapGroup.CommandRunner, CommandRunnerKeyAction.values),
    KeymapGroupBinding("keymap-modal-", KeymapGroup.Modal, ModalKeyAction.values),
    KeymapGroupBinding("keymap-panel-", KeymapGroup.Panel, PanelKeyAction.values),
    KeymapGroupBinding("keymap-peek-", KeymapGroup.Peek, PeekKeyAction.values)
  )
