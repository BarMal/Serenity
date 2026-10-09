package com.serenity.state.reducers

import com.serenity.command.*
import com.serenity.keystroke.events.{CommandRunnerEvent, RunnerSubmit}
import com.serenity.state.models.AppState

/** How the command runner's settings rows and pickers preview a value rather than commit it: Left/Right on a row,
  * highlighting a value in a picker, and Enter or leaving the row afterwards. The pending value itself is
  * [[SettingsPreviewReducer]]'s.
  */
private[reducers] object CommandRunnerReducerSettingsPreview:

  /** A preview belongs to the row it was made on: submitting ends it by committing it, and moving off the row abandons
    * it.
    */
  def settled(event: CommandRunnerEvent, result: ReducerResult): ReducerResult =
    val submitted = event match
      case RunnerSubmit => SettingsPreviewReducer.committed(result)
      case _            => result
    submitted.state.runtime.pendingSetting match
      case Some(pending) if !previewScope(submitted.state).contains(pending.scope) =>
        val abandoned = SettingsPreviewReducer.revert(submitted.state)
        ReducerResult(abandoned.state, submitted.effects ++ abandoned.effects)
      case _ => submitted

  /** Highlighting a value in a picker -- a font family, a theme -- shows it without committing it. */
  def previewHighlighted(state: AppState): ReducerResult =
    val highlighted = for
      runner  <- CommandRunnerReducer.currentRunner(state)
      submenu <- CommandRunnerReducer.activeSubmenu(state)
      item    <- CommandRunnerReducer.submenuSelectedItem(runner)
    yield (submenu.current.groupId, item)
    highlighted match
      case Some((groupId, CommandSurfaceItem.CommandItem(command, _)))
          if SettingsPreviewReducer.isPreviewable(command.intent) =>
        SettingsPreviewReducer.preview(state, groupId, command)
      case _ => ReducerResult.noEffects(state)

  /** The setting under the highlight at the root: the row itself, or the row a search result points at, so a result
    * cycles in place.
    */
  def valueRow(runner: CommandRunner): Option[CommandSurfaceItem] =
    runner.selectedItem.flatMap {
      case result: CommandSurfaceItem.SettingSearchItem =>
        runner.submenuItems(result.targetGroupId).find(_.id == result.targetItemId)
      case row => Some(row)
    }

  def stepped(state: AppState, input: CommandSurfaceItem.InputItem, delta: Int): ReducerResult =
    input
      .steppedIntent(delta)
      .fold(ReducerResult.noEffects(state))(intent =>
        changed(state, input.id, Command.typed(input.id, input.label, intent, input.category))
      )

  /** A value a setting row has been moved to: previewed where it can be put back, committed where it cannot. */
  def changed(state: AppState, scope: String, command: Command): ReducerResult =
    if SettingsPreviewReducer.isPreviewable(command.intent) then SettingsPreviewReducer.preview(state, scope, command)
    else ReducerResult.withEffect(state, AppEffect.ExecuteCommand(command))

  /** What a preview is scoped to: a setting row, or the picker whose highlighted value is being previewed. */
  private def previewScope(state: AppState): Option[String] =
    CommandRunnerReducer.currentRunner(state).flatMap { runner =>
      if CommandRunnerReducer.submenuHasFocus(state) then
        CommandRunnerReducer.submenuSelectedItem(runner).flatMap {
          case option: CommandSurfaceItem.OptionItem => Some(option.id)
          case input: CommandSurfaceItem.InputItem   => Some(input.id)
          case CommandSurfaceItem.CommandItem(command, _) if SettingsPreviewReducer.isPreviewable(command.intent) =>
            CommandRunnerReducer.activeSubmenu(state).map(_.current.groupId)
          case _ => None
        }
      else
        runner.selectedItem.flatMap {
          case option: CommandSurfaceItem.OptionItem        => Some(option.id)
          case input: CommandSurfaceItem.InputItem          => Some(input.id)
          case result: CommandSurfaceItem.SettingSearchItem => Some(result.targetItemId)
          case _                                            => None
        }
    }

  /** Enter on a value that has been previewed but is not otherwise submittable commits the preview: a stepped number
    * has no edit to submit, and a search result would otherwise open its setting.
    */
  def committedPreview(state: AppState): Option[ReducerResult] =
    for
      runner <- CommandRunnerReducer.currentRunner(state)
      scope  <- previewScope(state)
      if isSteppedRow(state, runner)
      commit <- SettingsPreviewReducer.committing(state, scope)
    yield commit

  private def isSteppedRow(state: AppState, runner: CommandRunner): Boolean =
    if CommandRunnerReducer.submenuHasFocus(state) then
      CommandRunnerReducer.submenuSelectedItem(runner) match
        case Some(_: CommandSurfaceItem.InputItem) => !CommandRunnerReducer.submenuEditing(state)
        case _                                     => false
    else
      runner.selectedItem match
        case Some(_: CommandSurfaceItem.SettingSearchItem) => true
        case _                                             => false
