package com.serenity.state.reducers

import com.serenity.command.*
import com.serenity.state.models.{AppState, PendingSetting}

/** Previewing a setting from the command runner: highlighting a value shows it, Enter commits it, and abandoning it
  * puts back what was there when the preview began. A preview is a [[PendingSetting]] over the committed config and
  * theme: the previewed value is live in the state, but saving reads the committed one, so a preview is never written
  * to disk.
  */
object SettingsPreviewReducer:

  /** Whether `intent` sets one value that can be put back by restoring the config and theme. Everything else is
    * committed as soon as it is chosen: relative changes (a toggle or step would apply again on each highlight),
    * commands that act (moving a status segment, saving, resetting), and settings that also reshape the workspace (app
    * mode, Markdown view mode, UI presets), which restoring the config would not undo.
    */
  def isPreviewable(intent: CommandIntent): Boolean =
    intent match
      case CommandIntent.Theme(ThemeIntent.ApplyTheme(_))           => true
      case CommandIntent.View(ViewIntent.SetDefaultDocumentMode(_)) => true
      case CommandIntent.Settings(settings)                         => isPreviewableSetting(settings)
      case _                                                        => false

  private def isPreviewableSetting(intent: SettingsIntent): Boolean =
    intent match
      case SettingsIntent.Font(font)               => isPreviewableFont(font)
      case SettingsIntent.Cursor(_)                => true
      case SettingsIntent.StatusLine(statusLine)   => isPreviewableStatusLine(statusLine)
      case SettingsIntent.TextDisplay(textDisplay) => isPreviewableTextDisplay(textDisplay)
      case SettingsIntent.InterfaceChrome(_)       => true
      case SettingsIntent.SpellCheck(spellCheck)   => isPreviewableSpellCheck(spellCheck)
      case SettingsIntent.General(general)         => isPreviewableGeneral(general)

  private def isPreviewableFont(intent: FontIntent): Boolean =
    intent match
      case FontIntent.SetFontSize(_) | FontIntent.SetCodeFontSize(_) | FontIntent.SetTextFontSize(_) |
          FontIntent.SetUiFontSize(_) | FontIntent.SetTextScaleMode(_) | FontIntent.SetTextScaleMultiplier(_) |
          FontIntent.SetCodeFontFamily(_) | FontIntent.SetTextFontFamily(_) | FontIntent.SetUiFontFamily(_) |
          FontIntent.SetLigatures(_) | FontIntent.SetCodeLigatures(_) | FontIntent.SetTextLigatures(_) |
          FontIntent.SetUiLigatures(_) =>
        true
      case _ => false

  private def isPreviewableStatusLine(intent: StatusLineIntent): Boolean =
    intent match
      case StatusLineIntent.SetSegmentIncluded(_, _) | StatusLineIntent.SetPlacement(_) |
          StatusLineIntent.SetWordGoal(_) =>
        true
      case _ => false

  private def isPreviewableTextDisplay(intent: TextDisplayIntent): Boolean =
    intent match
      case TextDisplayIntent.SetLineNumbers(_) | TextDisplayIntent.SetLineNumberSide(_) |
          TextDisplayIntent.SetLineNumberMarginLeft(_) | TextDisplayIntent.SetLineNumberMarginRight(_) |
          TextDisplayIntent.SetLineNumberPadding(_) | TextDisplayIntent.SetWordWrap(_) |
          TextDisplayIntent.SetVisualLineCursorNavigation(_) | TextDisplayIntent.SetTypewriterScrolling(_) |
          TextDisplayIntent.SetColumnMode(_) | TextDisplayIntent.SetColumnTargetWidth(_) |
          TextDisplayIntent.SetColumnGap(_) | TextDisplayIntent.SetColumnCount(_) |
          TextDisplayIntent.SetFocusedTextBody(_) | TextDisplayIntent.SetContextualToolbarEnabled(_) |
          TextDisplayIntent.SetContextualToolbarDisplayMode(_) | TextDisplayIntent.SetTextAreaLeftInset(_) |
          TextDisplayIntent.SetTextAreaRightInset(_) | TextDisplayIntent.SetTextAreaTopInset(_) |
          TextDisplayIntent.SetTextAreaBottomInset(_) | TextDisplayIntent.SetDropCapsEnabled(_) =>
        true
      case _ => false

  private def isPreviewableSpellCheck(intent: SpellCheckIntent): Boolean =
    intent match
      case SpellCheckIntent.SetSpellCheckEnabled(_) | SpellCheckIntent.SetSpellCheckLanguages(_) |
          SpellCheckIntent.SetSpellCheckDictionaryPaths(_) | SpellCheckIntent.SetSpellCheckWords(_) =>
        true
      case SpellCheckIntent.AddWordAtCursorToDictionary => false

  private def isPreviewableGeneral(intent: GeneralSettingsIntent): Boolean =
    intent match
      case GeneralSettingsIntent.SetRenderFpsTarget(_) | GeneralSettingsIntent.SetRenderDamageGranularity(_) |
          GeneralSettingsIntent.SetCommandRunnerVisibleRows(_) | GeneralSettingsIntent.SetCommandRunnerItemGapRows(_) |
          GeneralSettingsIntent.SetCommandRunnerCursorGapRows(_) =>
        true
      case GeneralSettingsIntent.OpenSettings | GeneralSettingsIntent.SaveConfig |
          GeneralSettingsIntent.ResetSettings =>
        false

  /** Shows `command`'s value without committing it. The first preview of a `scope` records the committed config and
    * theme to fall back on; a preview of another scope first abandons the one in progress.
    */
  def preview(state: AppState, scope: String, command: Command): ReducerResult =
    val switching = state.runtime.pendingSetting.exists(_.scope != scope)
    val abandoned = if switching then revert(state) else ReducerResult.noEffects(state)
    val pending = abandoned.state.runtime.pendingSetting.fold(
      PendingSetting(scope, command, abandoned.state.persisted.config, abandoned.state.persisted.theme)
    )(_.copy(previewed = command))
    ReducerResult(
      withPending(abandoned.state, Some(pending)),
      abandoned.effects :+ AppEffect.ExecuteCommandUnrecorded(command)
    )

  /** Puts back the committed config and theme, ending the preview. A theme load still in flight is dropped. */
  def revert(state: AppState): ReducerResult =
    state.runtime.pendingSetting.fold(ReducerResult.noEffects(state)) { pending =>
      val restored = state.copy(
        persisted = state.persisted.copy(config = pending.committedConfig, theme = pending.committedTheme),
        runtime = state.runtime.copy(
          pendingSetting = None,
          themeDiscovery = state.runtime.themeDiscovery.copy(requestedThemeName = Some(pending.committedTheme.name))
        )
      )
      ReducerResult(
        CommandRunnerReducer.replaceRunner(restored, _.updateInputItems(pending.committedConfig)),
        List(AppEffect.Settings(SettingsEffect.ReapplyConfig))
      )
    }

  /** `state` with a preview that has outlived the command runner put back: a preview exists only while the runner it
    * was made in is open, however the runner came to be gone.
    */
  def withoutOrphanedPreview(state: AppState): AppState =
    if state.commandRunnerSurface.isEmpty then revert(state).state else state

  /** Commits the value being previewed under `scope`, if there is one. */
  def committing(state: AppState, scope: String): Option[ReducerResult] =
    state.runtime.pendingSetting
      .filter(_.scope == scope)
      .map(pending => ReducerResult(withPending(state, None), List(AppEffect.ExecuteCommand(pending.previewed))))

  /** `result` with any preview ended, if it commits a command: the value becomes the committed one. */
  def committed(result: ReducerResult): ReducerResult =
    if result.effects.exists(executesCommand) then result.copy(state = withPending(result.state, None)) else result

  private def executesCommand(effect: AppEffect): Boolean =
    effect match
      case AppEffect.ExecuteCommand(_) => true
      case _                           => false

  private def withPending(state: AppState, pending: Option[PendingSetting]): AppState =
    state.copy(runtime = state.runtime.copy(pendingSetting = pending))
