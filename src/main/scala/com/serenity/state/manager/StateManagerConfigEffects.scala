package com.serenity.state.manager

import java.nio.file.Files

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.command.*
import com.serenity.config.AppConfigOps.*
import com.serenity.config.{
  AppConfig,
  ConfigError,
  ConfigLoadResult,
  ConfigManager,
  ConfigNotice,
  ConfigRestart,
  HotkeyOverrides,
  LineNumberLayout,
  SpellCheckLanguage,
  StatusLinePlacement,
  StatusSegment
}
import com.serenity.io.{FileStamp, TimestampedBackup}
import com.serenity.spellcheck.{DictionaryWord, SpellChecker}
import com.serenity.state.models.*
import com.serenity.state.reducers.{CommandRunnerReducer, SettingsPreviewReducer}

/** Config-update infrastructure and the settings-intent dispatch that drives it: appearance, cursor, panel chrome,
  * spell-check, and general settings all funnel through the same commit-then-persist path. The state change commits
  * once, validated, on the dispatcher; the config file write runs on the Config lane (#1697), once per burst of changes
  * and skipped when nothing in the file would change. An outside edit of the file is reloaded on that same lane
  * (#1934). The session is not involved: it keeps no settings.
  */
final private[manager] class StateManagerConfigEffects(
    currentState: IO[AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    configPersistencePath: Option[java.nio.file.Path],
    onFontConfigChanged: com.serenity.ui.fonts.FontLoader.FontConfig => IO[Unit],
    deviceTextScaleProvider: IO[Double],
    editor: EffectEditorPort,
    renderCaches: RenderCaches,
    saveConfig: (AppConfig, java.nio.file.Path) => IO[Either[ConfigError, Unit]] = ConfigManager.saveConfigIO,
    showNotice: Notice => IO[Unit] = _ => IO.unit,
    configOnDisk: Option[AppConfig] = None,
    loadConfig: java.nio.file.Path => IO[Either[ConfigError, ConfigLoadResult]] = ConfigFileSync.defaultLoad
)(using balance: com.serenity.rope.Balance):

  private val writes = WriteCoalescer.unsafe

  private val configFile: Option[ConfigFileSync] =
    configPersistencePath.map(ConfigFileSync.unsafe(_, configOnDisk, saveConfig, loadConfig))

  /** The config file to watch for outside edits, absent when this session has none to keep. */
  val watch: Option[ConfigFileWatch] =
    configFile.map(file => ConfigFileWatch(file.path.toAbsolutePath.normalize, reloadFromDisk(file)))

  private[manager] def updateConfig(update: AppConfig => AppConfig): IO[AppConfig] =
    applyConfigUpdate(update)

  private def updateAppearanceConfig(update: AppConfig => AppConfig): IO[AppConfig] =
    applyConfigUpdate(update)

  private[manager] def updateTextDisplayConfig(update: AppConfig => AppConfig): IO[AppConfig] =
    applyConfigUpdate(update)

  /** Commits `update` (plus `syncState`) as one validated model write, then asks for the config file to be written.
    * Returns the config live afterwards -- the old one if validation rejected the change.
    */
  private def applyConfigUpdate(
    update: AppConfig => AppConfig,
    syncState: (AppState, AppConfig) => AppState = (state, _) => state
  ): IO[AppConfig] =
    editor.updateModelValidated(model => Some(StateManagerConfigEffects.configTransition(model, update, syncState))) >>
      currentState
        .map(_.persisted.config)
        .flatTap(config =>
          IO(renderCaches.frameState.configureCacheCapacity(config.surfaceConfig.rendererFrameStateCacheCapacity))
        )
        .flatTap(_ => requestUnlessPreviewing)

  // A previewed value stays in memory: the config file keeps the committed one until the preview is accepted.
  private def requestUnlessPreviewing: IO[Unit] =
    currentState.flatMap(live => requestConfigWrite.unlessA(live.runtime.pendingSetting.isDefined))

  /** Pushes the live config into the parts of the runtime that learn of a change only when told, after the state's own
    * config was put back without going through an update.
    */
  private[manager] def reapplyConfig: IO[Unit] =
    editor.updateModelValidated(model =>
      Some(
        model.copy(app = StateManagerConfigEffects.withUpdatedRunnerConfig(model.app, model.app.persisted.config))
      )
    ) >>
      currentState
        .map(_.persisted.config)
        .flatMap(config =>
          IO(renderCaches.frameState.configureCacheCapacity(config.surfaceConfig.rendererFrameStateCacheCapacity)) >>
            onFontConfigChanged(config.editorConfig.fontConfig)
        )

  private[manager] def updateFontConfig(
    update: com.serenity.ui.fonts.FontLoader.FontConfig => com.serenity.ui.fonts.FontLoader.FontConfig
  ): IO[Unit] =
    deviceTextScaleProvider.flatMap { deviceTextScale =>
      applyConfigUpdate(config =>
        config.withFontConfig(update(config.editorConfig.fontConfig).resolveAutoTextScale(deviceTextScale))
      )
        .flatMap(config => onFontConfigChanged(config.editorConfig.fontConfig))
    }

  private def updateSpellCheckConfig(
    update: com.serenity.config.SpellCheckConfig => com.serenity.config.SpellCheckConfig
  ): IO[Unit] =
    applyConfigUpdate(config => config.withSpellCheck(update(config.languageToolsConfig.spellCheck))).void >>
      editor.scheduleDocumentAnalysis()

  private def clampFontSize(size: Float): Float =
    size.max(8.0f).min(48.0f)

  private[manager] def interpret(intent: SettingsIntent, state: AppState): IO[Unit] =
    intent match
      case SettingsIntent.Font(fontIntent)               => interpretFontIntent(fontIntent)
      case SettingsIntent.StatusLine(statusLineIntent)   => interpretStatusLineIntent(statusLineIntent)
      case SettingsIntent.Cursor(cursorIntent)           => interpretCursorIntent(cursorIntent)
      case SettingsIntent.TextDisplay(textDisplayIntent) => interpretTextDisplayIntent(textDisplayIntent)
      case SettingsIntent.InterfaceChrome(interfaceChromeIntent) =>
        interpretInterfaceChromeIntent(interfaceChromeIntent)
      case SettingsIntent.SpellCheck(spellCheckIntent) => interpretSpellCheckIntent(spellCheckIntent, state)
      case SettingsIntent.General(generalIntent)       => interpretGeneralSettingsIntent(generalIntent, state)

  private def interpretFontIntent(intent: FontIntent): IO[Unit] =
    intent match
      case FontIntent.IncreaseFontSize =>
        updateFontConfig(config =>
          config.copy(
            fontSize = clampFontSize(config.fontSize + 1.0f),
            textFontSize = clampFontSize(config.textFontSize + 1.0f)
          )
        )
      case FontIntent.DecreaseFontSize =>
        updateFontConfig(config =>
          config.copy(
            fontSize = clampFontSize(config.fontSize - 1.0f),
            textFontSize = clampFontSize(config.textFontSize - 1.0f)
          )
        )
      case FontIntent.SetFontSize(size) =>
        updateFontConfig(config => config.copy(fontSize = clampFontSize(size), textFontSize = clampFontSize(size)))
      case FontIntent.SetCodeFontSize(size) =>
        updateFontConfig(_.copy(fontSize = clampFontSize(size)))
      case FontIntent.SetTextFontSize(size) =>
        updateFontConfig(_.copy(textFontSize = clampFontSize(size)))
      case FontIntent.SetUiFontSize(size) =>
        updateFontConfig(_.copy(uiFontSize = clampFontSize(size)))
      case FontIntent.SetTextScaleMode(mode) =>
        updateFontConfig(_.copy(textScaleMode = mode))
      case FontIntent.SetTextScaleMultiplier(scale) =>
        updateFontConfig(config =>
          config.copy(
            textScaleMode = com.serenity.ui.fonts.FontLoader.TextScaleMode.Manual,
            textScaleMultiplier = com.serenity.ui.fonts.FontLoader.FontConfig.clampTextScale(scale)
          )
        )
      case FontIntent.SetCodeFontFamily(family) =>
        updateFontConfig(_.copy(codeFontFamily = family))
      case FontIntent.SetTextFontFamily(family) =>
        updateFontConfig(_.copy(textFontFamily = family))
      case FontIntent.SetUiFontFamily(family) =>
        updateFontConfig(_.copy(uiFontFamily = family))
      case FontIntent.SetLigatures(enabled) =>
        updateFontConfig(_.copy(enableLigatures = enabled, textLigatures = enabled))
      case FontIntent.SetCodeLigatures(enabled) =>
        updateFontConfig(_.copy(enableLigatures = enabled))
      case FontIntent.SetTextLigatures(enabled) =>
        updateFontConfig(_.copy(textLigatures = enabled))
      case FontIntent.SetUiLigatures(enabled) =>
        updateFontConfig(_.copy(uiLigatures = enabled))
      case FontIntent.ToggleLigatures =>
        updateFontConfig(config =>
          config.copy(enableLigatures = !config.enableLigatures, textLigatures = !config.textLigatures)
        )

  private def interpretCursorIntent(intent: CursorIntent): IO[Unit] =
    intent match
      case CursorIntent.SetCursorMode(mode) =>
        updateAppearanceConfig(_.withCursorMode(mode)).void

  private def interpretStatusLineIntent(intent: StatusLineIntent): IO[Unit] =
    intent match
      case StatusLineIntent.SetSegmentIncluded(segment, included) =>
        updateTextDisplayConfig { config =>
          val current = config.statusLine.segments
          config.withStatusLineSegments(
            if included then StatusSegment.include(current, segment) else current.filterNot(_ == segment)
          )
        }.void
      case StatusLineIntent.MoveSegmentEarlier(segment) =>
        updateTextDisplayConfig(config =>
          config.withStatusLineSegments(StatusSegment.move(config.statusLine.segments, segment, -1))
        ).void
      case StatusLineIntent.MoveSegmentLater(segment) =>
        updateTextDisplayConfig(config =>
          config.withStatusLineSegments(StatusSegment.move(config.statusLine.segments, segment, 1))
        ).void
      case StatusLineIntent.SetPlacement(placement) =>
        updateTextDisplayConfig(_.withStatusLinePlacement(placement)).void
      case StatusLineIntent.ToggleVisibility =>
        updateTextDisplayConfig { config =>
          val next =
            if config.statusLine.placement == StatusLinePlacement.Off then StatusLinePlacement.Pinned
            else StatusLinePlacement.Off
          config.withStatusLinePlacement(next)
        }.void
      case StatusLineIntent.SetWordGoal(goal) =>
        updateTextDisplayConfig(_.withWordGoal(goal)).void

  private def interpretTextDisplayIntent(intent: TextDisplayIntent): IO[Unit] =
    intent match
      case TextDisplayIntent.ToggleLineNumbers =>
        updateTextDisplayConfig(config => config.withLineNumbers(!config.surfaceConfig.showLineNumbers)).void
      case TextDisplayIntent.ToggleWordWrap =>
        updateTextDisplayConfig(config => config.withWordWrap(!config.surfaceConfig.wordWrapEnabled)).void
      case TextDisplayIntent.ToggleFocusedTextBody =>
        updateTextDisplayConfig(config => config.withFocusedTextBody(!config.surfaceConfig.focusedTextBodyEnabled)).void
      case TextDisplayIntent.ToggleContextualToolbar =>
        editor.enqueueEvent(com.serenity.keystroke.events.ToggleContextualToolbar)
      case TextDisplayIntent.TogglePaneHeaders =>
        updateTextDisplayConfig(config => config.withPaneHeaders(!config.surfaceConfig.showPaneHeaders)).void
      case TextDisplayIntent.ToggleVisualLineCursorNavigation =>
        updateTextDisplayConfig(config =>
          config.withVisualLineCursorNavigation(!config.surfaceConfig.visualLineCursorNavigation)
        ).void
      case TextDisplayIntent.ToggleTypewriterScrolling =>
        updateTextDisplayConfig(config =>
          config.withTypewriterScrolling(!config.surfaceConfig.typewriterScrollingEnabled)
        ).void
      case TextDisplayIntent.SetLineNumbers(enabled) =>
        updateTextDisplayConfig(config => config.withLineNumbers(enabled)).void
      case TextDisplayIntent.SetLineNumberSide(side) =>
        updateLineNumberLayout(_.copy(side = side))
      case TextDisplayIntent.SetLineNumberMarginLeft(cells) =>
        updateLineNumberLayout(_.copy(marginLeft = Some(cells)))
      case TextDisplayIntent.SetLineNumberMarginRight(cells) =>
        updateLineNumberLayout(_.copy(marginRight = cells))
      case TextDisplayIntent.SetLineNumberPadding(cells) =>
        updateLineNumberLayout(_.copy(padding = Some(cells)))
      case TextDisplayIntent.SetWordWrap(enabled) =>
        updateTextDisplayConfig(config => config.withWordWrap(enabled)).void
      case TextDisplayIntent.SetVisualLineCursorNavigation(enabled) =>
        updateTextDisplayConfig(config => config.withVisualLineCursorNavigation(enabled)).void
      case TextDisplayIntent.SetTypewriterScrolling(enabled) =>
        updateTextDisplayConfig(config => config.withTypewriterScrolling(enabled)).void
      case TextDisplayIntent.ToggleColumnMode =>
        updateTextDisplayConfig(config => config.withColumnMode(!config.surfaceConfig.columnModeEnabled)).void
      case TextDisplayIntent.SetColumnMode(enabled) =>
        updateTextDisplayConfig(config => config.withColumnMode(enabled)).void
      case TextDisplayIntent.SetColumnTargetWidth(cells) =>
        updateTextDisplayConfig(config => config.withColumnTargetWidth(cells)).void
      case TextDisplayIntent.SetColumnGap(cells) =>
        updateTextDisplayConfig(config => config.withColumnGap(cells)).void
      case TextDisplayIntent.SetColumnCount(count) =>
        updateTextDisplayConfig(config => config.withColumnCount(count)).void
      case TextDisplayIntent.SetFocusedTextBody(enabled) =>
        updateTextDisplayConfig(config => config.withFocusedTextBody(enabled)).void
      case TextDisplayIntent.SetContextualToolbarEnabled(enabled) =>
        updateTextDisplayConfig(config => config.withContextualToolbarEnabled(enabled)).void
      case TextDisplayIntent.SetContextualToolbarDisplayMode(mode) =>
        updateTextDisplayConfig(config => config.withContextualToolbarDisplayMode(mode)).void
      case TextDisplayIntent.SetTextAreaLeftInset(value) =>
        updateTextDisplayConfig(_.withTextAreaLeftInset(value)).void
      case TextDisplayIntent.SetTextAreaRightInset(value) =>
        updateTextDisplayConfig(_.withTextAreaRightInset(value)).void
      case TextDisplayIntent.SetTextAreaTopInset(value) =>
        updateTextDisplayConfig(_.withTextAreaTopInset(value)).void
      case TextDisplayIntent.SetTextAreaBottomInset(value) =>
        updateTextDisplayConfig(_.withTextAreaBottomInset(value)).void
      case TextDisplayIntent.SetDropCapsEnabled(enabled) =>
        updateTextDisplayConfig(_.withDropCapsEnabled(enabled)).void

  private def updateLineNumberLayout(update: LineNumberLayout => LineNumberLayout): IO[Unit] =
    updateTextDisplayConfig(config => config.withLineNumberLayout(update(config.lineNumberLayout))).void

  private def interpretInterfaceChromeIntent(intent: InterfaceChromeIntent): IO[Unit] =
    intent match
      case InterfaceChromeIntent.SetCommandRunnerShowKeyHints(enabled) =>
        updateAppearanceConfig(_.withCommandRunnerShowKeyHints(enabled)).void
      case InterfaceChromeIntent.SetUiElementGap(gap) =>
        updateAppearanceConfig(_.withUiElementGap(Some(gap))).void
      case InterfaceChromeIntent.SetUiOutlineThicknessPx(thickness) =>
        updateAppearanceConfig(_.withUiOutlineThicknessPx(thickness)).void
      case InterfaceChromeIntent.SetInterfaceDensity(density) =>
        updateAppearanceConfig(_.withInterfaceDensity(density)).void
      case InterfaceChromeIntent.SetWindowChromeMode(mode) =>
        updateAppearanceConfig(_.withWindowChromeMode(mode)).void
      case InterfaceChromeIntent.SetWheelScrollLines(lines) =>
        updateConfig(_.withWheelScrollLines(lines)).void
      case InterfaceChromeIntent.SetPanelEscapeTarget(mode, target) =>
        updateConfig(_.withPanelEscapeTarget(mode, target)).void

  private def interpretSpellCheckIntent(intent: SpellCheckIntent, state: AppState): IO[Unit] =
    intent match
      case SpellCheckIntent.SetSpellCheckEnabled(enabled) =>
        updateSpellCheckConfig(_.copy(enabled = enabled))
      case SpellCheckIntent.SetSpellCheckLanguages(languages) =>
        updateSpellCheckConfig(_.copy(languages = languages.map(SpellCheckLanguage.canonical)))
      case SpellCheckIntent.SetSpellCheckDictionaryPaths(paths) =>
        updateSpellCheckConfig(_.copy(dictionaryPaths = paths))
      case SpellCheckIntent.SetSpellCheckWords(words) =>
        updateSpellCheckConfig(_.copy(additionalWords = words))
      case SpellCheckIntent.AddWordAtCursorToDictionary =>
        addFlaggedWordAtCursorToDictionary(state)

  // #1531: silently a no-op when the cursor is not on a flagged word -- mirrors how `delete-document-comment`
  // is a no-op with no comment at the cursor, rather than surfacing an error for a command reachable from a
  // static context-menu/command-palette entry that doesn't know in advance whether it applies.
  private def addFlaggedWordAtCursorToDictionary(state: AppState): IO[Unit] =
    SpellChecker.flaggedWordAtCursor(state).traverse_(addWordToDictionary)

  /** Persists `word` in the configured custom words, which every later session loads. */
  private[manager] def addWordToDictionary(word: String): IO[Unit] =
    val normalized = DictionaryWord.normalize(word)
    updateSpellCheckConfig(config => config.copy(additionalWords = (config.additionalWords :+ normalized).distinct))

  /** Moves the config file aside first and goes no further if that fails, so a reset never overwrites the only copy. A
    * session with no config file (safe mode, whose settings are not the user's) has nothing to reset.
    */
  private def resetSettings: IO[Unit] =
    configPersistencePath.fold(logger.info("[CONFIG] This session has no config file, so there is nothing to reset")) {
      path =>
        IO.realTimeInstant
          .flatMap(now =>
            IO.blocking {
              val backup = TimestampedBackup.siblingOf(path, now)
              Option.when(Files.exists(path))(TimestampedBackup.moveAside(path, backup))
            }
          )
          .attempt
          .flatMap {
            case Left(error) =>
              logger.warn(error)("[CONFIG] Settings were not reset: the config file could not be backed up")
            case Right(backup) =>
              backup.traverse_(kept => logger.info(s"[CONFIG] Previous settings kept at $kept")) >>
                configFile.traverse_(_.forget) >> restoreDefaultSettings
          }
    }

  private def restoreDefaultSettings: IO[Unit] =
    deviceTextScaleProvider.flatMap { deviceTextScale =>
      val defaults = AppConfig.default
      applyConfigUpdate(current =>
        HotkeyOverrides.likeRunning(
          current,
          defaults.withFontConfig(defaults.editorConfig.fontConfig.resolveAutoTextScale(deviceTextScale))
        )
      ).flatMap(config => onFontConfigChanged(config.editorConfig.fontConfig))
    }

  private def interpretGeneralSettingsIntent(intent: GeneralSettingsIntent, state: AppState): IO[Unit] =
    intent match
      case GeneralSettingsIntent.OpenSettings =>
        currentState.flatMap { current =>
          val newState = CommandRunnerReducer.openSettings(current, CommandRegistry.withToggleUI)(using balance)
          editor.commitState(newState, current)
        }
      case GeneralSettingsIntent.SaveConfig =>
        persistConfigFile(state.committedConfig)
      case GeneralSettingsIntent.ResetSettings =>
        resetSettings
      case GeneralSettingsIntent.SetRenderFpsTarget(target) =>
        updateAppearanceConfig(_.withRenderFpsTarget(target)).void
      case GeneralSettingsIntent.SetRenderDamageGranularity(granularity) =>
        updateAppearanceConfig(_.withRenderDamageGranularity(granularity)).void
      case GeneralSettingsIntent.SetCommandRunnerVisibleRows(rows) =>
        updateAppearanceConfig(_.withCommandRunnerVisibleRows(rows)).void
      case GeneralSettingsIntent.SetCommandRunnerItemGapRows(rows) =>
        updateAppearanceConfig(_.withCommandRunnerItemGapRows(rows)).void
      case GeneralSettingsIntent.SetCommandRunnerCursorGapRows(rows) =>
        updateAppearanceConfig(_.withCommandRunnerCursorGapRows(rows)).void
      case GeneralSettingsIntent.SetAutoSaveMode(mode) =>
        applyConfigUpdate(_.withAutoSaveMode(mode)).void
      case GeneralSettingsIntent.SetAutoSaveDelayMillis(millis) =>
        applyConfigUpdate(_.withAutoSaveDelayMillis(millis)).void

  /** Queues a write of `config` to the config file, behind any config write already queued, whether or not it looks
    * changed.
    */
  private[manager] def persistConfigFile(config: AppConfig): IO[Unit] =
    configFile.traverse_(file =>
      editor.submitEffect(PersistenceLanes.Config, file.write(config).flatMap(_.fold(reportSaveFailure, _ => IO.unit)))
    )

  private def requestConfigWrite: IO[Unit] =
    writes.request(editor.submitEffect(PersistenceLanes.Config, _), writeLatestConfig)

  private def writeLatestConfig: IO[Unit] =
    configFile.traverse_(file =>
      currentState
        .flatMap(state => file.writeIfChanged(state.committedConfig))
        .flatMap(_.fold(reportSaveFailure, _ => IO.unit))
    )

  private def reportSaveFailure(error: ConfigError): IO[Unit] =
    logger.warn(error.cause.getOrElse(new RuntimeException(error.message)))(s"[CONFIG] ${error.message}")

  private def reloadFromDisk(file: ConfigFileSync): IO[Unit] =
    editor.submitEffect(
      PersistenceLanes.Config,
      file.externalChange
        .flatMap(_.traverse_(applyExternalChange(file, _)))
        .handleErrorWith(error => logger.warn(error)(s"[CONFIG] Reloading ${file.path} failed"))
    )

  private def applyExternalChange(file: ConfigFileSync, change: ConfigFileChange): IO[Unit] =
    change match
      case ConfigFileChange.Unreadable(error, stamp) =>
        file.noteSeen(stamp) >> showConfigNotice(List(ConfigNotice.unparseableOnReload(file.path, error)))
      case ConfigFileChange.Edited(loaded, stamp) =>
        (deviceTextScaleProvider, file.onDisk, currentState.map(_.persisted.config)).flatMapN { (scale, disk, live) =>
          val config   = loaded.config.withFontConfig(loaded.config.editorConfig.fontConfig.resolveAutoTextScale(scale))
          val unseen   = !disk.flatMap(_.stamp).contains(stamp)
          val problems = Option.when(unseen)(ConfigNotice.forLoad(file.path, loaded.report)).flatten.toList
          if disk.exists(_.encoded == ConfigManager.configToString(config)) then
            file.noteSeen(stamp) >> showConfigNotice(problems)
          else reloadInto(file, disk.map(_.config), live, config, stamp, problems)
        }

  private def reloadInto(
    file: ConfigFileSync,
    base: Option[AppConfig],
    before: AppConfig,
    config: AppConfig,
    stamp: FileStamp,
    problems: List[String]
  ): IO[Unit] =
    editor.dispatchEffectResult(EffectResult.ConfigReloaded(base, config), _ => IO.unit) >>
      currentState.map(_.persisted.config).flatMap { now =>
        if now == config then
          file.adopt(config, stamp) >> configApplied(before, config) >> logger.info(
            s"[CONFIG] Reloaded ${file.path}"
          ) >>
            showConfigNotice(problems ++ ConfigRestart.notice(ConfigRestart.changed(before, config)))
        else
          showConfigNotice(
            problems :+ s"${file.path.getFileName} was edited while a setting was being changed here, so the " +
              "settings from this session were kept."
          )
      }

  private def configApplied(before: AppConfig, config: AppConfig): IO[Unit] =
    IO(renderCaches.frameState.configureCacheCapacity(config.surfaceConfig.rendererFrameStateCacheCapacity)) >>
      IO.whenA(config.editorConfig.fontConfig != before.editorConfig.fontConfig)(
        onFontConfigChanged(config.editorConfig.fontConfig)
      ) >>
      IO.whenA(config.languageToolsConfig.spellCheck != before.languageToolsConfig.spellCheck)(
        editor.scheduleDocumentAnalysis()
      )

  private def showConfigNotice(messages: List[String]): IO[Unit] =
    IO.whenA(messages.nonEmpty)(
      showNotice(Notice(NoticeLevel.Warning, messages.mkString(" "), topic = Some(NoticeTopic.ConfigFile)))
    )

private[manager] object StateManagerConfigEffects:

  /** The whole state change of a config update: the new config, the live command runner and contextual toolbar
    * refreshed for it, and `syncState`.
    */
  def configTransition(
    model: Model,
    update: AppConfig => AppConfig,
    syncState: (AppState, AppConfig) => AppState
  ): Model =
    val config = update(model.app.persisted.config)
    model.copy(app = syncState(configUpdated(model.app, _ => config), config))

  /** `loaded` as the live config, unless it already is or a change made here since `base` would be undone by it.
    *
    * A preview is not a change made here: the file is compared with the config the preview began from. If it differs,
    * the preview ends and `loaded` replaces both, so that Escape has nothing stale to put back and the file's edit is
    * not lost. Re-running the previewed command against a config it was not chosen over would be a guess.
    */
  def reloaded(state: AppState, base: Option[AppConfig], loaded: AppConfig): AppState =
    state.runtime.pendingSetting match
      case Some(pending) =>
        if pending.committedConfig == loaded || base.exists(_ != pending.committedConfig) then state
        else
          val rebased = pending.copy(committedConfig = loaded)
          SettingsPreviewReducer.revert(state.copy(runtime = state.runtime.copy(pendingSetting = Some(rebased)))).state
      case None =>
        val live = state.persisted.config
        if live == loaded || base.exists(_ != live) then state else configUpdated(state, _ => loaded)

  def configUpdated(state: AppState, update: AppConfig => AppConfig): AppState =
    val config = update(state.persisted.config)
    withUpdatedRunnerConfig(state.copy(persisted = state.persisted.copy(config = config)), config)

  def withUpdatedRunnerConfig(state: AppState, config: AppConfig): AppState =
    val commandRunnerSurfaceId = state.commandRunnerSurface.map(_.id)
    val updatedRunner =
      state.commandRunnerSurface.flatMap { surface =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            Some(runner.updateInputItems(config))
          case _ =>
            None
      }
    val updatedSurfaces = state.runtime.uiSurfaces.map {
      // When updatedRunner is None, .fold falls back to the surface unchanged -- the same outcome the
      // original `if updatedRunner.isDefined` guard produced by skipping to the `case other => other` tail.
      case current if commandRunnerSurfaceId.contains(current.id) =>
        updatedRunner.fold(current)(runner => current.copy(content = SurfaceContent.CommandPalette(runner)))
      case current @ UiSurface(_, SurfaceContent.ContextualToolbar(toolbarState), _, _) =>
        current.copy(
          content = SurfaceContent.ContextualToolbar(
            toolbarState.copy(displayMode = config.surfaceConfig.contextualToolbarDisplayMode)
          )
        )
      case other =>
        other
    }
    state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))
