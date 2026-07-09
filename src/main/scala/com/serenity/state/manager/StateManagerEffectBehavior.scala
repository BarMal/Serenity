package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.animation.AnimationConfig
import com.serenity.command.*
import com.serenity.config.{DefaultDocumentMode, MarkdownViewMode}
import com.serenity.document.{CommentRendering, DocumentNavigation, DocumentOutline}
import com.serenity.io.{FileEntry, FileUtils}
import com.serenity.keystroke.events.ExplorerEvent
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.project.*
import com.serenity.richtext.*
import com.serenity.session.SessionSaveTrigger
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.text.TextEditing
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.config.{ThemeConfigWriter, ThemeCreatorState}

private[manager] trait StateManagerEffectBehavior extends StateManagerWorkflowBehavior:
  this: StateManager =>

  private val CommandRunnerSubmenuSurfaceId = SurfaceId("command-runner-submenu")
  private val UnsavedPresetCopySuffix       = " (modified, unsaved)"

  protected def interpretEffect(effect: AppEffect): IO[Unit] =
    effect match
      case AppEffect.Lifecycle(effect)      => interpretLifecycleEffect(effect)
      case AppEffect.CommandRequest(effect) => interpretCommandEffect(effect)
      case AppEffect.Theme(effect)          => interpretThemeEffect(effect)
      case AppEffect.Surface(effect)        => interpretSurfaceEffect(effect)
      case AppEffect.File(effect)           => interpretFileEffect(effect)
      case AppEffect.Explorer(effect)       => interpretExplorerEffect(effect)
      case AppEffect.Workflow(effect)       => interpretWorkflowEffect(effect)
      case AppEffect.LspQueue(effect)       => interpretLspQueueEffect(effect)

  private def interpretLifecycleEffect(effect: LifecycleEffect): IO[Unit] =
    effect match
      case LifecycleEffect.CompleteQuit =>
        quitSignal.complete(()).attempt.void

  private def interpretCommandEffect(effect: CommandEffect): IO[Unit] =
    effect match
      case CommandEffect.Execute(command) =>
        logger.info(s"[COMMAND] ${StateManager.describeCommandExecution(command)}") >>
          stateRef.get.flatMap(state => interpretCommand(command, state))

  private def interpretThemeEffect(effect: ThemeEffect): IO[Unit] =
    effect match
      case ThemeEffect.SwitchTheme(themeName) => applyThemeByName(themeName)
      case ThemeEffect.ReloadTheme(themeName) => reloadThemeByName(themeName)
      case ThemeEffect.SaveThemeConfig(config) =>
        ThemeConfigWriter
          .writeUserTheme(config)
          .flatTap(path => logger.info(s"[THEMES] Saved user theme '${config.name}' to $path"))
          .flatMap(_ => themeManager.listAvailableThemes.flatMap(themeNamesRef.set))
          .handleErrorWith(ex => logger.error(ex)(s"[THEMES] Failed to save user theme '${config.name}'"))

  private def interpretSurfaceEffect(effect: SurfaceEffect): IO[Unit] =
    effect match
      case SurfaceEffect.OpenThemePicker =>
        stateRef.get.flatMap(openThemePickerEffect)
      case SurfaceEffect.OpenThemeCreator =>
        stateRef.get.flatMap(openThemeCreatorEffect)
      case SurfaceEffect.OpenFileSearch =>
        stateRef.get.flatMap(openFileSearchEffect)

  private def interpretFileEffect(effect: FileEffect): IO[Unit] =
    effect match
      case FileEffect.SaveBuffer(bufferId)         => saveBufferEffect(bufferId)
      case FileEffect.SaveBufferAs(bufferId, path) => saveBufferAsEffect(bufferId, path)
      case FileEffect.DirectLoadFile(path)         => directLoadFileEffect(path)

  private def interpretExplorerEffect(effect: ExplorerEffect): IO[Unit] =
    effect match
      case ExplorerEffect.OpenRoot(position, path, size) =>
        pinExplorerPanelEffect(position, path, size)
      case ExplorerEffect.LoadDirectory(position, path) =>
        loadPinnedDirectoryEffect(position, path)

  private def interpretWorkflowEffect(effect: WorkflowEffect): IO[Unit] =
    effect match
      case WorkflowEffect.RequestOpenFile =>
        requestOpenFileDialog
      case WorkflowEffect.RequestSaveAs =>
        stateRef.get.flatMap(state => requestSaveAsFileDialog(state, state.focusedBufferId))
      case WorkflowEffect.RefreshFileWorkflow(surfaceId) =>
        refreshFileWorkflowEffect(surfaceId)
      case WorkflowEffect.SubmitFileWorkflow(surfaceId) =>
        submitFileWorkflowEffect(surfaceId)
      case WorkflowEffect.SubmitReplaceWorkflow(surfaceId) =>
        submitReplaceWorkflowEffect(surfaceId)
      case WorkflowEffect.SubmitCloseWorkflow(surfaceId) =>
        submitCloseWorkflowEffect(surfaceId)

  private def interpretLspQueueEffect(effect: LspQueueEffect): IO[Unit] =
    effect match
      case LspQueueEffect.Enqueue(effect) =>
        lspQueue.offer(effect)

  protected def withUpdatedRunnerConfig(state: AppState, config: com.serenity.config.AppConfig): AppState =
    val commandRunnerSurfaceId = state.commandRunnerSurface.map(_.id)
    val updatedRunner =
      state.commandRunnerSurface.flatMap { surface =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            val configRunner = runner.updateInputItems(config)
            Some(
              configRunner.copy(optionSelections =
                configRunner.optionSelections ++ CommandRunnerPanelSelections.fromState(state)
              )
            )
          case _ =>
            None
      }
    val updatedSurfaces = state.uiSurfaces.map {
      case current if updatedRunner.isDefined && commandRunnerSurfaceId.contains(current.id) =>
        current.copy(content = SurfaceContent.CommandPalette(updatedRunner.get))
      case current @ UiSurface(_, SurfaceContent.CommandPaletteSubmenu(_, groupId, previewOnly), _, _)
          if updatedRunner.isDefined =>
        current.copy(content = SurfaceContent.CommandPaletteSubmenu(updatedRunner.get, groupId, previewOnly))
      case current @ UiSurface(_, SurfaceContent.ContextualToolbar(toolbarState), _, _) =>
        current.copy(
          content =
            SurfaceContent.ContextualToolbar(toolbarState.copy(displayMode = config.contextualToolbarDisplayMode))
        )
      case other =>
        other
    }
    state.copy(uiSurfaces = updatedSurfaces)

  protected def updateConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    updateConfigWithEditedPresetDraft(update, markEditedUiPresetDraftFromCommandRunner)

  private def updateAppearanceConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    updateConfigWithEditedPresetDraft(
      update,
      markEditedUiPresetDraftFromCommandRunner
    )

  private def updateDocumentDefaultsConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    updateConfigWithEditedPresetDraft(
      update,
      markEditedUiPresetDraftFromCommandRunner
    )

  private def updateMotionConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    updateConfigWithEditedPresetDraft(
      update,
      markEditedUiPresetDraftFromCommandRunner
    )

  private def updateCustomMotionConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    updateMotionConfig(config => update(config).copy(motionPreset = com.serenity.config.MotionPreset.Custom))

  private def updateTextDisplayConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    updateConfigWithEditedPresetDraft(
      update,
      markEditedUiPresetDraftFromCommandRunner
    )

  private def updateConfigWithEditedPresetDraft(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig,
    markEditedPreset: IO[Unit]
  ): IO[com.serenity.config.AppConfig] =
    stateRef
      .modify { state =>
        val newConfig = update(state.config)
        val newState  = withUpdatedRunnerConfig(state.copy(config = newConfig), newConfig)
        (newState, newConfig)
      }
      .flatTap(config =>
        configPersistencePath match
          case Some(path) =>
            IO.blocking(com.serenity.config.ConfigManager.saveConfig(config, path)).flatMap {
              case true  => IO.unit
              case false => logger.warn(s"[CONFIG] Failed to persist config to $path")
            }
          case None =>
            IO.unit
      )
      .flatTap(_ => markEditedPreset)
      .flatTap(_ =>
        stateRef.get
          .flatMap(state => sessionPersistence.maybeSaveSession(state, SessionSaveTrigger.Manual))
          .handleErrorWith(error => logger.error(error)("[SESSION] Auto-save after config change failed"))
      )

  private def markEditedUiPresetDraftFromCommandRunner: IO[Unit] =
    stateRef.update { state =>
      editingUiPresetName(state) match
        case Some(presetName) =>
          val sourceName = sourcePresetName(presetName)
          updateCommandRunnerPresetContextInState(
            state,
            Some(unsavedPresetCopyName(sourceName)),
            s"Editing unsaved copy of $sourceName. Save the preset to preserve it."
          )
        case None =>
          state
    }

  private def editingUiPresetName(state: AppState): Option[String] =
    state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) =>
          runner.editingPresetName.map(_.trim).filter(_.nonEmpty)
        case _ =>
          None
    }

  private def unsavedPresetCopyName(name: String): String =
    val trimmed = name.trim
    if trimmed.endsWith(UnsavedPresetCopySuffix) then trimmed
    else s"$trimmed$UnsavedPresetCopySuffix"

  private def sourcePresetName(name: String): String =
    val trimmed = name.trim
    if trimmed.endsWith(UnsavedPresetCopySuffix) then trimmed.dropRight(UnsavedPresetCopySuffix.length).trim
    else trimmed

  protected def updateFontConfig(
    update: com.serenity.ui.fonts.FontLoader.FontConfig => com.serenity.ui.fonts.FontLoader.FontConfig
  ): IO[Unit] =
    updateConfigWithEditedPresetDraft(
      config => config.withFontConfig(update(config.fontConfig)),
      markEditedUiPresetDraftFromCommandRunner
    )
      .flatMap(config => onFontConfigChanged(config.fontConfig))

  protected def updateSpellCheckConfig(
    update: com.serenity.config.SpellCheckConfig => com.serenity.config.SpellCheckConfig
  ): IO[Unit] =
    updateConfigWithEditedPresetDraft(
      config => config.withSpellCheck(update(config.spellCheck)),
      markEditedUiPresetDraftFromCommandRunner
    ).void >>
      scheduleDocumentAnalysis()

  protected def clampFontSize(size: Float): Float =
    size.max(8.0f).min(48.0f)

  protected def interpretCommand(command: Command, state: AppState): IO[Unit] =
    command.intent match
      case CommandIntent.ToggleLineNumbers =>
        updateTextDisplayConfig(config => config.withLineNumbers(!config.showLineNumbers)).void
      case CommandIntent.ToggleGutter =>
        updateTextDisplayConfig(config => config.withGutter(!config.showGutter)).void
      case CommandIntent.ToggleWordWrap =>
        updateTextDisplayConfig(config => config.withWordWrap(!config.wordWrapEnabled)).void
      case CommandIntent.ToggleFocusedTextBody =>
        updateTextDisplayConfig(config => config.withFocusedTextBody(!config.focusedTextBodyEnabled)).void
      case CommandIntent.ToggleContextualToolbar =>
        applyEvent(com.serenity.keystroke.events.ToggleContextualToolbar)
      case CommandIntent.SetLineNumbers(enabled) =>
        updateTextDisplayConfig(config => config.withLineNumbers(enabled)).void
      case CommandIntent.SetGutter(enabled) =>
        updateTextDisplayConfig(config => config.withGutter(enabled)).void
      case CommandIntent.SetWordWrap(enabled) =>
        updateTextDisplayConfig(config => config.withWordWrap(enabled)).void
      case CommandIntent.SetFocusedTextBody(enabled) =>
        updateTextDisplayConfig(config => config.withFocusedTextBody(enabled)).void
      case CommandIntent.SetContextualToolbarEnabled(enabled) =>
        updateTextDisplayConfig(config => config.withContextualToolbarEnabled(enabled)).void
      case CommandIntent.SetContextualToolbarDisplayMode(mode) =>
        updateTextDisplayConfig(config => config.withContextualToolbarDisplayMode(mode)).void
      case CommandIntent.SaveCurrentFile =>
        state.focusedBufferId match
          case Some(bufferId) => saveBufferEffect(bufferId)
          case None           => logger.debug("[CMD] No focused buffer to save")
      case CommandIntent.SaveCurrentFileAs =>
        requestSaveAsFileDialog(state, state.focusedBufferId)
      case CommandIntent.SaveConfig =>
        persistConfigFile(state.config)
      case CommandIntent.SaveSession =>
        saveSession()
      case CommandIntent.RestoreSession =>
        loadSession().flatMap {
          case Some(restored) => validateAndUpdateState(restoreSessionIntoCurrentViewport(restored, state), state)
          case None           => logger.debug("[SESSION] Restore requested without a saved session")
        }
      case CommandIntent.ClearSession =>
        clearSession()
      case CommandIntent.OpenFile =>
        requestOpenFileDialog
      case CommandIntent.ExportCurrentTheme =>
        exportCurrentThemeEffect(state)
      case CommandIntent.QuitApp =>
        beginCloseAction(CloseScope.Quit, state)
      case CommandIntent.CloseAll =>
        beginCloseAction(CloseScope.All, state)
      case CommandIntent.CloseOthers =>
        beginCloseAction(CloseScope.Others, state)
      case CommandIntent.NewFile =>
        val registry = CommandRegistry.withToggleUI
        updateState(current =>
          AppEventReducer.reduce(com.serenity.keystroke.events.NewTab, current, registry)(using balance).state
        )
      case CommandIntent.CloseCurrentFile =>
        beginCloseAction(CloseScope.Current, state)
      case CommandIntent.FindInCurrentFile =>
        updateState(current => ModalStateReducer.show(findModalForState(current), current).state)
      case CommandIntent.FindAllInCurrentFile =>
        updateState(current => ModalStateReducer.show(findModalForState(current), current).state)
      case CommandIntent.ReplaceInCurrentFile =>
        updateState(current => ModalStateReducer.show(Modal.ReplaceWorkflow(ReplaceWorkflowState()), current).state)
      case CommandIntent.ReplaceAllInCurrentFile =>
        updateState(current =>
          ModalStateReducer
            .show(
              Modal.ReplaceWorkflow(ReplaceWorkflowState(selectedAction = ReplaceWorkflowAction.ReplaceAll)),
              current
            )
            .state
        )
      case CommandIntent.Copy =>
        applyEvent(com.serenity.keystroke.events.Copy)
      case CommandIntent.Cut =>
        applyEvent(com.serenity.keystroke.events.Cut)
      case CommandIntent.Paste =>
        applyEvent(com.serenity.keystroke.events.Paste)
      case CommandIntent.SelectAll =>
        applyEvent(com.serenity.keystroke.events.SelectAll)
      case CommandIntent.ToggleRichTextMark(mark) =>
        updateState(current => toggleRichTextMark(current, mark))
      case CommandIntent.SetRichTextFontFamily(family) =>
        updateState(current => setRichTextFontFamily(current, family))
      case CommandIntent.SetRichTextFontSize(size) =>
        updateState(current => setRichTextFontSize(current, size))
      case CommandIntent.SetRichTextColor(color) =>
        updateState(current => setRichTextColor(current, color))
      case CommandIntent.SetRichTextParagraphRole(role) =>
        updateState(current => setRichTextParagraphRole(current, role))
      case CommandIntent.SetRichTextParagraphAlignment(alignment) =>
        updateState(current => setRichTextParagraphAlignment(current, alignment))
      case CommandIntent.ToggleCommentLens =>
        toggleCommentLens(state)
      case CommandIntent.AddDocumentComment(text) =>
        addDocumentComment(state, text)
      case CommandIntent.DeleteDocumentComment =>
        deleteDocumentComment(state)
      case CommandIntent.OpenGotoLine =>
        updateState(current => ModalStateReducer.show(Modal.GotoLine(""), current).state)
      case CommandIntent.ToggleBookmark =>
        toggleBookmark(state)
      case CommandIntent.NextBookmark =>
        navigateBookmark(state, DocumentNavigation.nextSymbol)
      case CommandIntent.PreviousBookmark =>
        navigateBookmark(state, DocumentNavigation.previousSymbol)
      case CommandIntent.NextDocumentComment =>
        navigateDocumentComment(state, DocumentNavigation.nextSymbol)
      case CommandIntent.PreviousDocumentComment =>
        navigateDocumentComment(state, DocumentNavigation.previousSymbol)
      case CommandIntent.NextDocumentSymbol =>
        navigateDocumentSymbol(state, DocumentNavigation.nextSymbol)
      case CommandIntent.PreviousDocumentSymbol =>
        navigateDocumentSymbol(state, DocumentNavigation.previousSymbol)
      case CommandIntent.NavigateBack =>
        navigateHistoryBack()
      case CommandIntent.NavigateForward =>
        navigateHistoryForward()
      case CommandIntent.RequestLspHover =>
        requestLspHover(state)
      case CommandIntent.RequestLspDefinition =>
        requestLspDefinition(state)
      case CommandIntent.ToggleTheme =>
        toggleThemeEffect(state)
      case CommandIntent.ReloadTheme =>
        reloadThemeEffect(state)
      case CommandIntent.OpenThemeChooser =>
        openThemePickerEffect(state)
      case CommandIntent.OpenThemeCreator =>
        openThemeCreatorEffect(state)
      case CommandIntent.ReloadThemes =>
        themeManager.listAvailableThemes
          .flatMap(themeNamesRef.set)
          .handleErrorWith(ex => logger.error(ex)("[THEMES] Failed to reload theme list"))
      case CommandIntent.PinExplorerPanel =>
        setPanelPin(PanelKind.Explorer, Some(PanelPosition.Left))
      case CommandIntent.PinOutlinePanel =>
        setPanelPin(PanelKind.Outline, Some(PanelPosition.Right))
      case CommandIntent.PinDiagnosticsPanel =>
        setPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Bottom))
      case CommandIntent.OpenMarkdownPreview =>
        setPanelPin(PanelKind.MarkdownPreview, Some(PanelPosition.Right))
      case CommandIntent.SetPanelPin(kind, position) =>
        setPanelPin(kind, position)
      case CommandIntent.MovePanelEarlier(kind) =>
        movePanelKind(kind, delta = -1)
      case CommandIntent.MovePanelLater(kind) =>
        movePanelKind(kind, delta = 1)
      case CommandIntent.SetMarkdownViewMode(mode) =>
        setMarkdownViewMode(mode)
      case CommandIntent.SetDefaultDocumentMode(mode) =>
        updateDocumentDefaultsConfig(_.withDefaultDocumentMode(mode)).void
      case CommandIntent.SetSpellCheckEnabled(enabled) =>
        updateSpellCheckConfig(_.copy(enabled = enabled))
      case CommandIntent.SetSpellCheckLanguages(languages) =>
        updateSpellCheckConfig(_.copy(languages = languages))
      case CommandIntent.SetSpellCheckDictionaryPaths(paths) =>
        updateSpellCheckConfig(_.copy(dictionaryPaths = paths))
      case CommandIntent.SetSpellCheckWords(words) =>
        updateSpellCheckConfig(_.copy(additionalWords = words))
      case CommandIntent.SetInterfaceDensity(density) =>
        updateAppearanceConfig(_.withInterfaceDensity(density)).void
      case CommandIntent.FocusPanel(position) =>
        switchToPinnedPanel(position)
      case CommandIntent.UnpinPanel(position) =>
        unpinPanel(position) >> markEditedUiPresetDraftFromCommandRunner
      case CommandIntent.ExpandPanel(position) =>
        expandPinnedPanel(position)
      case CommandIntent.CollapseExpandedPanel =>
        collapseExpandedPanel()
      case CommandIntent.FormatCurrentFile =>
        logger.debug("[CMD] Format command requested")
      case CommandIntent.SetAnimationMode(mode) =>
        updateMotionConfig { config =>
          mode match
            case AnimationMode.None   => config.withoutCharacterAnimation
            case AnimationMode.Quick  => config.withMotionPreset(com.serenity.config.MotionPreset.Expressive)
            case AnimationMode.Smooth => config.withMotionPreset(com.serenity.config.MotionPreset.Smooth)
            case AnimationMode.Subtle => config.withMotionPreset(com.serenity.config.MotionPreset.Subtle)
        }.void
      case CommandIntent.SetMaterialPreset(preset) =>
        updateAppearanceConfig(_.withMaterialPreset(preset)).void
      case CommandIntent.SetMotionPreset(preset) =>
        updateMotionConfig(_.withMotionPreset(preset)).void
      case CommandIntent.SetElementTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withElementTransitionSpeedScale(scale)).void
      case CommandIntent.SetEditorTextTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withEditorTextTransitionSpeedScale(Some(scale))).void
      case CommandIntent.SetCommandRunnerTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withCommandRunnerTransitionSpeedScale(Some(scale))).void
      case CommandIntent.SetUiTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withUiTransitionSpeedScale(Some(scale))).void
      case CommandIntent.SetCursorTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withCursorTransitionSpeedScale(Some(scale))).void
      case CommandIntent.SetCommandRunnerAnimation(animation) =>
        updateCustomMotionConfig(_.withCommandRunnerAnimation(animation)).void
      case CommandIntent.SetUiAnimation(animation) =>
        updateCustomMotionConfig(_.withUiAnimation(animation)).void
      case CommandIntent.SetCommandRunnerVisibleRows(rows) =>
        updateAppearanceConfig(_.withCommandRunnerVisibleRows(rows)).void
      case CommandIntent.SetRenderFpsTarget(target) =>
        updateAppearanceConfig(_.withRenderFpsTarget(target)).void
      case CommandIntent.SetEditorInsertionTransitionKind(kind) =>
        updateCustomMotionConfig(_.withEditorInsertionTransitionKind(kind)).void
      case CommandIntent.SetCommandRunnerTransitionKind(kind) =>
        updateCustomMotionConfig(_.withCommandRunnerTransitionKind(Some(kind))).void
      case CommandIntent.SetPanelOpenTransitionKind(kind) =>
        updateCustomMotionConfig(_.withPanelOpenTransitionKind(Some(kind))).void
      case CommandIntent.SetPanelCloseTransitionKind(kind) =>
        updateCustomMotionConfig(_.withPanelCloseTransitionKind(Some(kind))).void
      case CommandIntent.SetBackgroundStyle(style) =>
        updateAppearanceConfig(_.withBackgroundStyle(style)).void
      case CommandIntent.SetBlurRadius(r) =>
        updateAppearanceConfig(_.withBlurRadius(r)).void
      case CommandIntent.SetAnimationDuration(ms) =>
        updateMotionConfig { config =>
          val newAnim =
            if ms <= 0 then None
            else
              Some(
                config.characterAnimation.fold(
                  AnimationConfig(
                    steps = 12,
                    totalDuration = scala.concurrent.duration.Duration.fromNanos(ms * 1_000_000L)
                  )
                )(existing =>
                  existing.copy(totalDuration = scala.concurrent.duration.Duration.fromNanos(ms * 1_000_000L))
                )
              )
          config.copy(characterAnimation = newAnim, motionPreset = com.serenity.config.MotionPreset.Custom)
        }.void
      case CommandIntent.SetAnimationSteps(n) =>
        updateMotionConfig { config =>
          val newAnim =
            if n <= 0 then None
            else
              Some(
                config.characterAnimation.fold(
                  AnimationConfig(
                    steps = n,
                    totalDuration = scala.concurrent.duration.Duration.fromNanos(200_000_000L)
                  )
                )(existing => existing.copy(steps = n))
              )
          config.copy(characterAnimation = newAnim, motionPreset = com.serenity.config.MotionPreset.Custom)
        }.void
      case CommandIntent.SetCursorMode(mode) =>
        updateAppearanceConfig(_.withCursorMode(mode)).void
      case CommandIntent.SetCursorInfoBarMode(mode) =>
        updateAppearanceConfig(_.withCursorInfoBarMode(mode)).void
      case CommandIntent.SetCursorInfoBarPlacement(placement) =>
        updateAppearanceConfig(_.withCursorInfoBarPlacement(placement)).void
      case CommandIntent.SetUiElementGap(gap) =>
        updateAppearanceConfig(_.withUiElementGap(gap)).void
      case CommandIntent.SetUiCornerRadiusPx(radius) =>
        updateAppearanceConfig(_.withUiCornerRadiusPx(radius)).void
      case CommandIntent.SetUiOutlineThicknessPx(thickness) =>
        updateAppearanceConfig(_.withUiOutlineThicknessPx(thickness)).void
      case CommandIntent.IncreaseFontSize =>
        updateFontConfig(config =>
          config.copy(
            fontSize = clampFontSize(config.fontSize + 1.0f),
            textFontSize = clampFontSize(config.textFontSize + 1.0f)
          )
        )
      case CommandIntent.DecreaseFontSize =>
        updateFontConfig(config =>
          config.copy(
            fontSize = clampFontSize(config.fontSize - 1.0f),
            textFontSize = clampFontSize(config.textFontSize - 1.0f)
          )
        )
      case CommandIntent.SetFontSize(size) =>
        updateFontConfig(config => config.copy(fontSize = clampFontSize(size), textFontSize = clampFontSize(size)))
      case CommandIntent.SetCodeFontSize(size) =>
        updateFontConfig(_.copy(fontSize = clampFontSize(size)))
      case CommandIntent.SetTextFontSize(size) =>
        updateFontConfig(_.copy(textFontSize = clampFontSize(size)))
      case CommandIntent.SetUiFontSize(size) =>
        updateFontConfig(_.copy(uiFontSize = clampFontSize(size)))
      case CommandIntent.SetTextScaleMode(mode) =>
        updateFontConfig(config => config.copy(textScaleMode = mode).resolveAutoTextScale(config.textScaleMultiplier))
      case CommandIntent.SetTextScaleMultiplier(scale) =>
        updateFontConfig(config =>
          config.copy(
            textScaleMode = com.serenity.ui.fonts.FontLoader.TextScaleMode.Manual,
            textScaleMultiplier = com.serenity.ui.fonts.FontLoader.FontConfig.clampTextScale(scale)
          )
        )
      case CommandIntent.SetCodeFontFamily(family) =>
        updateFontConfig(_.copy(codeFontFamily = family))
      case CommandIntent.SetTextFontFamily(family) =>
        updateFontConfig(_.copy(textFontFamily = family))
      case CommandIntent.SetUiFontFamily(family) =>
        updateFontConfig(_.copy(uiFontFamily = family))
      case CommandIntent.SetLigatures(enabled) =>
        updateFontConfig(_.copy(enableLigatures = enabled, textLigatures = enabled))
      case CommandIntent.SetCodeLigatures(enabled) =>
        updateFontConfig(_.copy(enableLigatures = enabled))
      case CommandIntent.SetTextLigatures(enabled) =>
        updateFontConfig(_.copy(textLigatures = enabled))
      case CommandIntent.SetUiLigatures(enabled) =>
        updateFontConfig(_.copy(uiLigatures = enabled))
      case CommandIntent.SaveUiPreset(name) if command.name == "ui-preset-create" =>
        saveUiPresetEffect(name) >> focusCreatedPresetOptions(name)
      case CommandIntent.SaveUiPreset(name) =>
        saveUiPresetEffect(name)
      case CommandIntent.ApplyUiPreset(name) =>
        applyUiPresetEffect(name)
      case CommandIntent.DuplicateUiPreset(sourceName, targetName) =>
        duplicateUiPresetEffect(sourceName, targetName)
      case CommandIntent.RenameUiPreset(sourceName, targetName) =>
        renameUiPresetEffect(sourceName, targetName)
      case CommandIntent.DeleteUiPreset(name) =>
        deleteUiPresetEffect(name)
      case CommandIntent.ResetUiPreset(name) =>
        resetUiPresetEffect(name)
      case CommandIntent.SetTextAreaLeftInset(value) =>
        updateTextDisplayConfig(_.withTextAreaLeftInset(value)).void
      case CommandIntent.SetTextAreaRightInset(value) =>
        updateTextDisplayConfig(_.withTextAreaRightInset(value)).void
      case CommandIntent.SetTextAreaTopInset(value) =>
        updateTextDisplayConfig(_.withTextAreaTopInset(value)).void
      case CommandIntent.SetTextAreaBottomInset(value) =>
        updateTextDisplayConfig(_.withTextAreaBottomInset(value)).void
      case CommandIntent.RunProjectTask(kind) =>
        runProjectTask(state, kind)
      case CommandIntent.ToggleLigatures =>
        updateFontConfig(config =>
          config.copy(enableLigatures = !config.enableLigatures, textLigatures = !config.textLigatures)
        )
      case CommandIntent.StartupNewSession =>
        createStartupSession()
      case CommandIntent.StartupRestoreSession =>
        restoreStartupSession()
      case CommandIntent.StartupOpenFile =>
        requestOpenFileDialog
      case CommandIntent.SetBufferLanguage(language) =>
        state.focusedBufferId match
          case Some(bufferId) =>
            state.buffers.get(bufferId) match
              case Some(buffer) =>
                val updateLanguage =
                  updateState(s => s.copy(buffers = s.buffers + (bufferId -> buffer.copy(language = language))))

                val refreshLspBinding =
                  buffer.filePath match
                    case Some(path) if buffer.language != language =>
                      val uri  = path.toUri.toString
                      val text = buffer.content.collect()
                      val closeOld =
                        buffer.language.fold(IO.unit)(previous => lspQueue.offer(LspEffect.FileClosed(uri, previous)))
                      val openNew =
                        language.fold(IO.unit)(next => lspQueue.offer(LspEffect.FileOpened(uri, next, text)))
                      closeOld >> openNew
                    case _ =>
                      IO.unit

                updateLanguage >> refreshLspBinding
              case None =>
                IO.unit
          case None => IO.unit
      case CommandIntent.SetGlobalHotkey(action, binding) =>
        updateConfig(_.withHotkeyOverride(action, binding)).void
      case CommandIntent.SetEditorKeyBinding(action, binding) =>
        updateConfig(_.withEditorKeyOverride(action, binding)).void
      case CommandIntent.SetCommandRunnerKeyBinding(action, binding) =>
        updateConfig(_.withCommandRunnerKeyOverride(action, binding)).void
      case CommandIntent.SetModalKeyBinding(action, binding) =>
        updateConfig(_.withModalKeyOverride(action, binding)).void
      case CommandIntent.SetPanelKeyBinding(action, binding) =>
        updateConfig(_.withPanelKeyOverride(action, binding)).void
      case CommandIntent.SetPeekKeyBinding(action, binding) =>
        updateConfig(_.withPeekKeyOverride(action, binding)).void
      case CommandIntent.ResetGlobalHotkey(action) =>
        updateConfig(_.resetHotkeyOverride(action)).void
      case CommandIntent.ResetEditorKeyBinding(action) =>
        updateConfig(_.resetEditorKeyOverride(action)).void
      case CommandIntent.ResetCommandRunnerKeyBinding(action) =>
        updateConfig(_.resetCommandRunnerKeyOverride(action)).void
      case CommandIntent.ResetModalKeyBinding(action) =>
        updateConfig(_.resetModalKeyOverride(action)).void
      case CommandIntent.ResetPanelKeyBinding(action) =>
        updateConfig(_.resetPanelKeyOverride(action)).void
      case CommandIntent.ResetPeekKeyBinding(action) =>
        updateConfig(_.resetPeekKeyOverride(action)).void

  private def toggleRichTextMark(
    state: AppState,
    mark: com.serenity.richtext.InlineMark
  ): AppState =
    activeEditorContentBuffer(state) match
      case Some(buffer) =>
        val selections = buffer.allSelections.filter(selection => selection.start != selection.end)
        if selections.isEmpty then state
        else
          val text = buffer.content.collect()
          val baseDocument = buffer.richTextDocument
            .filter(_.matchesPlainText(text))
            .getOrElse(com.serenity.richtext.RichTextDocument.fromPlainText(text))
          val updatedDocument = selections
            .foldLeft(baseDocument)((document, selection) => document.toggleMark(richTextRange(selection), mark))
            .normalized
          state.copy(
            buffers = state.buffers.updated(
              buffer.id,
              buffer.copy(
                isDirty = true,
                isNewEmpty = false,
                richTextDocument = Some(updatedDocument)
              )
            )
          )
      case None =>
        state

  private def setRichTextParagraphRole(state: AppState, role: ParagraphRole): AppState =
    updateRichTextParagraphs(state)((document, range) => document.setParagraphRole(range, role))

  private def setRichTextParagraphAlignment(state: AppState, alignment: ParagraphAlignment): AppState =
    updateRichTextParagraphs(state)((document, range) => document.setParagraphAlignment(range, alignment))

  private def setRichTextFontFamily(state: AppState, family: String): AppState =
    updateRichTextInlineStyles(state)((document, range) => document.setFontFamily(range, family))

  private def setRichTextFontSize(state: AppState, size: Float): AppState =
    updateRichTextInlineStyles(state)((document, range) => document.setFontSize(range, size))

  private def setRichTextColor(state: AppState, color: String): AppState =
    updateRichTextInlineStyles(state)((document, range) => document.setColor(range, color))

  private def updateRichTextInlineStyles(
    state: AppState
  )(update: (RichTextDocument, RichTextRange) => RichTextDocument): AppState =
    activeEditorContentBuffer(state) match
      case Some(buffer) =>
        val ranges = buffer.allSelections.filter(selection => selection.start != selection.end).map(richTextRange)
        if ranges.isEmpty then state
        else
          val text = buffer.content.collect()
          val baseDocument = buffer.richTextDocument
            .filter(_.matchesPlainText(text))
            .getOrElse(RichTextDocument.fromPlainText(text))
          val updatedDocument = ranges.foldLeft(baseDocument)(update).normalized
          if updatedDocument == baseDocument.normalized then state
          else
            state.copy(
              buffers = state.buffers.updated(
                buffer.id,
                buffer.copy(
                  isDirty = true,
                  isNewEmpty = false,
                  richTextDocument = Some(updatedDocument)
                )
              )
            )
      case None =>
        state

  private def updateRichTextParagraphs(
    state: AppState
  )(update: (RichTextDocument, RichTextRange) => RichTextDocument): AppState =
    activeEditorContentBuffer(state) match
      case Some(buffer) =>
        val ranges = richTextParagraphRanges(buffer)
        if ranges.isEmpty then state
        else
          val text = buffer.content.collect()
          val baseDocument = buffer.richTextDocument
            .filter(_.matchesPlainText(text))
            .getOrElse(RichTextDocument.fromPlainText(text))
          val updatedDocument = ranges.foldLeft(baseDocument)(update).normalized
          if updatedDocument == baseDocument.normalized then state
          else
            state.copy(
              buffers = state.buffers.updated(
                buffer.id,
                buffer.copy(
                  isDirty = true,
                  isNewEmpty = false,
                  richTextDocument = Some(updatedDocument)
                )
              )
            )
      case None =>
        state

  private def richTextParagraphRanges(buffer: Buffer): List[RichTextRange] =
    val selections = buffer.allSelections.filter(selection => selection.start != selection.end).map(richTextRange)
    if selections.nonEmpty then selections
    else
      buffer.cursors.distinct.map { cursor =>
        RichTextRange(
          start = com.serenity.richtext.RichTextPosition(cursor.line, cursor.column),
          end = com.serenity.richtext.RichTextPosition(cursor.line, cursor.column)
        )
      }

  private def activeEditorContentBuffer(state: AppState): Option[Buffer] =
    state.layout.activeEditorPaneId
      .flatMap(state.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(state.buffers.get)

  private def richTextRange(selection: Selection): com.serenity.richtext.RichTextRange =
    com.serenity.richtext.RichTextRange(
      start = com.serenity.richtext.RichTextPosition(selection.start.line, selection.start.column),
      end = com.serenity.richtext.RichTextPosition(selection.end.line, selection.end.column)
    )

  protected def saveUiPresetEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")
      case Some(presetName) =>
        for
          state <- stateRef.get
          windowSize <- windowSizeProvider.handleErrorWith(error =>
            logger.error(error)("[PRESET] Window size capture failed").as(None)
          )
          preset = UiPreset.capture(presetName, state, windowSize)
          _ <- uiPresetStore
            .upsert(preset)
            .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to save UI preset $presetName"))
          _ <- refreshCommandRunnerUiPresetPreviews
          _ <- updateCommandRunnerPresetContext(Some(presetName), s"Preset saved. Configure $presetName.")
        yield ()

  protected def applyUiPresetEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")
      case Some(presetName) =>
        uiPresetStore
          .find(presetName)
          .map(_.orElse(UiPreset.builtIn(presetName)))
          .flatMap {
            case None =>
              logger.warn(s"[PRESET] UI preset not found: $presetName")
            case Some(preset) =>
              for
                currentState <- stateRef.get
                theme <- themeManager
                  .loadTheme(preset.themeName)
                  .handleErrorWith(error =>
                    logger.error(error)(s"[PRESET] Failed to load theme ${preset.themeName}; keeping current theme") >>
                      IO.pure(currentState.theme)
                  )
                _ <- stateRef.modify { state =>
                  val restoredPresetState = UiPreset.applyToState(preset, state, theme)
                  val restoredDocumentState =
                    applyPresetDocumentModeToActiveEmptyBuffer(restoredPresetState, preset.config.defaultDocumentMode)
                  val restoredOutlineState = hydratePresetOutlinePanels(restoredDocumentState)
                  val restored             = withUpdatedRunnerConfig(restoredOutlineState, preset.config)
                  (restored, ())
                }
                _ <- persistConfigFile(preset.config)
                _ <- onFontConfigChanged(preset.config.fontConfig)
                  .handleErrorWith(error => logger.error(error)("[PRESET] Failed to apply preset font config"))
                _ <- reloadPresetDirectories(preset)
                _ <- openPresetMarkdownPreviewIfNeeded(preset)
                _ <- stateRef.get
                  .flatMap(state => sessionPersistence.maybeSaveSession(state, SessionSaveTrigger.Manual))
                  .handleErrorWith(error => logger.error(error)("[SESSION] Auto-save after preset apply failed"))
              yield ()
          }
          .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to apply UI preset $presetName"))

  private def applyPresetDocumentModeToActiveEmptyBuffer(state: AppState, mode: DefaultDocumentMode): AppState =
    state.focusedBufferId.flatMap(state.buffers.get) match
      case Some(buffer) if buffer.isNewEmpty && buffer.content.weight == 0 && buffer.filePath.isEmpty =>
        val updatedBuffer =
          mode match
            case DefaultDocumentMode.PlainText =>
              buffer.copy(language = None, richTextDocument = None)
            case DefaultDocumentMode.Markdown =>
              buffer.copy(language = Some(LanguageId.Markdown), richTextDocument = None)
            case DefaultDocumentMode.RichText =>
              buffer.copy(language = None, richTextDocument = Some(RichTextDocument.fromPlainText("")))
        state.copy(buffers = state.buffers + (buffer.id -> updatedBuffer))
      case _ =>
        state

  private def hydratePresetOutlinePanels(state: AppState): AppState =
    val symbols        = outlineSymbols(state)
    val activeLocation = currentOutlineActiveLocation(symbols, state)
    val hydratedSurfaces = state.uiSurfaces.map {
      case surface @ UiSurface(_, SurfaceContent.Outline(_, _), SurfacePresentation.Pinned(_, _), _) =>
        surface.copy(content = SurfaceContent.Outline(symbols, activeLocation))
      case surface =>
        surface
    }
    state.copy(uiSurfaces = hydratedSurfaces)

  private def openPresetMarkdownPreviewIfNeeded(preset: UiPreset): IO[Unit] =
    if preset.config.markdownViewMode == MarkdownViewMode.SplitPreview then openMarkdownPreview
    else IO.unit

  protected def duplicateUiPresetEffect(sourceName: String, targetName: String): IO[Unit] =
    (normalizedPresetName(sourceName), normalizedPresetName(targetName)) match
      case (Some(source), Some(target)) =>
        uiPresetStore
          .find(source)
          .map(_.orElse(UiPreset.builtIn(source)))
          .flatMap {
            case Some(preset) =>
              uiPresetStore.upsert(preset.copy(name = target)) >>
                refreshCommandRunnerUiPresetPreviews >>
                updateCommandRunnerPresetContext(Some(target), s"Preset duplicated. Configure $target.")
            case None =>
              logger.warn(s"[PRESET] UI preset not found: $source")
          }
          .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to duplicate UI preset $source"))
      case _ =>
        logger.warn("[PRESET] Ignoring duplicate request with empty UI preset name")

  protected def renameUiPresetEffect(sourceName: String, targetName: String): IO[Unit] =
    (normalizedPresetName(sourceName), normalizedPresetName(targetName)) match
      case (Some(source), _) if UiPreset.builtIn(source).nonEmpty =>
        updateCommandRunnerPresetContext(Some(source), s"Built-in preset cannot be renamed. Duplicate $source first.")
      case (Some(source), Some(target)) =>
        uiPresetStore
          .rename(source, target)
          .flatTap(_ =>
            refreshCommandRunnerUiPresetPreviews >>
              updateCommandRunnerPresetContext(Some(target), s"Preset renamed. Configure $target.")
          )
          .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to rename UI preset $source"))
      case _ =>
        logger.warn("[PRESET] Ignoring rename request with empty UI preset name")

  protected def deleteUiPresetEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case Some(presetName) if UiPreset.builtIn(presetName).nonEmpty =>
        updateCommandRunnerPresetContext(
          Some(presetName),
          "Built-in preset cannot be deleted. Use Reset Preset to discard overrides."
        )
      case Some(presetName) =>
        uiPresetStore
          .delete(presetName)
          .flatTap(_ =>
            refreshCommandRunnerUiPresetPreviews >> updateCommandRunnerPresetContext(None, "Preset deleted.")
          )
          .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to delete UI preset $presetName"))
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")

  protected def resetUiPresetEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case Some(presetName) =>
        UiPreset.builtIn(presetName) match
          case Some(_) =>
            uiPresetStore
              .delete(presetName)
              .flatTap(_ =>
                refreshCommandRunnerUiPresetPreviews >>
                  updateCommandRunnerPresetContext(Some(presetName), s"Preset reset. Configure $presetName.")
              )
              .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to reset UI preset $presetName"))
          case None =>
            logger.warn(s"[PRESET] Built-in UI preset not found: $presetName")
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")

  private def normalizedPresetName(name: String): Option[String] =
    Option(name.trim).filter(_.nonEmpty)

  private def refreshCommandRunnerUiPresetPreviews: IO[Unit] =
    uiPresetStore
      .list()
      .map(_.map(UiPreset.Preview.fromPreset))
      .handleErrorWith(error => logger.error(error)("[PRESET] Failed to list UI presets").as(Nil))
      .flatMap(previews => stateRef.update(state => updateCommandRunnerUiPresetPreviews(state, previews)))

  private def updateCommandRunnerUiPresetPreviews(state: AppState, previews: List[UiPreset.Preview]): AppState =
    state.commandRunnerSurface match
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            val updatedRunner = runner.withUiPresetPreviews(previews)
            val updatedSurfaces = state.uiSurfaces.map {
              case current if current.id == surface.id =>
                current.copy(content = SurfaceContent.CommandPalette(updatedRunner))
              case current @ UiSurface(_, SurfaceContent.CommandPaletteSubmenu(_, groupId, previewOnly), _, _) =>
                current.copy(content = SurfaceContent.CommandPaletteSubmenu(updatedRunner, groupId, previewOnly))
              case current =>
                current
            }
            state.copy(uiSurfaces = updatedSurfaces)
          case _ =>
            state
      case None =>
        state

  private def updateCommandRunnerPresetContext(presetName: Option[String], statusMessage: String): IO[Unit] =
    stateRef.update(updateCommandRunnerPresetContextInState(_, presetName, statusMessage))

  private def updateCommandRunnerPresetContextInState(
    state: AppState,
    presetName: Option[String],
    statusMessage: String
  ): AppState =
    state.commandRunnerSurface match
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            val updatedRunner = runner.copy(
              editingPresetName = presetName,
              editingItemId = None,
              editingText = "",
              statusMessage = Some(statusMessage)
            )
            val updatedSurfaces = state.uiSurfaces.map {
              case current if current.id == surface.id =>
                current.copy(content = SurfaceContent.CommandPalette(updatedRunner))
              case current @ UiSurface(_, SurfaceContent.CommandPaletteSubmenu(_, groupId, previewOnly), _, _) =>
                current.copy(content = SurfaceContent.CommandPaletteSubmenu(updatedRunner, groupId, previewOnly))
              case current =>
                current
            }
            state.copy(uiSurfaces = updatedSurfaces)
          case _ =>
            state
      case None =>
        state

  private def focusCreatedPresetOptions(name: String): IO[Unit] =
    stateRef.update { state =>
      state.commandRunnerSurface match
        case Some(surface) =>
          surface.content match
            case SurfaceContent.CommandPalette(runner) =>
              val updatedRunner = runner.copy(
                previewedGroupId = Some("settings-ui-presets"),
                activeSubmenu = Some(
                  CommandRunnerSubmenuState(
                    groupId = "settings-preset-edit",
                    parentGroupId = Some("settings-ui-presets")
                  )
                ),
                submenuSelections = runner.submenuSelections + ("settings-ui-presets" -> 2),
                editingItemId = None,
                editingText = "",
                editingPresetName = Some(name.trim),
                statusMessage = Some("Preset saved. Configure workspace options.")
              )
              val submenuSurface = UiSurface(
                id = CommandRunnerSubmenuSurfaceId,
                content = SurfaceContent.CommandPaletteSubmenu(
                  updatedRunner,
                  "settings-preset-edit",
                  previewOnly = false
                ),
                presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
              )
              val updatedSurfaces = state.uiSurfaces
                .filterNot(_.id == CommandRunnerSubmenuSurfaceId)
                .map {
                  case current if current.id == surface.id =>
                    current.copy(content = SurfaceContent.CommandPalette(updatedRunner))
                  case current =>
                    current
                } :+ submenuSurface
              state.copy(uiSurfaces = updatedSurfaces, focus = Focus.Surface(CommandRunnerSubmenuSurfaceId))
            case _ =>
              state
        case None =>
          state
    }

  private def runProjectTask(state: AppState, kind: ProjectTaskKind): IO[Unit] =
    projectTaskStartPath(state).flatMap { start =>
      ProjectTaskDetector.detect(start, kind) match
        case None =>
          pinProjectTerminal(ProjectTaskTerminal.noTask(kind, start))
        case Some(command) =>
          pinProjectTerminal(ProjectTaskTerminal.started(command)) >>
            ProjectTaskRunner
              .run(command)
              .attempt
              .flatMap {
                case Right(result) =>
                  pinProjectTerminal(ProjectTaskTerminal.completed(result))
                case Left(error) =>
                  pinProjectTerminal(ProjectTaskTerminal.failedToStart(command, error))
              }
    }

  private def projectTaskStartPath(state: AppState): IO[Path] =
    state.focusedBufferId
      .flatMap(state.buffers.get)
      .flatMap(_.filePath)
      .fold(FileUtils.getCurrentDirectory)(path => IO.pure(path))

  private def pinProjectTerminal(text: String): IO[Unit] =
    pinPanel(PanelContent.Terminal(text, text.length), PanelPosition.Bottom, 14)

  private def requestLspHover(state: AppState): IO[Unit] =
    activeLspRequestTarget(state) match
      case Some((uri, languageId, cursor, _)) =>
        lspQueue.offer(LspEffect.HoverRequested(uri, languageId, cursor.line, cursor.column, cursor))
      case None =>
        showLspUnavailablePeek(state)

  private def requestLspDefinition(state: AppState): IO[Unit] =
    activeLspRequestTarget(state) match
      case Some((uri, languageId, cursor, buffer)) =>
        lspQueue.offer(
          LspEffect.DefinitionRequested(
            uri,
            languageId,
            cursor.line,
            cursor.column,
            cursor,
            wordAtCursor(buffer, cursor)
          )
        )
      case None =>
        showLspUnavailablePeek(state)

  private def activeLspRequestTarget(state: AppState): Option[(String, LanguageId, CursorPosition, Buffer)] =
    for
      bufferId   <- activeEditorBufferId(state)
      buffer     <- state.buffers.get(bufferId)
      path       <- buffer.filePath
      languageId <- buffer.language
      cursor     <- buffer.cursors.headOption
    yield (path.toUri.toString, languageId, cursor, buffer)

  private def showLspUnavailablePeek(state: AppState): IO[Unit] =
    showPeek(
      PeekContent.QuickInfo("LSP requests need a saved buffer with a language mode."),
      state.activeCursorPosition.getOrElse(CursorPosition(0, 0))
    )

  private def wordAtCursor(buffer: Buffer, cursor: CursorPosition): String =
    val line = buffer.content.getLine(cursor.line).getOrElse("")
    if line.isEmpty then ""
    else
      val clamped = cursor.column.max(0).min(line.length)
      val start =
        Iterator.iterate(clamped)(i => i - 1).dropWhile(i => i > 0 && isSymbolChar(line.charAt(i - 1))).next()
      val end =
        Iterator.iterate(clamped)(i => i + 1).dropWhile(i => i < line.length && isSymbolChar(line.charAt(i))).next()
      line.substring(start, end)

  private def isSymbolChar(char: Char): Boolean =
    char.isLetterOrDigit || char == '_'

  private def persistConfigFile(config: com.serenity.config.AppConfig): IO[Unit] =
    configPersistencePath match
      case Some(path) =>
        IO.blocking(com.serenity.config.ConfigManager.saveConfig(config, path)).flatMap {
          case true  => IO.unit
          case false => logger.warn(s"[CONFIG] Failed to persist config to $path")
        }
      case None =>
        IO.unit

  private def reloadPresetDirectories(preset: UiPreset): IO[Unit] =
    preset.pinnedPanels.traverse_ { panel =>
      panel.content match
        case UiPreset.PanelContentSnapshot.DirectoryTree(rootPath, _, expandedPaths) =>
          (rootPath :: expandedPaths).distinct.traverse_(path =>
            loadPinnedDirectoryEffect(panel.position, Path.of(path))
          )
        case _ =>
          IO.unit
    }

  private def toggleCommentLens(state: AppState): IO[Unit] =
    state.commentLensSurface match
      case Some(_) =>
        updateState(dismissCommentLens)
      case None =>
        activeEditorComment(state) match
          case Some((cursor, lens)) =>
            updateState { current =>
              val surface = UiSurface(
                id = SurfaceId("comment-lens"),
                content = SurfaceContent.CommentLens(lens),
                presentation = SurfacePresentation.Floating(Some(cursor), SurfacePlacement.AboveCursor)
              )
              current
                .copy(uiSurfaces = current.uiSurfaces.filterNot(isCommentLensSurface) :+ surface)
                .pushFocus(Focus.Surface(surface.id))
            }
          case None =>
            logger.debug("[CMD] Comment lens requested without an active comment")

  private def activeEditorComment(state: AppState): Option[(CursorPosition, CommentLensState)] =
    for
      paneId   <- state.layout.activeEditorPaneId
      pane     <- state.layout.editorPanes.get(paneId)
      bufferId <- pane.bufferId
      buffer   <- state.buffers.get(bufferId)
      cursor   <- buffer.cursors.headOption
      comment  <- CommentRendering.atCursor(buffer)
    yield
      val target = buffer.documentComments.find(_.contains(cursor))
      val draft  = target.map(_.text).getOrElse(comment.raw)
      (
        cursor,
        CommentLensState(
          comment = comment,
          draft = draft,
          cursor = draft.length,
          target = target
        )
      )

  private def dismissCommentLens(state: AppState): AppState =
    state.copy(uiSurfaces = state.uiSurfaces.filterNot(isCommentLensSurface)).popFocus

  private def isCommentLensSurface(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.CommentLens(_) => true
      case _                             => false

  private def openMarkdownPreview: IO[Unit] =
    pinPanelKind(PanelKind.MarkdownPreview, PanelPosition.Right)

  private def setPanelPin(kind: PanelKind, position: Option[PanelPosition]): IO[Unit] =
    val updateEffect = position match
      case None =>
        updatePanelState(removePanelKind(kind))
      case Some(targetPosition) =>
        pinPanelKind(kind, targetPosition)
    updateEffect >> refreshCommandRunnerPanelSelections >> markEditedUiPresetDraftFromCommandRunner

  private def movePanelKind(kind: PanelKind, delta: Int): IO[Unit] =
    updatePanelState(reorderPanelKind(kind, delta)) >> markEditedUiPresetDraftFromCommandRunner

  private def pinPanelKind(kind: PanelKind, position: PanelPosition): IO[Unit] =
    kind match
      case PanelKind.Explorer =>
        stateRef.get.flatMap { state =>
          newestPanelKindSurface(kind, state) match
            case Some(surface) =>
              updatePanelState(upsertPanelKind(kind, surface.content, position, defaultPanelSize(kind, position)))
            case None =>
              FileUtils.getCurrentDirectory.flatMap(path =>
                interpretEffect(
                  AppEffect.Explorer(ExplorerEffect.OpenRoot(position, path, defaultPanelSize(kind, position)))
                )
              )
        }
      case PanelKind.Outline =>
        stateRef.get.flatMap { state =>
          val symbols = outlineSymbols(state)
          updatePanelState(
            upsertPanelKind(
              kind,
              SurfaceContent.Outline(symbols, currentOutlineActiveLocation(symbols, state)),
              position,
              defaultPanelSize(kind, position)
            )
          )
        }
      case PanelKind.Diagnostics =>
        updatePanelState(
          upsertPanelKind(kind, SurfaceContent.Diagnostics(Nil), position, defaultPanelSize(kind, position))
        )
      case PanelKind.MarkdownPreview =>
        stateRef.get.flatMap { state =>
          markdownPreviewContent(state) match
            case Some(content) =>
              updatePanelState(upsertPanelKind(kind, content, position, defaultPanelSize(kind, position)))
            case None =>
              logger.debug("[CMD] Markdown preview requested without an active Markdown buffer")
        }

  private def markdownPreviewContent(state: AppState): Option[SurfaceContent] =
    state.focusedBufferId
      .flatMap(state.buffers.get)
      .filter(_.language.contains(LanguageId.Markdown))
      .map { buffer =>
        val title = buffer.filePath
          .flatMap(path => Option(path.getFileName).map(_.toString))
          .getOrElse("Untitled")
        SurfaceContent.MarkdownPreview(buffer.id, title)
      }

  private def updatePanelState(update: AppState => AppState): IO[Unit] =
    stateRef.get.flatMap { state =>
      val updated = update(state)
      validateAndUpdateState(updated, state)
    }

  private def removePanelKind(kind: PanelKind)(state: AppState): AppState =
    val removedIds = state.uiSurfaces.collect {
      case surface if panelKindOf(surface.content).contains(kind) => surface.id
    }.toSet
    val nextFocus = state.focus match
      case Focus.Surface(surfaceId) if removedIds.contains(surfaceId) =>
        state.layout.activeEditorPaneId.map(Focus.EditorPane.apply).getOrElse(state.focus)
      case _ =>
        state.focus
    state.copy(
      uiSurfaces = state.uiSurfaces.filterNot(surface => removedIds.contains(surface.id)),
      focus = nextFocus
    )

  private def upsertPanelKind(
    kind: PanelKind,
    content: SurfaceContent,
    position: PanelPosition,
    size: Int
  )(state: AppState): AppState =
    val matchingSurfaces = state.uiSurfaces.filter(surface => panelKindOf(surface.content).contains(kind))
    val retainedSurface  = matchingSurfaces.reverse.headOption
    val stateWithoutKind = state.copy(
      uiSurfaces = state.uiSurfaces.filterNot(surface => panelKindOf(surface.content).contains(kind))
    )
    val (stateWithId, surface) = retainedSurface match
      case Some(existing) =>
        stateWithoutKind -> existing.copy(
          content = content,
          presentation = SurfacePresentation.Pinned(position, size),
          dismissOnMove = false
        )
      case None =>
        val (allocatedState, surfaceId) = stateWithoutKind.allocateSurfaceId
        allocatedState -> UiSurface(
          surfaceId,
          content,
          SurfacePresentation.Pinned(position, size),
          dismissOnMove = false
        )
    val nextFocus = state.focus match
      case Focus.Surface(surfaceId) if matchingSurfaces.exists(_.id == surfaceId) => Focus.Surface(surface.id)
      case _                                                                      => state.focus
    stateWithId.copy(uiSurfaces = stateWithId.uiSurfaces :+ surface, focus = nextFocus)

  private def reorderPanelKind(kind: PanelKind, delta: Int)(state: AppState): AppState =
    if delta == 0 then state
    else
      val pinnedPanels = state.uiSurfaces.collect {
        case surface @ UiSurface(_, _, SurfacePresentation.Pinned(position, _), _)
            if panelKindOf(surface.content).isDefined =>
          surface -> position
      }
      pinnedPanels.find((surface, _) => panelKindOf(surface.content).contains(kind)) match
        case None => state
        case Some((targetSurface, targetPosition)) =>
          val sameEdge = pinnedPanels.collect {
            case (surface, position) if position == targetPosition => surface
          }
          val currentIndex = sameEdge.indexWhere(_.id == targetSurface.id)
          val targetIndex  = (currentIndex + delta).max(0).min(sameEdge.length - 1)
          if currentIndex < 0 || currentIndex == targetIndex then state
          else
            val reorderedSameEdge = moveWithinList(sameEdge, currentIndex, targetIndex)
            val replacements      = reorderedSameEdge.iterator
            val updatedSurfaces = state.uiSurfaces.map { surface =>
              if sameEdge.exists(_.id == surface.id) then replacements.next()
              else surface
            }
            state.copy(uiSurfaces = updatedSurfaces)

  private def moveWithinList[A](values: List[A], from: Int, to: Int): List[A] =
    if from == to then values
    else
      values.lift(from) match
        case None => values
        case Some(value) =>
          val withoutValue = values.patch(from, Nil, 1)
          withoutValue.patch(to, List(value), 0)

  private def newestPanelKindSurface(kind: PanelKind, state: AppState): Option[UiSurface] =
    state.uiSurfaces.reverse.find(surface => panelKindOf(surface.content).contains(kind))

  private def panelKindOf(content: SurfaceContent): Option[PanelKind] =
    content match
      case SurfaceContent.DirectoryTree(_, _)   => Some(PanelKind.Explorer)
      case SurfaceContent.Outline(_, _)         => Some(PanelKind.Outline)
      case SurfaceContent.Diagnostics(_)        => Some(PanelKind.Diagnostics)
      case SurfaceContent.MarkdownPreview(_, _) => Some(PanelKind.MarkdownPreview)
      case _                                    => None

  private def defaultPanelSize(kind: PanelKind, position: PanelPosition): Int =
    kind match
      case PanelKind.MarkdownPreview => 40
      case PanelKind.Diagnostics =>
        position match
          case PanelPosition.Top | PanelPosition.Bottom => 10
          case PanelPosition.Left | PanelPosition.Right => 30
      case PanelKind.Explorer | PanelKind.Outline =>
        position match
          case PanelPosition.Top | PanelPosition.Bottom => 10
          case PanelPosition.Left | PanelPosition.Right => 30

  private def refreshCommandRunnerPanelSelections: IO[Unit] =
    stateRef.update { state =>
      val selections = CommandRunnerPanelSelections.fromState(state)
      val updatedSurfaces = state.uiSurfaces.map {
        case surface @ UiSurface(_, SurfaceContent.CommandPalette(runner), _, _) =>
          surface.copy(content =
            SurfaceContent.CommandPalette(runner.copy(optionSelections = runner.optionSelections ++ selections))
          )
        case surface @ UiSurface(_, SurfaceContent.CommandPaletteSubmenu(runner, groupId, previewOnly), _, _) =>
          surface.copy(content =
            SurfaceContent.CommandPaletteSubmenu(
              runner.copy(optionSelections = runner.optionSelections ++ selections),
              groupId,
              previewOnly
            )
          )
        case other => other
      }
      state.copy(uiSurfaces = updatedSurfaces)
    }

  private def outlineSymbols(state: AppState): List[Symbol] =
    state.focusedBufferId
      .flatMap(state.buffers.get)
      .map(outlineSymbolsForBuffer)
      .getOrElse(Nil)

  private def outlineSymbolsForBuffer(buffer: Buffer): List[Symbol] =
    (
      DocumentOutline.forBuffer(buffer) ++
        DocumentNavigation.bookmarkSymbols(buffer.bookmarks) ++
        DocumentNavigation.commentSymbols(buffer.documentComments)
    )
      .sortBy(symbol => (symbol.location.line, symbol.location.column, symbol.name))

  private def currentOutlineActiveLocation(symbols: List[Symbol], state: AppState): Option[Location] =
    state.activeCursorPosition
      .flatMap(cursor => DocumentNavigation.currentSymbol(symbols, cursor))
      .map(_.location)

  private def navigateDocumentSymbol(
    state: AppState,
    chooseSymbol: (List[Symbol], CursorPosition) => Option[Symbol]
  ): IO[Unit] =
    navigateSymbols(state, outlineSymbolsForBuffer, chooseSymbol, "Document symbol")

  private def navigateBookmark(
    state: AppState,
    chooseSymbol: (List[Symbol], CursorPosition) => Option[Symbol]
  ): IO[Unit] =
    navigateSymbols(state, buffer => DocumentNavigation.bookmarkSymbols(buffer.bookmarks), chooseSymbol, "Bookmark")

  private def navigateDocumentComment(
    state: AppState,
    chooseSymbol: (List[Symbol], CursorPosition) => Option[Symbol]
  ): IO[Unit] =
    navigateSymbols(
      state,
      buffer => DocumentNavigation.commentSymbols(buffer.documentComments),
      chooseSymbol,
      "Document comment"
    )

  private def navigateSymbols(
    state: AppState,
    symbolsForBuffer: Buffer => List[Symbol],
    chooseSymbol: (List[Symbol], CursorPosition) => Option[Symbol],
    label: String
  ): IO[Unit] =
    activeEditorBuffer(state)
      .flatMap {
        case (paneId, buffer) =>
          val cursor  = buffer.cursors.headOption.getOrElse(CursorPosition(0, 0))
          val symbols = symbolsForBuffer(buffer)
          chooseSymbol(symbols, cursor).map { symbol =>
            val before = NavigationPoint(paneId, buffer.id, cursor)
            val after = NavigationPoint(paneId, buffer.id, CursorPosition(symbol.location.line, symbol.location.column))
            before -> after
          }
      } match
      case Some((before, after)) if before != after =>
        val sweep = navigationSweep(before, after)
        updateState { current =>
          animateNavigationTarget(
            moveToNavigationPoint(current, after).copy(
              navigationBackStack = pushNavigationPoint(before, current.navigationBackStack),
              navigationForwardStack = Nil
            ),
            after,
            sweep
          )
        }
      case Some(_) =>
        logger.debug(s"[CMD] $label navigation requested for the current location")
      case None =>
        logger.debug(s"[CMD] $label navigation requested without a target")

  private def navigationSweep(before: NavigationPoint, after: NavigationPoint): com.serenity.animation.SweepDirection =
    if after.cursor.line < before.cursor.line ||
        (after.cursor.line == before.cursor.line && after.cursor.column < before.cursor.column)
    then com.serenity.animation.SweepDirection.Backward
    else com.serenity.animation.SweepDirection.Forward

  private def animateNavigationTarget(
    state: AppState,
    point: NavigationPoint,
    sweep: com.serenity.animation.SweepDirection
  ): AppState =
    state.buffers.get(point.bufferId) match
      case Some(buffer) =>
        val cells = VisibleBufferAnimationCells.fromBuffer(
          buffer,
          state.config.wordWrapEnabled,
          state.theme.background,
          state.theme.foreground
        )

        if cells.isEmpty then state
        else
          state.config.scaledUiAnimation.fold(state) { config =>
            val animated = com.serenity.animation.FlowAnimationBuilder.build(
              cells,
              com.serenity.animation.FlowDirection.ByRow,
              sweep,
              config.steps
            )
            val updatedBuffer = buffer.copy(animations = buffer.animations.clearAll().mergeAnimations(animated))
            state.copy(buffers = state.buffers + (point.bufferId -> updatedBuffer))
          }
      case None =>
        state

  private def updateNavigationHistory(
    state: AppState,
    target: NavigationPoint,
    backStack: List[NavigationPoint],
    forwardStack: List[NavigationPoint],
    sweep: com.serenity.animation.SweepDirection
  ): AppState =
    animateNavigationTarget(
      moveToNavigationPoint(state, target).copy(
        navigationBackStack = backStack,
        navigationForwardStack = forwardStack
      ),
      target,
      sweep
    )

  private def navigateHistoryBack(): IO[Unit] =
    updateState { current =>
      current.navigationBackStack match
        case target :: remaining =>
          currentNavigationPoint(current) match
            case Some(point) =>
              updateNavigationHistory(
                current,
                target,
                remaining,
                pushNavigationPoint(point, current.navigationForwardStack),
                navigationSweep(point, target)
              )
            case None => current
        case Nil => current
    }

  private def navigateHistoryForward(): IO[Unit] =
    updateState { current =>
      current.navigationForwardStack match
        case target :: remaining =>
          currentNavigationPoint(current) match
            case Some(point) =>
              updateNavigationHistory(
                current,
                target,
                pushNavigationPoint(point, current.navigationBackStack),
                remaining,
                navigationSweep(point, target)
              )
            case None => current
        case Nil => current
    }

  private def currentNavigationPoint(state: AppState): Option[NavigationPoint] =
    activeEditorBuffer(state).flatMap {
      case (paneId, buffer) =>
        buffer.cursors.headOption.map(cursor => NavigationPoint(paneId, buffer.id, cursor))
    }

  private def pushNavigationPoint(point: NavigationPoint, stack: List[NavigationPoint]): List[NavigationPoint] =
    stack match
      case head :: _ if head == point => stack
      case _                          => point :: stack

  private def moveToNavigationPoint(state: AppState, point: NavigationPoint): AppState =
    (state.layout.editorPanes.get(point.paneId), state.buffers.get(point.bufferId)) match
      case (Some(pane), Some(buffer)) =>
        val viewport = CursorViewport.adjustForCursor(buffer, state, point.cursor)
        val updatedBuffer = buffer.copy(
          cursors = List(point.cursor),
          selection = None,
          selections = Nil,
          preferredColumn = Some(point.cursor.column),
          preferredXPx = None,
          multiCursorVerticalStates = Nil,
          viewport = viewport
        )
        state.copy(
          buffers = state.buffers + (point.bufferId -> updatedBuffer),
          layout = state.layout.copy(
            editorPanes = state.layout.editorPanes + (point.paneId -> pane.copy(bufferId = Some(point.bufferId))),
            activeEditorPaneId = Some(point.paneId)
          ),
          focus = Focus.EditorPane(point.paneId)
        )
      case _ => state

  private def toggleBookmark(state: AppState): IO[Unit] =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val cursor = buffer.cursors.headOption.getOrElse(CursorPosition(0, 0))
        updateState { current =>
          current.buffers.get(buffer.id) match
            case Some(currentBuffer) =>
              val bookmarks =
                if currentBuffer.bookmarks.contains(cursor) then currentBuffer.bookmarks.filterNot(_ == cursor)
                else (cursor :: currentBuffer.bookmarks).distinct.sortBy(position => (position.line, position.column))

              current.copy(buffers = current.buffers + (buffer.id -> currentBuffer.copy(bookmarks = bookmarks)))
            case None => current
        }
      case None =>
        logger.debug("[CMD] Toggle bookmark requested without an active editor buffer")

  private def addDocumentComment(state: AppState, text: String): IO[Unit] =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val cursor           = buffer.cursors.headOption.getOrElse(CursorPosition(0, 0))
        val normalizedCursor = snapCursorAfterGrapheme(buffer, cursor)
        val range = buffer.primarySelection
          .map(selection => normalizedCommentSelectionRange(buffer, selection))
          .getOrElse(normalizedCursor -> normalizedCursor)
        val commentText = Option(text.trim).filter(_.nonEmpty).getOrElse("Comment")
        val comment     = DocumentComment(range._1, range._2, commentText)
        updateState: current =>
          current.buffers.get(buffer.id) match
            case Some(currentBuffer) =>
              val existingCommentAtCursor = currentBuffer.documentComments.find(_.contains(normalizedCursor))
              val updatedComment = existingCommentAtCursor
                .map(existing => existing.copy(text = commentText))
                .getOrElse(comment)
              val comments = (updatedComment :: currentBuffer.documentComments.filterNot(existing =>
                existingCommentAtCursor.contains(existing) ||
                  (existing.start == comment.start && existing.end == comment.end)
              )).sortBy(existing => (existing.start.line, existing.start.column, existing.text))
              current.copy(
                buffers =
                  current.buffers + (buffer.id -> currentBuffer.copy(documentComments = comments, isDirty = true))
              )
            case None => current
      case None =>
        logger.debug("[CMD] Add document comment requested without an active editor buffer")

  private def normalizedCommentSelectionRange(
    buffer: Buffer,
    selection: Selection
  ): (CursorPosition, CursorPosition) =
    val startOffset = buffer.content.lineColumnToOffset(selection.start.line, selection.start.column)
    val endOffset   = buffer.content.lineColumnToOffset(selection.end.line, selection.end.column)
    if startOffset >= endOffset then
      val cursor = offsetToCursorPosition(buffer, graphemeBoundaryAfterOrAt(buffer, startOffset))
      cursor -> cursor
    else
      offsetToCursorPosition(buffer, graphemeBoundaryBeforeOrAt(buffer, startOffset)) ->
        offsetToCursorPosition(buffer, graphemeBoundaryAfterOrAt(buffer, endOffset))

  private def snapCursorAfterGrapheme(buffer: Buffer, cursor: CursorPosition): CursorPosition =
    val offset = buffer.content.lineColumnToOffset(cursor.line, cursor.column)
    offsetToCursorPosition(buffer, graphemeBoundaryAfterOrAt(buffer, offset))

  private def offsetToCursorPosition(buffer: Buffer, offset: Int): CursorPosition =
    val (line, column) = buffer.content.offsetToLineColumn(offset)
    CursorPosition(line, column)

  private def graphemeBoundaryBeforeOrAt(buffer: Buffer, offset: Int): Int =
    TextEditing.graphemeBoundaryBeforeOrAt(RopeCharacterSource(buffer.content), offset)

  private def graphemeBoundaryAfterOrAt(buffer: Buffer, offset: Int): Int =
    TextEditing.graphemeBoundaryAfterOrAt(RopeCharacterSource(buffer.content), offset)

  final private case class RopeCharacterSource(content: com.serenity.rope.Rope) extends TextEditing.CharacterSource:
    override def length: Int =
      content.weight

    override def charAt(index: Int): Char =
      content.index(index).getOrElse('\u0000')

  private def deleteDocumentComment(state: AppState): IO[Unit] =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val cursor = buffer.cursors.headOption.getOrElse(CursorPosition(0, 0))
        updateState: current =>
          current.buffers.get(buffer.id) match
            case Some(currentBuffer) =>
              val comments = currentBuffer.documentComments.filterNot(_.contains(cursor))
              current.copy(
                buffers = current.buffers + (buffer.id ->
                  currentBuffer.copy(
                    documentComments = comments,
                    isDirty = currentBuffer.isDirty || comments != currentBuffer.documentComments
                  ))
              )
            case None => current
      case None =>
        logger.debug("[CMD] Delete document comment requested without an active editor buffer")

  private def activeEditorBuffer(state: AppState): Option[(PaneId, Buffer)] =
    for
      paneId   <- state.layout.activeEditorPaneId
      pane     <- state.layout.editorPanes.get(paneId)
      bufferId <- pane.bufferId
      buffer   <- state.buffers.get(bufferId)
    yield (paneId, buffer)

  private def setMarkdownViewMode(mode: MarkdownViewMode): IO[Unit] =
    val updateConfigEffect = updateConfig(_.withMarkdownViewMode(mode)).void
    val updateModeEffect = mode match
      case MarkdownViewMode.SplitPreview =>
        updateConfigEffect >> openMarkdownPreview
      case MarkdownViewMode.Source | MarkdownViewMode.InlineLens =>
        updateConfigEffect >> unpinMarkdownPreviewPanel()
    updateModeEffect >> markEditedUiPresetDraftFromCommandRunner

  private def unpinMarkdownPreviewPanel(): IO[Unit] =
    updateState { state =>
      val markdownPreviewSurfaceIds = state.pinnedSurfaces.collect {
        case UiSurface(id, SurfaceContent.MarkdownPreview(_, _), SurfacePresentation.Pinned(_, _), _) => id
      }.toSet
      val nextFocus = state.focus match
        case Focus.Surface(surfaceId) if markdownPreviewSurfaceIds.contains(surfaceId) =>
          state.layout.activeEditorPaneId.map(Focus.EditorPane.apply).getOrElse(state.focus)
        case _ =>
          state.focus
      state.copy(
        uiSurfaces = state.uiSurfaces.filterNot(surface => markdownPreviewSurfaceIds.contains(surface.id)),
        focus = nextFocus
      )
    }

  protected def pinExplorerPanelEffect(position: PanelPosition, path: Path, size: Int): IO[Unit] =
    for
      fileEntries <- fileManager.getFileBrowser.listDirectory(path)
      dirEntries = toDirEntries(fileEntries)
      _ <- applyEvent(
        ExplorerEvent.RootDirectoryLoaded(
          position = position,
          rootPath = path,
          size = size,
          entries = dirEntries,
          selectedPath = dirEntries.headOption.map(_.path)
        )
      )
    yield ()

  protected def loadPinnedDirectoryEffect(position: PanelPosition, path: Path): IO[Unit] =
    (for
      fileEntries <- fileManager.getFileBrowser.listDirectory(path)
      dirEntries = toDirEntries(fileEntries)
      _ <- applyEvent(ExplorerEvent.DirectoryLoaded(position, path, dirEntries))
    yield ()).handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to load directory $path"))

  private def toDirEntries(entries: List[FileEntry]): List[DirEntry] =
    entries.map(entry => DirEntry(entry.path, entry.name, entry.isDirectory))

  protected def directLoadFileEffect(path: Path): IO[Unit] =
    IO.blocking(FileUtils.isReadableFile(path)).flatMap {
      case false => logger.debug(s"[FILE] DirectLoad: file not readable: $path")
      case true =>
        stateRef
          .modify { state =>
            val bufferId = state.nextBufferId
            (state.copy(nextBufferId = BufferId(bufferId.value + 1)), bufferId)
          }
          .flatMap(bufferId => fileManager.loadFile(path, bufferId))
          .flatMap { loadedBuffer =>
            stateRef.modify { state =>
              val newBufferId = loadedBuffer.id
              val stateWithBuffer = state.copy(
                buffers = state.buffers + (newBufferId -> loadedBuffer)
              )
              val updatedState = EditorState.insertBufferInOrder(stateWithBuffer, newBufferId)
              val rebalanced   = EditorState.rebalancePanes(updatedState, Some(newBufferId))
              val focused      = EditorState.focusBuffer(rebalanced, newBufferId)
              val resized =
                focused.viewportSize
                  .map(viewportSize => LayoutEngine.syncViewportDimensions(focused, viewportSize))
                  .getOrElse(focused)
              (resized, loadedBuffer)
            }
          }
          .flatTap { loadedBuffer =>
            loadedBuffer.language match
              case Some(languageId) =>
                val uri  = path.toUri.toString
                val text = loadedBuffer.content.collect()
                lspQueue.offer(LspEffect.FileOpened(uri, languageId, text))
              case None => IO.unit
          }
          .flatTap(_ => stateRef.update(s => s.copy(recentFiles = trackRecentFile(s.recentFiles, path))))
          .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to load file at $path"))
          .void
    }

  protected def saveBufferEffect(bufferId: BufferId): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.buffers.get(bufferId) match
        case Some(buffer) if buffer.filePath.isDefined =>
          fileManager
            .saveBuffer(buffer)
            .flatMap(savedBuffer =>
              stateRef.update(current => current.copy(buffers = current.buffers + (bufferId -> savedBuffer)))
            )
            .flatTap(_ =>
              stateRef.get
                .flatMap(sessionPersistence.onBufferChange)
                .handleErrorWith(ex => logger.error(ex)("[SESSION] Auto-save after file save failed"))
            )
            .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to save buffer $bufferId"))
        case Some(_) =>
          logger.debug(s"[FILE] Buffer $bufferId has no file path; opening native Save As dialog") >>
            requestSaveAsFileDialog(state, Some(bufferId))
        case None =>
          logger.debug(s"[FILE] Buffer $bufferId not found for save")
    }

  protected def requestOpenFileDialog: IO[Unit] =
    FileUtils.getCurrentDirectory
      .flatMap(currentDirectory => fileDialog.chooseOpenFile(Some(currentDirectory)))
      .flatMap {
        case Some(path) =>
          updateState(_.copy(uiSurfaces = List.empty)) >> directLoadFileEffect(path)
        case None =>
          IO.unit
      }
      .handleErrorWith(ex => logger.error(ex)("[FILE] Native open-file dialog failed"))

  protected def exportCurrentThemeEffect(state: AppState): IO[Unit] =
    val config            = ThemeConfigWriter.themeToConfig(state.theme)
    val suggestedFileName = s"${ThemeConfigWriter.fileNameFor(config.name)}.conf"
    FileUtils.getCurrentDirectory
      .flatMap(currentDirectory => fileDialog.chooseSaveFile(Some(currentDirectory), Some(suggestedFileName)))
      .flatMap {
        case Some(path) =>
          ThemeConfigWriter
            .write(config, path)
            .flatTap(_ => logger.info(s"[THEMES] Exported current theme '${config.name}' to $path"))
        case None =>
          IO.unit
      }
      .handleErrorWith(ex => logger.error(ex)(s"[THEMES] Failed to export current theme '${config.name}'"))

  protected def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.buffers.get(bufferId) match
        case Some(buffer) =>
          fileManager
            .saveBuffer(buffer, path)
            .flatMap(savedBuffer =>
              stateRef.update(current => current.copy(buffers = current.buffers + (bufferId -> savedBuffer)))
            )
            .flatTap(_ => stateRef.update(s => s.copy(recentFiles = trackRecentFile(s.recentFiles, path))))
            .flatTap(_ =>
              stateRef.get
                .flatMap(sessionPersistence.onBufferChange)
                .handleErrorWith(ex => logger.error(ex)("[SESSION] Auto-save after file save failed"))
            )
            .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to save buffer $bufferId as $path"))
        case None =>
          logger.debug(s"[FILE] Buffer $bufferId not found for save as")
    }

  private def findModalForState(state: AppState): Modal =
    activeEditorBufferId(state)
      .flatMap(state.buffers.get)
      .flatMap { buffer =>
        buffer.findState match
          case Some(FindState(query, _, currentIndex)) if query.nonEmpty =>
            val resultSet = FindResultSet.normalized(query, findMatches(buffer, query).map(toFindResult), currentIndex)
            Some(Modal.Find(resultSet.query, resultSet.results, resultSet.currentIndex))
          case _ =>
            None
      }
      .getOrElse(Modal.Find("", Nil, 0))

  private def findMatches(buffer: Buffer, query: String): List[CursorPosition] =
    if query.isEmpty then Nil
    else buffer.content.searchAll(query).map(offset => cursorPositionForOffset(buffer.content, offset))

  private def toFindResult(cursor: CursorPosition): FindResult =
    FindResult(cursor.line, cursor.column)

  private def cursorPositionForOffset(content: com.serenity.rope.Rope, offset: Int): CursorPosition =
    val (line, column) = content.offsetToLineColumn(offset)
    CursorPosition(line, column)

  protected def toggleThemeEffect(state: AppState): IO[Unit] =
    val targetThemeName =
      state.theme.name match
        case "light"                                    => "dark"
        case "dark"                                     => "light"
        case "default-light"                            => "default-dark"
        case "default-dark"                             => "default-light"
        case name if name.toLowerCase.contains("light") => "default-dark"
        case _                                          => "default-light"

    interpretEffect(AppEffect.SwitchTheme(targetThemeName))

  protected def reloadThemeEffect(state: AppState): IO[Unit] =
    interpretEffect(AppEffect.ReloadTheme(state.theme.name))

  protected def applyThemeByName(themeName: String): IO[Unit] =
    themeManager
      .loadTheme(themeName)
      .flatMap { newTheme =>
        updateState { state =>
          val transition =
            if state.theme == newTheme then None
            else state.config.scaledUiAnimation.map(config => ThemeTransition(state.theme, 0, config.steps))
          state.copy(theme = newTheme, themeTransition = transition)
        } >> markEditedUiPresetDraftFromCommandRunner
      }
      .handleErrorWith(ex => logger.error(ex)(s"[THEME] Failed to switch theme to $themeName"))

  protected def reloadThemeByName(themeName: String): IO[Unit] =
    themeManager
      .loadTheme(themeName)
      .flatMap(theme => updateState(_.copy(theme = theme)))
      .handleErrorWith(ex => logger.error(ex)(s"[THEME] Failed to reload theme $themeName"))

  protected def openThemePickerEffect(state: AppState): IO[Unit] =
    themeNamesRef.get.flatMap { themeNames =>
      if themeNames.isEmpty then IO.unit
      else
        val currentTheme             = state.theme.name
        val selectedIndex            = themeNames.indexOf(currentTheme).max(0)
        val pickerState              = ThemePickerState(themeNames, selectedIndex, currentTheme)
        val (stateWithId, surfaceId) = state.allocateSurfaceId
        val surface = UiSurface(
          id = surfaceId,
          content = SurfaceContent.ThemePicker(pickerState),
          presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
        )
        validateAndUpdateState(
          stateWithId.copy(
            uiSurfaces = stateWithId.uiSurfaces :+ surface,
            focus = Focus.Surface(surfaceId)
          ),
          state
        )
    }

  protected def openThemeCreatorEffect(state: AppState): IO[Unit] =
    val creatorState             = ThemeCreatorState.fromTheme(state.theme)
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.ThemeCreator(creatorState),
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
    validateAndUpdateState(
      stateWithId
        .copy(
          uiSurfaces = stateWithId.uiSurfaces.filterNot {
            _.content match
              case SurfaceContent.ThemeCreator(_) => true
              case _                              => false
          } :+ surface
        )
        .pushFocus(Focus.Surface(surfaceId)),
      state
    )

  protected def openFileSearchEffect(state: AppState): IO[Unit] =
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.FileSearch(FileSearchState("", Nil, 0)),
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
    validateAndUpdateState(
      stateWithId.copy(
        uiSurfaces = stateWithId.uiSurfaces :+ surface,
        focus = Focus.Surface(surfaceId)
      ),
      state
    )
