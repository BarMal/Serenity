package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.serenity.animation.AnimationConfig
import com.serenity.command.*
import com.serenity.config.{DefaultDocumentMode, HotkeyTrigger, KeymapGroup, MarkdownViewMode}
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
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.{ThemeConfigWriter, ThemeCreatorState}
import fs2.Stream

/** Workflow operations selected by command effects. */
private[manager] trait WorkflowEffectPort:
  def requestOpenFile: IO[Unit]
  def requestSaveAs: IO[Unit]
  def refresh(surfaceId: SurfaceId): IO[Unit]
  def refreshFind(request: FindSearchRequest): IO[Unit]
  def submitFile(surfaceId: SurfaceId): IO[Unit]
  def submitReplace(surfaceId: SurfaceId): IO[Unit]
  def submitClose(surfaceId: SurfaceId): IO[Unit]

/** Interprets workflow effects without editor, theme, file, or runtime dependencies. */
final private[manager] class WorkflowEffectHandler(port: WorkflowEffectPort):

  def interpret(effect: WorkflowEffect): IO[Unit] =
    effect match
      case WorkflowEffect.RequestOpenFile           => port.requestOpenFile
      case WorkflowEffect.RequestSaveAs             => port.requestSaveAs
      case WorkflowEffect.RefreshFileWorkflow(id)   => port.refresh(id)
      case WorkflowEffect.RefreshFind(request)      => port.refreshFind(request)
      case WorkflowEffect.SubmitFileWorkflow(id)    => port.submitFile(id)
      case WorkflowEffect.SubmitReplaceWorkflow(id) => port.submitReplace(id)
      case WorkflowEffect.SubmitCloseWorkflow(id)   => port.submitClose(id)

/** Lifecycle operation required by lifecycle effects. */
private[manager] trait LifecycleEffectPort:
  def completeQuit: IO[Unit]

/** Interprets lifecycle effects without runtime, editor, or workflow dependencies. */
final private[manager] class LifecycleEffectHandler(port: LifecycleEffectPort):

  def interpret(effect: LifecycleEffect): IO[Unit] =
    effect match
      case LifecycleEffect.CompleteQuit => port.completeQuit

/** Owns ordered I/O interpretation for reducer effects. */
final private[manager] class StateManagerEffectHandlers(
    runtime: EffectRuntimePort,
    editor: EffectEditorPort,
    surfaces: EffectSurfacePort,
    files: EffectFilePort,
    sessions: EffectSessionPort,
    workflow: EffectModalWorkflowPort
)(using balance: com.serenity.rope.Balance):

  import editor.*
  import files.*
  import runtime.*
  import surfaces.*
  import sessions.*
  import workflow.*
  private val CommandRunnerSubmenuSurfaceId = SurfaceId("command-runner-submenu")
  private val DoubleTapWindow               = 200.millis

  private val workflowEffects = new WorkflowEffectHandler(new WorkflowEffectPort:
    def requestOpenFile: IO[Unit] = requestOpenFileDialog
    def requestSaveAs: IO[Unit]   = stateRef.get.flatMap(state => requestSaveAsFileDialog(state, state.focusedBufferId))
    def refresh(surfaceId: SurfaceId): IO[Unit]           = refreshFileWorkflowEffect(surfaceId)
    def refreshFind(request: FindSearchRequest): IO[Unit] = scheduleFindSearch(request)
    def submitFile(surfaceId: SurfaceId): IO[Unit]        = submitFileWorkflowEffect(surfaceId)
    def submitReplace(surfaceId: SurfaceId): IO[Unit]     = submitReplaceWorkflowEffect(surfaceId)
    def submitClose(surfaceId: SurfaceId): IO[Unit]       = submitCloseWorkflowEffect(surfaceId))

  private val lifecycleEffects = new LifecycleEffectHandler(
    new LifecycleEffectPort:
      def completeQuit: IO[Unit] = quitSignal.complete(()).attempt.void
  )

  private val animationEffects = new AnimationEffectHandler(bufferAnimationsRef)

  private[manager] val behavior = new CommandEffectInterpreter(
    CommandEffectInterpreter.Dependencies(
      interpretLifecycleEffect,
      interpretCommandEffect,
      interpretThemeEffect,
      interpretSurfaceEffect,
      interpretFileEffect,
      interpretExplorerEffect,
      interpretWorkflowEffect,
      interpretLspQueueEffect,
      animationEffects.interpret
    )
  )

  private[manager] def interpretEffect(effect: AppEffect): IO[Unit] =
    effect match
      case AppEffect.ScheduleCommandRunnerBindingExpiry(recordedAtMillis) =>
        scheduleCommandRunnerBindingExpiry(recordedAtMillis)
      case _ =>
        behavior.interpret(effect)

  private def scheduleCommandRunnerBindingExpiry(recordedAtMillis: Long): IO[Unit] =
    (IO.sleep(DoubleTapWindow) >>
      stateRef.get.flatMap { state =>
        val result = CommandRunnerReducer.reduce(
          com.serenity.keystroke.events.RunnerBindingRecordingExpired(recordedAtMillis),
          state,
          CommandRegistry.withToggleUI
        )
        validateAndUpdateState(result.state, state) >> result.effects.traverse_(interpretEffect)
      }).start.void

  private def interpretLifecycleEffect(effect: LifecycleEffect): IO[Unit] =
    lifecycleEffects.interpret(effect)

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
    workflowEffects.interpret(effect)

  private def interpretLspQueueEffect(effect: LspQueueEffect): IO[Unit] =
    effect match
      case LspQueueEffect.Enqueue(effect) =>
        lspQueue.enqueue(effect)
      case LspQueueEffect.DocumentChanged(uri, languageId, text) =>
        lspQueue.enqueueDocumentChange(uri, languageId, text)

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
    val updatedSurfaces = state.runtime.uiSurfaces.map {
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
    state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))

  private[manager] def updateConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    applyConfigUpdate(update)

  private def updateAppearanceConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    applyConfigUpdate(update)

  private def updateDocumentDefaultsConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    applyConfigUpdate(update)

  private def updateMotionConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    stateRef.get.flatMap { previousState =>
      applyConfigUpdate(update).flatTap(cancelDisabledMotion(previousState.persisted.config, _))
    }

  private def updateMotionAccessibility(
    accessibility: com.serenity.config.MotionAccessibility
  ): IO[com.serenity.config.AppConfig] =
    updateMotionConfig(_.withMotionAccessibility(accessibility))

  private def updateWindowSitterConfig(
    update: com.serenity.animation.WindowSitterConfig => com.serenity.animation.WindowSitterConfig
  ): IO[Unit] =
    updateAppearanceConfig(config => config.withWindowSitterConfig(update(config.windowSitterConfig))).flatTap { config =>
      stateRef.update { state =>
        val sitter =
          if config.windowSitterConfig.enabled then
            com.serenity.animation.WindowSitter.fromConfig(config.windowSitterConfig)
          else com.serenity.animation.WindowSitter.default
        state.copy(runtime = state.runtime.copy(windowSitter = sitter))
      }
    }.void

  private def cancelActiveMotion(): IO[Unit] =
    clearBufferAnimations() >>
      stateRef.update(state =>
        state.copy(runtime =
          state.runtime.copy(
            themeTransition = None,
            uiSurfaces = state.runtime.uiSurfaces.filterNot(isGhostOverlay),
            surfaceAnimations = Map.empty,
            windowSitter = com.serenity.animation.WindowSitter.default
          )
        )
      )

  private def cancelDisabledMotion(
    previous: com.serenity.config.AppConfig,
    current: com.serenity.config.AppConfig
  ): IO[Unit] =
    val previousFamilies = previous.surfaceConfig.effectiveMotionConfiguration
    val currentFamilies  = current.surfaceConfig.effectiveMotionConfiguration
    if currentFamilies.families.values.forall(!_.enabled) && previousFamilies.families.values.exists(_.enabled) then
      cancelActiveMotion()
    else
      com.serenity.config.MotionFamily.values.toList
        .filter(family => previousFamilies.family(family).enabled && !currentFamilies.family(family).enabled)
        .traverse_(cancelMotionFamily) >>
        IO.whenA(
          !previousFamilies.family(com.serenity.config.MotionFamily.UiTransitions).enabled &&
            currentFamilies.family(com.serenity.config.MotionFamily.UiTransitions).enabled &&
            current.windowSitterConfig.enabled
        )(
          stateRef.update(state =>
            state.copy(runtime =
              state.runtime.copy(windowSitter =
                com.serenity.animation.WindowSitter.fromConfig(current.windowSitterConfig)
              )
            )
          )
        )

  private def cancelMotionFamily(family: com.serenity.config.MotionFamily): IO[Unit] =
    family match
      case com.serenity.config.MotionFamily.EditorText =>
        clearBufferAnimations(com.serenity.animation.AnimationOwner.EditorText)
      case com.serenity.config.MotionFamily.CommandSurfaces =>
        cancelSurfaceMotion(isCommandSurface)
      case com.serenity.config.MotionFamily.PinnedPanels =>
        cancelSurfaceMotion(isDockedSurface)
      case com.serenity.config.MotionFamily.UiTransitions =>
        clearBufferAnimations(com.serenity.animation.AnimationOwner.UiTransitions) >>
          stateRef.update(state =>
            state.copy(runtime =
              state.runtime.copy(
                themeTransition = None,
                windowSitter = com.serenity.animation.WindowSitter.default
              )
            )
          )
      case com.serenity.config.MotionFamily.Cursor =>
        IO.unit

  private def clearBufferAnimations(): IO[Unit] =
    bufferAnimationsRef.update(
      _.view
        .mapValues { animations =>
          val cleared = animations.clearAll()
          if cleared eq animations then animations else cleared
        }
        .toMap
    )

  private def clearBufferAnimations(owner: com.serenity.animation.AnimationOwner): IO[Unit] =
    bufferAnimationsRef.update(
      _.view
        .mapValues { animations =>
          val cleared = animations.clear(owner)
          if cleared eq animations then animations else cleared
        }
        .toMap
    )

  private def cancelSurfaceMotion(matches: UiSurface => Boolean): IO[Unit] =
    stateRef.update { state =>
      val matchingIds = state.runtime.uiSurfaces.collect { case surface if matches(surface) => surface.id }.toSet
      state.copy(runtime =
        state.runtime.copy(
          uiSurfaces = state.runtime.uiSurfaces.filterNot(surface => matches(surface) && isGhostOverlay(surface)),
          surfaceAnimations =
            state.runtime.surfaceAnimations.filterNot((surfaceId, _) => matchingIds.contains(surfaceId))
        )
      )
    }

  private def isCommandSurface(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.CommandPalette(_) | SurfaceContent.CommandPaletteSubmenu(_, _, _) => true
      case SurfaceContent.GhostOverlay(content, _) =>
        content match
          case SurfaceContent.CommandPalette(_) | SurfaceContent.CommandPaletteSubmenu(_, _, _) => true
          case _                                                                                => false
      case _ => false

  /** A transient close-fade ghost, which is discarded rather than animated when motion is cancelled. */
  private def isGhostOverlay(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.GhostOverlay(_, _) => true
      case _                                 => false

  /** A surface occupying a workspace dock, whether at its pinned size or expanded over the editor. */
  private def isDockedSurface(surface: UiSurface): Boolean =
    surface.presentation match
      case SurfacePresentation.Pinned(_, _) | SurfacePresentation.Expanded(_, _) => true
      case _                                                                     => false

  private def updateCustomMotionConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    updateMotionConfig(config => update(config).withCustomMotionBaseline)

  private def updateTextDisplayConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    applyConfigUpdate(update)

  /** Applies a configuration change to live state, persists it, and auto-saves the session. */
  private def applyConfigUpdate(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    stateRef
      .modify { state =>
        val newConfig = update(state.persisted.config)
        val newState =
          withUpdatedRunnerConfig(state.copy(persisted = state.persisted.copy(config = newConfig)), newConfig)
        (newState, newConfig)
      }
      .flatTap(config =>
        configPersistencePath match
          case Some(path) =>
            com.serenity.config.ConfigManager.saveConfigIO(config, path).flatMap {
              case Right(_) => IO.unit
              case Left(error) =>
                logger
                  .warn(error.cause.getOrElse(new RuntimeException(error.message)))(s"[CONFIG] ${error.message}")
            }
          case None =>
            IO.unit
      )
      .flatTap(_ =>
        stateRef.get
          .flatMap(state => sessionPersistence.maybeSaveSession(state, SessionSaveTrigger.Manual))
          .handleErrorWith(error => logger.error(error)("[SESSION] Auto-save after config change failed"))
      )

  private[manager] def updateFontConfig(
    update: com.serenity.ui.fonts.FontLoader.FontConfig => com.serenity.ui.fonts.FontLoader.FontConfig
  ): IO[Unit] =
    deviceTextScaleProvider.flatMap { deviceTextScale =>
      applyConfigUpdate(config =>
        config.withFontConfig(update(config.fontConfig).resolveAutoTextScale(deviceTextScale))
      )
        .flatMap(config => onFontConfigChanged(config.fontConfig))
    }

  protected def updateSpellCheckConfig(
    update: com.serenity.config.SpellCheckConfig => com.serenity.config.SpellCheckConfig
  ): IO[Unit] =
    applyConfigUpdate(config => config.withSpellCheck(update(config.spellCheck))).void >>
      scheduleDocumentAnalysis()

  protected def clampFontSize(size: Float): Float =
    size.max(8.0f).min(48.0f)

  private[manager] def interpretCommand(command: Command, state: AppState): IO[Unit] =
    command.intent match
      case CommandIntent.OpenSettings =>
        val registry = CommandRegistry.withToggleUI
        updateState { current =>
          val opened = AppEventReducer
            .reduce(com.serenity.keystroke.events.ToggleCommandRunner, current, registry)(using balance)
            .state
          opened.commandRunnerSurface match
            case Some(surface) =>
              surface.content match
                case SurfaceContent.CommandPalette(runner) =>
                  opened.copy(runtime = opened.runtime.copy(uiSurfaces = opened.runtime.uiSurfaces.map {
                    case currentSurface if currentSurface.id == surface.id =>
                      currentSurface.copy(content = SurfaceContent.CommandPalette(runner.openSettings))
                    case currentSurface => currentSurface
                  }))
                case _ => opened
            case None => opened
        }
      case CommandIntent.ToggleLineNumbers =>
        updateTextDisplayConfig(config => config.withLineNumbers(!config.showLineNumbers)).void
      case CommandIntent.ToggleGutter =>
        updateTextDisplayConfig(config => config.withGutter(!config.showGutter)).void
      case CommandIntent.ToggleWordWrap =>
        updateTextDisplayConfig(config => config.withWordWrap(!config.wordWrapEnabled)).void
      case CommandIntent.ToggleFocusedTextBody =>
        updateTextDisplayConfig(config => config.withFocusedTextBody(!config.focusedTextBodyEnabled)).void
      case CommandIntent.ToggleContextualToolbar =>
        enqueueEvent(com.serenity.keystroke.events.ToggleContextualToolbar)
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
        persistConfigFile(state.persisted.config)
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
      case CommandIntent.OpenRecentFile(path) =>
        IO.blocking(java.nio.file.Files.isRegularFile(path) && java.nio.file.Files.isReadable(path)).flatMap {
          case true  => directLoadFileEffect(path)
          case false => logger.warn(s"[STARTUP] Recent file is unavailable: $path")
        }
      case CommandIntent.OpenFileSearch =>
        openFileSearchEffect(state)
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
      case CommandIntent.NextTab =>
        updateState(EditorState.navigateToNextBuffer)
      case CommandIntent.PreviousTab =>
        updateState(EditorState.navigateToPreviousBuffer)
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
        enqueueEvent(com.serenity.keystroke.events.Copy)
      case CommandIntent.Cut =>
        enqueueEvent(com.serenity.keystroke.events.Cut)
      case CommandIntent.Paste =>
        enqueueEvent(com.serenity.keystroke.events.Paste)
      case CommandIntent.SelectAll =>
        enqueueEvent(com.serenity.keystroke.events.SelectAll)
      case CommandIntent.Undo =>
        enqueueEvent(com.serenity.keystroke.events.Undo)
      case CommandIntent.Redo =>
        enqueueEvent(com.serenity.keystroke.events.Redo)
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
      case CommandIntent.RequestLspCompletion =>
        requestLspCompletion(state)
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
      case CommandIntent.PinCommentsPanel =>
        setPanelPin(PanelKind.Comments, Some(PanelPosition.Right))
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
      case CommandIntent.SetWindowChromeMode(mode) =>
        updateAppearanceConfig(_.withWindowChromeMode(mode)).void
      case CommandIntent.SetWindowSitterEnabled(enabled) =>
        updateWindowSitterConfig(_.copy(enabled = enabled))
      case CommandIntent.SetWindowSitterAction(action) =>
        updateWindowSitterConfig(_.copy(action = action))
      case CommandIntent.SetWindowSitterFrames(frames) =>
        updateWindowSitterConfig(_.copy(frames = frames))
      case CommandIntent.SetWindowSitterActiveTicks(ticks) =>
        updateWindowSitterConfig(_.copy(activeTicks = ticks))
      case CommandIntent.SetWindowSitterFastActiveTicks(ticks) =>
        updateWindowSitterConfig(_.copy(fastActiveTicks = ticks))
      case CommandIntent.SetWindowSitterFastTypingThresholdMs(ms) =>
        updateWindowSitterConfig(_.copy(fastTypingThresholdMs = ms))
      case CommandIntent.FocusPanel(position) =>
        switchToPinnedPanel(PanelTarget.ByPosition(position))
      case CommandIntent.UnpinPanel(position) =>
        unpinPanel(PanelTarget.ByPosition(position))
      case CommandIntent.ExpandPanel(position) =>
        expandPinnedPanel(PanelTarget.ByPosition(position))
      case CommandIntent.CollapseExpandedPanel =>
        collapseExpandedPanel()
      case CommandIntent.FormatCurrentFile =>
        logger.debug("[CMD] Format command requested")
      case CommandIntent.SetMaterialPreset(preset) =>
        updateAppearanceConfig(_.withMaterialPreset(preset)).void
      case CommandIntent.SetPostProcessingEffect(effect) =>
        updateAppearanceConfig(_.withPostProcessingEffect(effect)).void
      case CommandIntent.SetUiShadowsEnabled(enabled) =>
        updateAppearanceConfig(_.withUiShadowsEnabled(enabled)).void
      case CommandIntent.SetMotionPreset(preset) =>
        updateMotionConfig(_.withMotionPreset(preset)).void
      case CommandIntent.SetMotionAccessibility(accessibility) =>
        updateMotionAccessibility(accessibility).void
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
      case CommandIntent.SetCommandRunnerItemGapRows(rows) =>
        updateAppearanceConfig(_.withCommandRunnerItemGapRows(rows)).void
      case CommandIntent.SetCommandRunnerCursorGapRows(rows) =>
        updateAppearanceConfig(_.withCommandRunnerCursorGapRows(rows)).void
      case CommandIntent.SetRenderFpsTarget(target) =>
        updateAppearanceConfig(_.withRenderFpsTarget(target)).void
      case CommandIntent.SetRenderDamageGranularity(granularity) =>
        updateAppearanceConfig(_.withRenderDamageGranularity(granularity)).void
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
        updateCustomMotionConfig { config =>
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
          config.withEditorTextAnimation(newAnim)
        }.void
      case CommandIntent.SetAnimationSteps(n) =>
        updateCustomMotionConfig { config =>
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
          config.withEditorTextAnimation(newAnim)
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
        updateFontConfig(_.copy(textScaleMode = mode))
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
      case CommandIntent.SaveUiPresetAsNew(name) =>
        saveUiPresetAsNewEffect(name)
      case CommandIntent.OverwriteUiPreset(name) =>
        overwriteUiPresetEffect(name)
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
      case CommandIntent.CancelProjectTask =>
        cancelProjectTask
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
            state.persisted.buffers.get(bufferId) match
              case Some(buffer) =>
                val updateLanguage =
                  updateState(s =>
                    s.copy(persisted =
                      s.persisted.copy(buffers =
                        s.persisted.buffers + (bufferId -> buffer.copy(document =
                          buffer.document.copy(language = language)
                        ))
                      )
                    )
                  )

                val refreshLspBinding =
                  buffer.document.filePath match
                    case Some(path) if buffer.document.language != language =>
                      val uri  = path.toUri.toString
                      val text = buffer.document.content.collect()
                      val closeOld =
                        buffer.document.language.fold(IO.unit)(previous =>
                          lspQueue.enqueue(LspEffect.FileClosed(uri, previous))
                        )
                      val openNew =
                        language.fold(IO.unit)(next => lspQueue.enqueue(LspEffect.FileOpened(uri, next, text)))
                      closeOld >> openNew
                    case _ =>
                      IO.unit

                updateLanguage >> refreshLspBinding
              case None =>
                IO.unit
          case None => IO.unit
      case CommandIntent.SetGlobalHotkey(action, binding) =>
        updateGlobalHotkeyBinding(action, binding)
      case CommandIntent.ResolveGlobalHotkeyConflict(action, binding) =>
        updateConfig(_.withHotkeyOverrideUnbindingConflicts(action, binding)).void
      case CommandIntent.ResolveFocusedKeymapConflict(itemId, binding) =>
        updateConfig(resolveFocusedKeymapConflict(itemId, binding)).void
      case CommandIntent.SetEditorKeyBinding(action, binding) =>
        setKeymapBinding("keymap-editor-", KeymapGroup.Editor)(action, binding)
      case CommandIntent.SetCommandRunnerKeyBinding(action, binding) =>
        setKeymapBinding("keymap-command-runner-", KeymapGroup.CommandRunner)(action, binding)
      case CommandIntent.SetModalKeyBinding(action, binding) =>
        setKeymapBinding("keymap-modal-", KeymapGroup.Modal)(action, binding)
      case CommandIntent.SetPanelKeyBinding(action, binding) =>
        setKeymapBinding("keymap-panel-", KeymapGroup.Panel)(action, binding)
      case CommandIntent.SetPeekKeyBinding(action, binding) =>
        setKeymapBinding("keymap-peek-", KeymapGroup.Peek)(action, binding)
      case CommandIntent.ResetGlobalHotkey(action) =>
        updateConfig(_.resetHotkeyOverride(action)).void
      case CommandIntent.ResetEditorKeyBinding(action) =>
        updateConfig(_.resetKeymapBinding(KeymapGroup.Editor)(action)).void
      case CommandIntent.ResetCommandRunnerKeyBinding(action) =>
        updateConfig(_.resetKeymapBinding(KeymapGroup.CommandRunner)(action)).void
      case CommandIntent.ResetModalKeyBinding(action) =>
        updateConfig(_.resetKeymapBinding(KeymapGroup.Modal)(action)).void
      case CommandIntent.ResetPanelKeyBinding(action) =>
        updateConfig(_.resetKeymapBinding(KeymapGroup.Panel)(action)).void
      case CommandIntent.ResetPeekKeyBinding(action) =>
        updateConfig(_.resetKeymapBinding(KeymapGroup.Peek)(action)).void

  private def setKeymapBinding[A <: com.serenity.config.KeymapEventAction[E], E <: com.serenity.keystroke.events.Event](
    prefix: String,
    group: KeymapGroup[A, E]
  )(action: A, binding: String): IO[Unit] =
    updateKeyBinding(s"$prefix${action.configKey}", binding, _.withKeymapBinding(group)(action, binding))

  private def updateKeyBinding(
    itemId: String,
    binding: String,
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[Unit] =
    stateRef.get.flatMap { state =>
      val updatedConfig = update(state.persisted.config)
      if updatedConfig == state.persisted.config then
        if currentFocusedKeymapOwnsBinding(state.persisted.config, itemId, binding) then IO.unit
        else stateRef.update(withFocusedKeymapConflictMessage(itemId, binding))
      else updateConfig(_ => updatedConfig).void
    }

  private def currentFocusedKeymapOwnsBinding(
    config: com.serenity.config.AppConfig,
    itemId: String,
    binding: String
  ): Boolean =
    HotkeyTrigger
      .parse(binding)
      .exists(trigger =>
        StateManagerEffectHandlers.keymapGroupBindings
          .exists(_.ownsBinding(config.inputConfig.focusedKeymapConfig, itemId, trigger))
      )

  private def resolveFocusedKeymapConflict(
    itemId: String,
    binding: String
  )(config: com.serenity.config.AppConfig): com.serenity.config.AppConfig =
    StateManagerEffectHandlers.keymapGroupBindings
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
      case current @ UiSurface(_, SurfaceContent.CommandPaletteSubmenu(runner, groupId, previewOnly), _, _) =>
        current.copy(
          content = SurfaceContent.CommandPaletteSubmenu(
            withFocusedKeymapConflict(runner, itemId, binding),
            groupId,
            previewOnly
          )
        )
      case current => current
    }))

  private def withFocusedKeymapConflict(runner: CommandRunner, itemId: String, binding: String): CommandRunner =
    runner.copy(
      activeSubmenu = runner.activeSubmenu.map(
        _.copy(
          editingItemId = Some(itemId),
          editingText = binding,
          recordingItemId = None,
          pendingGlobalHotkeyConflict = None,
          pendingFocusedKeymapConflict = Some(itemId -> binding)
        )
      ),
      statusMessage = Some(
        "Binding is already assigned. Enter to unbind the other action, or Escape to preserve it."
      )
    )

  private def withGlobalKeymapConflictMessage(
    action: com.serenity.config.HotkeyAction,
    binding: String
  )(state: AppState): AppState =
    state.commandRunnerSurface match
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
              case current if current.id == surface.id =>
                current.copy(content =
                  SurfaceContent.CommandPalette(
                    runner.copy(
                      activeSubmenu = runner.activeSubmenu.map(
                        _.copy(
                          editingItemId = Some(s"keymap-global-${action.configKey}"),
                          editingText = binding,
                          recordingItemId = None,
                          pendingGlobalHotkeyConflict = Some(action -> binding),
                          pendingFocusedKeymapConflict = None
                        )
                      ),
                      statusMessage = Some(
                        "Binding is already assigned. Enter to unbind the other action, or Escape to preserve it."
                      )
                    )
                  )
                )
              case current =>
                current
            }))
          case _ => state
      case None => state

  private def toggleRichTextMark(
    state: AppState,
    mark: com.serenity.richtext.InlineMark
  ): AppState =
    activeEditorContentBuffer(state) match
      case Some(buffer) =>
        val selections = buffer.allSelections.filter(selection => selection.start != selection.end)
        val text       = buffer.document.content.collect()
        val baseDocument = buffer.richText.richTextDocument
          .filter(_.matchesPlainText(text))
          .getOrElse(com.serenity.richtext.RichTextDocument.fromPlainText(text))
        val insertionStyle =
          if selections.isEmpty then
            buffer.richText.insertionRichTextStyle.getOrElse(RichTextStyle.empty) match
              case style if style.marks.contains(mark) => style.withoutMark(mark)
              case style                               => style.withMark(mark)
          else RichTextStyle.empty
        val updatedDocument =
          if selections.isEmpty then baseDocument
          else
            selections
              .foldLeft(baseDocument)((document, selection) => document.toggleMark(richTextRange(selection), mark))
              .normalized
        state.copy(persisted =
          state.persisted.copy(buffers =
            state.persisted.buffers.updated(
              buffer.id,
              buffer.copy(
                document = buffer.document.copy(isDirty = true, isNewEmpty = false),
                richText = buffer.richText.copy(
                  richTextDocument = Some(updatedDocument),
                  insertionRichTextStyle = Some(insertionStyle)
                )
              )
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
          val text = buffer.document.content.collect()
          val baseDocument = buffer.richText.richTextDocument
            .filter(_.matchesPlainText(text))
            .getOrElse(RichTextDocument.fromPlainText(text))
          val updatedDocument = ranges.foldLeft(baseDocument)(update).normalized
          if updatedDocument == baseDocument.normalized then state
          else
            state.copy(persisted =
              state.persisted.copy(buffers =
                state.persisted.buffers.updated(
                  buffer.id,
                  buffer.copy(
                    document = buffer.document.copy(isDirty = true, isNewEmpty = false),
                    richText = buffer.richText.copy(richTextDocument = Some(updatedDocument))
                  )
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
          val text = buffer.document.content.collect()
          val baseDocument = buffer.richText.richTextDocument
            .filter(_.matchesPlainText(text))
            .getOrElse(RichTextDocument.fromPlainText(text))
          val updatedDocument = ranges.foldLeft(baseDocument)(update).normalized
          if updatedDocument == baseDocument.normalized then state
          else
            state.copy(persisted =
              state.persisted.copy(buffers =
                state.persisted.buffers.updated(
                  buffer.id,
                  buffer.copy(
                    document = buffer.document.copy(isDirty = true, isNewEmpty = false),
                    richText = buffer.richText.copy(richTextDocument = Some(updatedDocument))
                  )
                )
              )
            )
      case None =>
        state

  private def richTextParagraphRanges(buffer: Buffer): List[RichTextRange] =
    val selections = buffer.allSelections.filter(selection => selection.start != selection.end).map(richTextRange)
    if selections.nonEmpty then selections
    else
      buffer.editing.cursors.distinct.map { cursor =>
        RichTextRange(
          start = com.serenity.richtext.RichTextPosition(cursor.line, cursor.column),
          end = com.serenity.richtext.RichTextPosition(cursor.line, cursor.column)
        )
      }

  private def activeEditorContentBuffer(state: AppState): Option[Buffer] =
    state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(state.persisted.buffers.get)

  private def richTextRange(selection: Selection): com.serenity.richtext.RichTextRange =
    com.serenity.richtext.RichTextRange(
      start = com.serenity.richtext.RichTextPosition(selection.start.line, selection.start.column),
      end = com.serenity.richtext.RichTextPosition(selection.end.line, selection.end.column)
    )

  /** Captures the live workspace as a new custom preset, rejecting names that already exist. */
  protected def saveUiPresetAsNewEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")
      case Some(presetName) =>
        capturedPreset(presetName).flatMap { preset =>
          uiPresetStore.create(preset).attempt.flatMap {
            case Left(error) =>
              reportPresetFailure(presetName, s"Could not save $presetName", error)
            case Right(_) =>
              refreshCommandRunnerUiPresetPreviews >>
                focusCreatedPresetOptions(presetName, s"Preset saved. Configure $presetName.")
          }
        }

  /** Overwrites an existing custom preset with the live workspace; the last write wins. */
  protected def overwriteUiPresetEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")
      case Some(presetName) if UiPreset.builtIn(presetName).nonEmpty =>
        updateCommandRunnerPresetContext(
          Some(presetName),
          s"Built-in preset cannot be overwritten. Duplicate $presetName first."
        )
      case Some(presetName) =>
        uiPresetStore
          .find(presetName)
          .flatMap {
            case None =>
              updateCommandRunnerPresetContext(
                Some(presetName),
                s"Custom preset '$presetName' was not found. Use Save As New Preset."
              )
            case Some(existing) =>
              capturedPreset(existing.name).flatMap { preset =>
                uiPresetStore.upsert(preset).attempt.flatMap {
                  case Left(error) =>
                    reportPresetFailure(existing.name, s"Could not save ${existing.name}", error)
                  case Right(_) =>
                    refreshCommandRunnerUiPresetPreviews >>
                      updateCommandRunnerPresetContext(
                        Some(existing.name),
                        s"Preset overwritten. Configure ${existing.name}."
                      )
                }
              }
          }
          .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to overwrite UI preset $presetName"))

  private def capturedPreset(presetName: String): IO[UiPreset] =
    for
      state <- stateRef.get
      windowSize <- windowSizeProvider.handleErrorWith(error =>
        logger.error(error)("[PRESET] Window size capture failed").as(None)
      )
    yield UiPreset.capture(presetName, state, windowSize)

  private def reportPresetFailure(presetName: String, summary: String, error: Throwable): IO[Unit] =
    logger.error(error)(s"[PRESET] $summary") >>
      updateCommandRunnerPresetContext(
        Some(presetName),
        s"$summary: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}"
      )

  protected def applyUiPresetEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")
      case Some(presetName) =>
        uiPresetStore
          .find(presetName)
          .map { customPreset =>
            customPreset
              .map(_ -> false)
              .orElse(UiPreset.builtIn(presetName).map(_ -> true))
          }
          .flatMap {
            case None =>
              logger.warn(s"[PRESET] UI preset not found: $presetName")
            case Some((preset, isBuiltInWorkflow)) =>
              loadUiPresetResources(preset).flatMap {
                case Left(reason) =>
                  rejectUiPresetPreview(presetName, reason)
                case Right(theme) =>
                  for
                    appliedConfig <- stateRef.modify { state =>
                      val restoredPresetState =
                        if isBuiltInWorkflow then UiPreset.applyBuiltInWorkflowToState(preset, state, theme)
                        else UiPreset.applyToState(preset, state, theme)
                      val restoredDocumentState =
                        applyPresetDocumentModeToActiveEmptyBuffer(
                          restoredPresetState,
                          preset.config.defaultDocumentMode
                        )
                      val restoredOutlineState = hydratePresetSymbolPanels(restoredDocumentState)
                      val restored =
                        withUpdatedRunnerConfig(restoredOutlineState, restoredOutlineState.persisted.config)
                      (restored, restored.persisted.config)
                    }
                    _ <- persistConfigFile(appliedConfig)
                    _ <- onFontConfigChanged(appliedConfig.fontConfig)
                      .handleErrorWith(error => logger.error(error)("[PRESET] Failed to apply preset font config"))
                    _ <- reloadPresetDirectories(preset)
                    _ <- openPresetMarkdownPreviewIfNeeded(preset)
                    _ <- stateRef.get
                      .flatMap(state => sessionPersistence.maybeSaveSession(state, SessionSaveTrigger.Manual))
                      .handleErrorWith(error => logger.error(error)("[SESSION] Auto-save after preset apply failed"))
                  yield ()
              }
          }
          .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to apply UI preset $presetName"))

  private def applyPresetDocumentModeToActiveEmptyBuffer(state: AppState, mode: DefaultDocumentMode): AppState =
    state.focusedBufferId.flatMap(state.persisted.buffers.get) match
      case Some(buffer)
          if buffer.document.isNewEmpty && buffer.document.content.weight == 0 && buffer.document.filePath.isEmpty =>
        val updatedBuffer =
          mode match
            case DefaultDocumentMode.PlainText =>
              buffer.copy(
                document = buffer.document.copy(language = None),
                richText = buffer.richText.copy(richTextDocument = None)
              )
            case DefaultDocumentMode.Markdown =>
              buffer.copy(
                document = buffer.document.copy(language = Some(LanguageId.Markdown)),
                richText = buffer.richText.copy(richTextDocument = None)
              )
            case DefaultDocumentMode.RichText =>
              buffer.copy(
                document = buffer.document.copy(language = None),
                richText = buffer.richText.copy(richTextDocument = Some(RichTextDocument.fromPlainText("")))
              )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> updatedBuffer)))
      case _ =>
        state

  private def hydratePresetSymbolPanels(state: AppState): AppState =
    val outlineSymbolsList = outlineSymbols(state)
    val outlineActive      = currentSymbolActiveLocation(outlineSymbolsList, state)
    val commentSymbolsList = commentPanelSymbols(state)
    val commentActive      = currentSymbolActiveLocation(commentSymbolsList, state)
    val hydratedSurfaces = state.runtime.uiSurfaces.map {
      case surface @ UiSurface(_, SurfaceContent.Outline(_, _), SurfacePresentation.Pinned(_, _), _) =>
        surface.copy(content = SurfaceContent.Outline(outlineSymbolsList, outlineActive))
      case surface @ UiSurface(_, SurfaceContent.Comments(_, _), SurfacePresentation.Pinned(_, _), _) =>
        surface.copy(content = SurfaceContent.Comments(commentSymbolsList, commentActive))
      case surface =>
        surface
    }
    state.copy(runtime = state.runtime.copy(uiSurfaces = hydratedSurfaces))

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
              uiPresetStore.create(preset.copy(name = target)).attempt.flatMap {
                case Left(error) =>
                  reportPresetFailure(target, s"Could not duplicate $source", error)
                case Right(_) =>
                  refreshCommandRunnerUiPresetPreviews >>
                    updateCommandRunnerPresetContext(Some(target), s"Preset duplicated. Configure $target.")
              }
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
    Option(UiPreset.normalizedName(name)).filter(_.nonEmpty)

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
            val updatedSurfaces = state.runtime.uiSurfaces.map {
              case current if current.id == surface.id =>
                current.copy(content = SurfaceContent.CommandPalette(updatedRunner))
              case current @ UiSurface(_, SurfaceContent.CommandPaletteSubmenu(_, groupId, previewOnly), _, _) =>
                current.copy(content = SurfaceContent.CommandPaletteSubmenu(updatedRunner, groupId, previewOnly))
              case current =>
                current
            }
            state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))
          case _ =>
            state
      case None =>
        state

  private def loadUiPresetResources(preset: UiPreset): IO[Either[String, Theme]] =
    FontLoader.missingFamilies(preset.config.fontConfig) match
      case missing :: _ => IO.pure(Left(s"Preset requires unavailable $missing."))
      case Nil =>
        themeManager.loadTheme(preset.themeName).attempt.map {
          case Right(theme) => Right(theme)
          case Left(error) =>
            val detail = Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
            Left(s"Theme '${preset.themeName}' could not be loaded: $detail")
        }

  private def rejectUiPresetPreview(name: String, reason: String): IO[Unit] =
    logger.warn(s"[PRESET] Cannot preview UI preset '$name': $reason") >>
      updateCommandRunnerPresetContext(Some(name), s"Cannot preview $name: $reason")

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
            val updatedSurfaces = state.runtime.uiSurfaces.map {
              case current if current.id == surface.id =>
                current.copy(content = SurfaceContent.CommandPalette(updatedRunner))
              case current @ UiSurface(_, SurfaceContent.CommandPaletteSubmenu(_, groupId, previewOnly), _, _) =>
                current.copy(content = SurfaceContent.CommandPaletteSubmenu(updatedRunner, groupId, previewOnly))
              case current =>
                current
            }
            state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))
          case _ =>
            state
      case None =>
        state

  private def focusCreatedPresetOptions(name: String, statusMessage: String): IO[Unit] =
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
                statusMessage = Some(statusMessage)
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
              val updatedSurfaces = state.runtime.uiSurfaces
                .filterNot(_.id == CommandRunnerSubmenuSurfaceId)
                .map {
                  case current if current.id == surface.id =>
                    current.copy(content = SurfaceContent.CommandPalette(updatedRunner))
                  case current =>
                    current
                } :+ submenuSurface
              state.copy(
                persisted = state.persisted.copy(focus = Focus.Surface(CommandRunnerSubmenuSurfaceId)),
                runtime = state.runtime.copy(uiSurfaces = updatedSurfaces)
              )
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
          projectTaskSemaphore.permit.use { _ =>
            projectTaskFiberRef.get.flatMap {
              case Some(_) =>
                pinProjectTerminal(
                  "A project task is already running. Use Cancel Project Task before starting another."
                )
              case None =>
                for
                  outputRef <- Ref.of[IO, String]("")
                  finished  <- Deferred[IO, Unit]
                  startTask <- Deferred[IO, Unit]
                  renderer <- Stream
                    .awakeEvery[IO](100.millis)
                    .evalMap(_ =>
                      outputRef.get.flatMap(output => pinProjectTerminal(ProjectTaskTerminal.running(command, output)))
                    )
                    .interruptWhen(Stream.eval(finished.get).as(true))
                    .compile
                    .drain
                    .start
                  task = startTask.get >> ProjectTaskRunner
                    .runStreaming(command)(chunk =>
                      outputRef.update(output => ProjectTaskRunner.appendOutputTail(output, chunk))
                    )
                    .attempt
                    .flatMap {
                      case Right(result) => pinProjectTerminal(ProjectTaskTerminal.completed(result))
                      case Left(error)   => pinProjectTerminal(ProjectTaskTerminal.failedToStart(command, error))
                    }
                    .guarantee(
                      finished.complete(()).attempt.void >> renderer.joinWithNever >> ProjectTaskOwnership
                        .clear(projectTaskFiberRef, finished)
                    )
                  fiber <- (pinProjectTerminal(ProjectTaskTerminal.started(command)) >> task).start
                  _     <- projectTaskFiberRef.set(Some(ManagedProjectTask(finished, fiber)))
                  _     <- startTask.complete(())
                yield ()
            }
          }
    }

  private def projectTaskStartPath(state: AppState): IO[Path] =
    state.focusedBufferId
      .flatMap(state.persisted.buffers.get)
      .flatMap(_.document.filePath)
      .fold(FileUtils.getCurrentDirectory)(path => IO.pure(path))

  private def pinProjectTerminal(text: String): IO[Unit] =
    pinPanel(PanelContent.Terminal(text, text.length), PanelPosition.Bottom, 14)

  private def cancelProjectTask: IO[Unit] =
    ProjectTaskOwnership.cancel(projectTaskFiberRef, projectTaskSemaphore).flatMap {
      case true  => pinProjectTerminal("Project task cancelled.")
      case false => pinProjectTerminal("No project task is running.")
    }

  private def requestLspHover(state: AppState): IO[Unit] =
    activeLspRequestTarget(state) match
      case Some((uri, languageId, cursor, _)) =>
        lspQueue.enqueue(LspEffect.HoverRequested(uri, languageId, cursor.line, cursor.column, cursor))
      case None =>
        showLspUnavailablePeek(state)

  private def requestLspCompletion(state: AppState): IO[Unit] =
    activeLspRequestTarget(state) match
      case Some((uri, languageId, cursor, _)) =>
        lspQueue.enqueue(LspEffect.CompletionRequested(uri, languageId, cursor.line, cursor.column, cursor))
      case None =>
        showLspUnavailablePeek(state)

  private def requestLspDefinition(state: AppState): IO[Unit] =
    activeLspRequestTarget(state) match
      case Some((uri, languageId, cursor, buffer)) =>
        lspQueue.enqueue(
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
      buffer     <- state.persisted.buffers.get(bufferId)
      path       <- buffer.document.filePath
      languageId <- buffer.document.language
      cursor     <- buffer.editing.cursors.headOption
    yield (path.toUri.toString, languageId, cursor, buffer)

  private def showLspUnavailablePeek(state: AppState): IO[Unit] =
    showPeek(
      PeekContent.QuickInfo("LSP requests need a saved buffer with a language mode."),
      state.activeCursorPosition.getOrElse(CursorPosition(0, 0))
    )

  private def wordAtCursor(buffer: Buffer, cursor: CursorPosition): String =
    val line = buffer.document.content.getLine(cursor.line).getOrElse("")
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
        com.serenity.config.ConfigManager.saveConfigIO(config, path).flatMap {
          case Right(_) => IO.unit
          case Left(error) =>
            logger.warn(error.cause.getOrElse(new RuntimeException(error.message)))(s"[CONFIG] ${error.message}")
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
        CommentRendering.activeEditorComment(state) match
          case Some(_) =>
            updateState(CommentRendering.openLensAtCursor)
          case None =>
            logger.debug("[CMD] Comment lens requested without an active comment")

  private def dismissCommentLens(state: AppState): AppState =
    state
      .copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(isCommentLensSurface)))
      .popFocus

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
    updateEffect >> refreshCommandRunnerPanelSelections

  private def movePanelKind(kind: PanelKind, delta: Int): IO[Unit] =
    updatePanelState(reorderPanelKind(kind, delta))

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
              SurfaceContent.Outline(symbols, currentSymbolActiveLocation(symbols, state)),
              position,
              defaultPanelSize(kind, position)
            )
          )
        }
      case PanelKind.Comments =>
        stateRef.get.flatMap { state =>
          val symbols = commentPanelSymbols(state)
          updatePanelState(
            upsertPanelKind(
              kind,
              SurfaceContent.Comments(symbols, currentSymbolActiveLocation(symbols, state)),
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
      .flatMap(state.persisted.buffers.get)
      .filter(_.document.language.contains(LanguageId.Markdown))
      .map { buffer =>
        val title = buffer.document.filePath
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
    val removedIds = state.runtime.uiSurfaces.collect {
      case surface if panelKindOf(surface.content).contains(kind) => surface.id
    }.toSet
    val nextFocus = state.persisted.focus match
      case Focus.Surface(surfaceId) if removedIds.contains(surfaceId) =>
        state.persisted.layout.activeEditorPaneId.map(Focus.EditorPane.apply).getOrElse(state.persisted.focus)
      case _ =>
        state.persisted.focus
    state.copy(
      persisted = state.persisted.copy(focus = nextFocus),
      runtime =
        state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.filterNot(surface => removedIds.contains(surface.id)))
    )

  private def upsertPanelKind(
    kind: PanelKind,
    content: SurfaceContent,
    position: PanelPosition,
    size: Int
  )(state: AppState): AppState =
    val matchingSurfaces = state.runtime.uiSurfaces.filter(surface => panelKindOf(surface.content).contains(kind))
    val retainedSurface  = matchingSurfaces.reverse.headOption
    val stateWithoutKind = state.copy(runtime =
      state.runtime.copy(uiSurfaces =
        state.runtime.uiSurfaces.filterNot(surface => panelKindOf(surface.content).contains(kind))
      )
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
    val nextFocus = state.persisted.focus match
      case Focus.Surface(surfaceId) if matchingSurfaces.exists(_.id == surfaceId) => Focus.Surface(surface.id)
      case _                                                                      => state.persisted.focus
    stateWithId.copy(
      persisted = stateWithId.persisted.copy(focus = nextFocus),
      runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces :+ surface)
    )

  private def reorderPanelKind(kind: PanelKind, delta: Int)(state: AppState): AppState =
    if delta == 0 then state
    else
      val pinnedPanels = state.runtime.uiSurfaces.collect {
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
            val updatedSurfaces = state.runtime.uiSurfaces.map { surface =>
              if sameEdge.exists(_.id == surface.id) then replacements.next()
              else surface
            }
            state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))

  private def moveWithinList[A](values: List[A], from: Int, to: Int): List[A] =
    if from == to then values
    else
      values.lift(from) match
        case None => values
        case Some(value) =>
          val withoutValue = values.patch(from, Nil, 1)
          withoutValue.patch(to, List(value), 0)

  private def newestPanelKindSurface(kind: PanelKind, state: AppState): Option[UiSurface] =
    state.runtime.uiSurfaces.reverse.find(surface => panelKindOf(surface.content).contains(kind))

  private def panelKindOf(content: SurfaceContent): Option[PanelKind] =
    content match
      case SurfaceContent.DirectoryTree(_, _)   => Some(PanelKind.Explorer)
      case SurfaceContent.Outline(_, _)         => Some(PanelKind.Outline)
      case SurfaceContent.Comments(_, _)        => Some(PanelKind.Comments)
      case SurfaceContent.Diagnostics(_, _)     => Some(PanelKind.Diagnostics)
      case SurfaceContent.MarkdownPreview(_, _) => Some(PanelKind.MarkdownPreview)
      case _                                    => None

  private def defaultPanelSize(kind: PanelKind, position: PanelPosition): Int =
    kind match
      case PanelKind.MarkdownPreview => 40
      case PanelKind.Diagnostics =>
        position match
          case PanelPosition.Top | PanelPosition.Bottom => 10
          case PanelPosition.Left | PanelPosition.Right => 30
      case PanelKind.Explorer | PanelKind.Outline | PanelKind.Comments =>
        position match
          case PanelPosition.Top | PanelPosition.Bottom => 10
          case PanelPosition.Left | PanelPosition.Right => 30

  private def refreshCommandRunnerPanelSelections: IO[Unit] =
    stateRef.update { state =>
      val selections = CommandRunnerPanelSelections.fromState(state)
      val updatedSurfaces = state.runtime.uiSurfaces.map {
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
      state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))
    }

  private def outlineSymbols(state: AppState): List[Symbol] =
    state.focusedBufferId
      .flatMap(state.persisted.buffers.get)
      .map(outlineSymbolsForBuffer)
      .getOrElse(Nil)

  private def outlineSymbolsForBuffer(buffer: Buffer): List[Symbol] =
    (
      DocumentOutline.forBuffer(buffer) ++
        DocumentNavigation.bookmarkSymbols(buffer.annotations.bookmarks)
    )
      .sortBy(symbol => (symbol.location.line, symbol.location.column, symbol.name))

  private def currentSymbolActiveLocation(symbols: List[Symbol], state: AppState): Option[Location] =
    state.activeCursorPosition
      .flatMap(cursor => DocumentNavigation.currentSymbol(symbols, cursor))
      .map(_.location)

  private def commentPanelSymbols(state: AppState): List[Symbol] =
    state.focusedBufferId
      .flatMap(state.persisted.buffers.get)
      .map(buffer => DocumentNavigation.commentSymbols(buffer.annotations.documentComments))
      .getOrElse(Nil)

  private def navigateDocumentSymbol(
    state: AppState,
    chooseSymbol: (List[Symbol], CursorPosition) => Option[Symbol]
  ): IO[Unit] =
    navigateSymbols(state, outlineSymbolsForBuffer, chooseSymbol, "Document symbol")

  private def navigateBookmark(
    state: AppState,
    chooseSymbol: (List[Symbol], CursorPosition) => Option[Symbol]
  ): IO[Unit] =
    navigateSymbols(
      state,
      buffer => DocumentNavigation.bookmarkSymbols(buffer.annotations.bookmarks),
      chooseSymbol,
      "Bookmark"
    )

  private def navigateDocumentComment(
    state: AppState,
    chooseSymbol: (List[Symbol], CursorPosition) => Option[Symbol]
  ): IO[Unit] =
    navigateSymbols(
      state,
      buffer => DocumentNavigation.commentSymbols(buffer.annotations.documentComments),
      chooseSymbol,
      "Document comment",
      onTargetResolved = Some(CommentRendering.openLensAtCursor)
    )

  private def navigateSymbols(
    state: AppState,
    symbolsForBuffer: Buffer => List[Symbol],
    chooseSymbol: (List[Symbol], CursorPosition) => Option[Symbol],
    label: String,
    onTargetResolved: Option[AppState => AppState] = None
  ): IO[Unit] =
    activeEditorBuffer(state)
      .flatMap {
        case (paneId, buffer) =>
          val cursor  = buffer.editing.cursors.headOption.getOrElse(CursorPosition(0, 0))
          val symbols = symbolsForBuffer(buffer)
          chooseSymbol(symbols, cursor).map { symbol =>
            val before = NavigationPoint(paneId, buffer.id, cursor)
            val after = NavigationPoint(paneId, buffer.id, CursorPosition(symbol.location.line, symbol.location.column))
            before -> after
          }
      } match
      case Some((before, after)) if before != after =>
        val sweep = navigationSweep(before, after)
        stateRef.modify { current =>
          val movedBase = moveToNavigationPoint(current, after)
          val moved = movedBase.copy(runtime =
            movedBase.runtime.copy(navigation =
              NavigationHistory(
                backStack = pushNavigationPoint(before, current.runtime.navigation.backStack),
                forwardStack = Nil
              )
            )
          )
          val animationUpdate = applyNavigationAnimationUpdate(animationUpdateForNavigationTarget(moved, after, sweep))
          (onTargetResolved.fold(moved)(_(moved)), animationUpdate)
        }.flatten
      case Some(_) =>
        onTargetResolved match
          case Some(transform) => updateState(transform)
          case None            => logger.debug(s"[CMD] $label navigation requested for the current location")
      case None =>
        logger.debug(s"[CMD] $label navigation requested without a target")

  private def navigationSweep(before: NavigationPoint, after: NavigationPoint): com.serenity.animation.SweepDirection =
    if after.cursor.line < before.cursor.line ||
        (after.cursor.line == before.cursor.line && after.cursor.column < before.cursor.column)
    then com.serenity.animation.SweepDirection.Backward
    else com.serenity.animation.SweepDirection.Forward

  private def animationUpdateForNavigationTarget(
    state: AppState,
    point: NavigationPoint,
    sweep: com.serenity.animation.SweepDirection
  ): Option[(BufferId, com.serenity.animation.AnimationState => com.serenity.animation.AnimationState)] =
    state.persisted.buffers.get(point.bufferId).flatMap { buffer =>
      val cells = VisibleBufferAnimationCells.fromBuffer(
        buffer,
        state.persisted.config.wordWrapEnabled,
        state.persisted.theme.background,
        state.persisted.theme.foreground
      )

      if cells.isEmpty then None
      else
        state.persisted.config.scaledUiAnimation.map { config =>
          val animated = com.serenity.animation.FlowAnimationBuilder.build(
            cells,
            com.serenity.animation.FlowDirection.ByRow,
            sweep,
            config.steps
          )
          val uiAnimations =
            animated.view.mapValues(_.copy(owner = com.serenity.animation.AnimationOwner.UiTransitions)).toMap
          point.bufferId -> ((animations: com.serenity.animation.AnimationState) =>
            animations
              .clear(com.serenity.animation.AnimationOwner.UiTransitions)
              .mergeUiTransitionAnimations(uiAnimations)
          )
        }
    }

  private def applyNavigationAnimationUpdate(
    update: Option[(BufferId, com.serenity.animation.AnimationState => com.serenity.animation.AnimationState)]
  ): IO[Unit] =
    update.fold(IO.unit) {
      case (bufferId, f) =>
        bufferAnimationsRef.update(map =>
          map.updated(bufferId, f(map.getOrElse(bufferId, com.serenity.animation.AnimationState.empty)))
        )
    }

  private def updateNavigationHistory(
    state: AppState,
    target: NavigationPoint,
    backStack: List[NavigationPoint],
    forwardStack: List[NavigationPoint],
    sweep: com.serenity.animation.SweepDirection
  ): (AppState, IO[Unit]) =
    val movedBase = moveToNavigationPoint(state, target)
    val moved = movedBase.copy(runtime =
      movedBase.runtime.copy(navigation = NavigationHistory(backStack = backStack, forwardStack = forwardStack))
    )
    (moved, applyNavigationAnimationUpdate(animationUpdateForNavigationTarget(moved, target, sweep)))

  private def navigateHistoryBack(): IO[Unit] =
    stateRef.modify { current =>
      current.runtime.navigation.backStack match
        case target :: remaining =>
          currentNavigationPoint(current) match
            case Some(point) =>
              updateNavigationHistory(
                current,
                target,
                remaining,
                pushNavigationPoint(point, current.runtime.navigation.forwardStack),
                navigationSweep(point, target)
              )
            case None => (current, IO.unit)
        case Nil => (current, IO.unit)
    }.flatten

  private def navigateHistoryForward(): IO[Unit] =
    stateRef.modify { current =>
      current.runtime.navigation.forwardStack match
        case target :: remaining =>
          currentNavigationPoint(current) match
            case Some(point) =>
              updateNavigationHistory(
                current,
                target,
                pushNavigationPoint(point, current.runtime.navigation.backStack),
                remaining,
                navigationSweep(point, target)
              )
            case None => (current, IO.unit)
        case Nil => (current, IO.unit)
    }.flatten

  private def currentNavigationPoint(state: AppState): Option[NavigationPoint] =
    activeEditorBuffer(state).flatMap {
      case (paneId, buffer) =>
        buffer.editing.cursors.headOption.map(cursor => NavigationPoint(paneId, buffer.id, cursor))
    }

  private def pushNavigationPoint(point: NavigationPoint, stack: List[NavigationPoint]): List[NavigationPoint] =
    stack match
      case head :: _ if head == point => stack
      case _                          => point :: stack

  private def moveToNavigationPoint(state: AppState, point: NavigationPoint): AppState =
    (state.persisted.layout.editorPanes.get(point.paneId), state.persisted.buffers.get(point.bufferId)) match
      case (Some(pane), Some(buffer)) =>
        val viewport = CursorViewport.adjustForCursor(buffer, state, point.cursor)
        val updatedBuffer = buffer.copy(
          editing = buffer.editing.copy(
            cursors = List(point.cursor),
            selection = None,
            selections = Nil,
            preferredColumn = Some(point.cursor.column),
            preferredXPx = None,
            multiCursorVerticalStates = Nil
          ),
          viewport = viewport
        )
        state.copy(persisted =
          state.persisted.copy(
            buffers = state.persisted.buffers + (point.bufferId -> updatedBuffer),
            layout = state.persisted.layout.copy(
              editorPanes =
                state.persisted.layout.editorPanes + (point.paneId -> pane.copy(bufferId = Some(point.bufferId))),
              activeEditorPaneId = Some(point.paneId)
            ),
            focus = Focus.EditorPane(point.paneId)
          )
        )
      case _ => state

  private def toggleBookmark(state: AppState): IO[Unit] =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val cursor = buffer.editing.cursors.headOption.getOrElse(CursorPosition(0, 0))
        updateState { current =>
          current.persisted.buffers.get(buffer.id) match
            case Some(currentBuffer) =>
              val bookmarks =
                if currentBuffer.annotations.bookmarks.contains(cursor) then
                  currentBuffer.annotations.bookmarks.filterNot(_ == cursor)
                else
                  (cursor :: currentBuffer.annotations.bookmarks).distinct
                    .sortBy(position => (position.line, position.column))

              current.copy(persisted =
                current.persisted.copy(buffers =
                  current.persisted.buffers + (buffer.id ->
                    currentBuffer.copy(annotations = currentBuffer.annotations.copy(bookmarks = bookmarks)))
                )
              )
            case None => current
        }
      case None =>
        logger.debug("[CMD] Toggle bookmark requested without an active editor buffer")

  private def addDocumentComment(state: AppState, text: String): IO[Unit] =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val cursor           = buffer.editing.cursors.headOption.getOrElse(CursorPosition(0, 0))
        val normalizedCursor = snapCursorAfterGrapheme(buffer, cursor)
        val range = buffer.primarySelection
          .map(selection => normalizedCommentSelectionRange(buffer, selection))
          .getOrElse(normalizedCursor -> normalizedCursor)
        val commentText = Option(text.trim).filter(_.nonEmpty).getOrElse("Comment")
        val comment     = DocumentComment(range._1, range._2, commentText)
        updateState: current =>
          current.persisted.buffers.get(buffer.id) match
            case Some(currentBuffer) =>
              val existingCommentAtCursor =
                currentBuffer.annotations.documentComments.find(_.contains(normalizedCursor))
              val updatedComment = existingCommentAtCursor
                .map(existing => existing.copy(text = commentText))
                .getOrElse(comment)
              val comments = (updatedComment :: currentBuffer.annotations.documentComments.filterNot(existing =>
                existingCommentAtCursor.contains(existing) ||
                  (existing.start == comment.start && existing.end == comment.end)
              )).sortBy(existing => (existing.start.line, existing.start.column, existing.text))
              current.copy(persisted =
                current.persisted.copy(buffers =
                  current.persisted.buffers + (buffer.id ->
                    currentBuffer.copy(
                      annotations = currentBuffer.annotations.copy(documentComments = comments),
                      document = currentBuffer.document.copy(isDirty = true)
                    ))
                )
              )
            case None => current
      case None =>
        logger.debug("[CMD] Add document comment requested without an active editor buffer")

  private def normalizedCommentSelectionRange(
    buffer: Buffer,
    selection: Selection
  ): (CursorPosition, CursorPosition) =
    val startOffset = buffer.document.content.lineColumnToOffset(selection.start.line, selection.start.column)
    val endOffset   = buffer.document.content.lineColumnToOffset(selection.end.line, selection.end.column)
    if startOffset >= endOffset then
      val cursor = offsetToCursorPosition(buffer, graphemeBoundaryAfterOrAt(buffer, startOffset))
      cursor -> cursor
    else
      offsetToCursorPosition(buffer, graphemeBoundaryBeforeOrAt(buffer, startOffset)) ->
        offsetToCursorPosition(buffer, graphemeBoundaryAfterOrAt(buffer, endOffset))

  private def snapCursorAfterGrapheme(buffer: Buffer, cursor: CursorPosition): CursorPosition =
    val offset = buffer.document.content.lineColumnToOffset(cursor.line, cursor.column)
    offsetToCursorPosition(buffer, graphemeBoundaryAfterOrAt(buffer, offset))

  private def offsetToCursorPosition(buffer: Buffer, offset: Int): CursorPosition =
    val (line, column) = buffer.document.content.offsetToLineColumn(offset)
    CursorPosition(line, column)

  private def graphemeBoundaryBeforeOrAt(buffer: Buffer, offset: Int): Int =
    TextEditing.graphemeBoundaryBeforeOrAt(RopeCharacterSource(buffer.document.content), offset)

  private def graphemeBoundaryAfterOrAt(buffer: Buffer, offset: Int): Int =
    TextEditing.graphemeBoundaryAfterOrAt(RopeCharacterSource(buffer.document.content), offset)

  final private case class RopeCharacterSource(content: com.serenity.rope.Rope) extends TextEditing.CharacterSource:
    override def length: Int =
      content.weight

    override def charAt(index: Int): Char =
      content.index(index).getOrElse('\u0000')

  private def deleteDocumentComment(state: AppState): IO[Unit] =
    activeEditorBuffer(state) match
      case Some((_, buffer)) =>
        val cursor = buffer.editing.cursors.headOption.getOrElse(CursorPosition(0, 0))
        updateState: current =>
          current.persisted.buffers.get(buffer.id) match
            case Some(currentBuffer) =>
              val comments = currentBuffer.annotations.documentComments.filterNot(_.contains(cursor))
              current.copy(persisted =
                current.persisted.copy(buffers =
                  current.persisted.buffers + (buffer.id ->
                    currentBuffer.copy(
                      annotations = currentBuffer.annotations.copy(documentComments = comments),
                      document = currentBuffer.document.copy(
                        isDirty =
                          currentBuffer.document.isDirty || comments != currentBuffer.annotations.documentComments
                      )
                    ))
                )
              )
            case None => current
      case None =>
        logger.debug("[CMD] Delete document comment requested without an active editor buffer")

  private def activeEditorBuffer(state: AppState): Option[(PaneId, Buffer)] =
    for
      paneId   <- state.persisted.layout.activeEditorPaneId
      pane     <- state.persisted.layout.editorPanes.get(paneId)
      bufferId <- pane.bufferId
      buffer   <- state.persisted.buffers.get(bufferId)
    yield (paneId, buffer)

  private def setMarkdownViewMode(mode: MarkdownViewMode): IO[Unit] =
    val updateConfigEffect = updateConfig(_.withMarkdownViewMode(mode)).void
    val updateModeEffect = mode match
      case MarkdownViewMode.SplitPreview =>
        updateConfigEffect >> openMarkdownPreview
      case MarkdownViewMode.Source | MarkdownViewMode.InlineLens =>
        updateConfigEffect >> unpinMarkdownPreviewPanel()
    updateModeEffect

  private def unpinMarkdownPreviewPanel(): IO[Unit] =
    updateState { state =>
      val markdownPreviewSurfaceIds = state.pinnedSurfaces.collect {
        case UiSurface(id, SurfaceContent.MarkdownPreview(_, _), SurfacePresentation.Pinned(_, _), _) => id
      }.toSet
      val nextFocus = state.persisted.focus match
        case Focus.Surface(surfaceId) if markdownPreviewSurfaceIds.contains(surfaceId) =>
          state.persisted.layout.activeEditorPaneId.map(Focus.EditorPane.apply).getOrElse(state.persisted.focus)
        case _ =>
          state.persisted.focus
      state.copy(
        persisted = state.persisted.copy(focus = nextFocus),
        runtime = state.runtime.copy(uiSurfaces =
          state.runtime.uiSurfaces.filterNot(surface => markdownPreviewSurfaceIds.contains(surface.id))
        )
      )
    }

  protected def pinExplorerPanelEffect(position: PanelPosition, path: Path, size: Int): IO[Unit] =
    for
      fileEntries <- fileManager.getFileBrowser.listDirectory(path)
      dirEntries = toDirEntries(fileEntries)
      _ <- enqueueEvent(
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
      _ <- enqueueEvent(ExplorerEvent.DirectoryLoaded(position, path, dirEntries))
    yield ()).handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to load directory $path"))

  private def toDirEntries(entries: List[FileEntry]): List[DirEntry] =
    entries.map(entry => DirEntry(entry.path, entry.name, entry.isDirectory))

  private[manager] def directLoadFileEffect(path: Path): IO[Unit] =
    IO.blocking(FileUtils.isReadableFile(path)).flatMap {
      case false => logger.debug(s"[FILE] DirectLoad: file not readable: $path")
      case true =>
        stateRef
          .modify { state =>
            val bufferId = state.runtime.nextBufferId
            (state.copy(runtime = state.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))), bufferId)
          }
          .flatMap(bufferId => fileManager.loadFile(path, bufferId))
          .flatMap { loadedBuffer =>
            stateRef.modify { state =>
              val newBufferId = loadedBuffer.id
              val stateWithBuffer = state.copy(persisted =
                state.persisted.copy(buffers = state.persisted.buffers + (newBufferId -> loadedBuffer))
              )
              val updatedState = EditorState.insertBufferInOrder(stateWithBuffer, newBufferId)
              val rebalanced   = EditorState.rebalancePanes(updatedState, Some(newBufferId))
              val focused      = EditorState.focusBuffer(rebalanced, newBufferId)
              val resized =
                focused.runtime.viewportSize
                  .map(viewportSize => LayoutEngine.syncViewportDimensions(focused, viewportSize))
                  .getOrElse(focused)
              (resized, loadedBuffer)
            }
          }
          .flatTap { loadedBuffer =>
            loadedBuffer.document.language match
              case Some(languageId) =>
                val uri  = path.toUri.toString
                val text = loadedBuffer.document.content.collect()
                lspQueue.enqueue(LspEffect.FileOpened(uri, languageId, text))
              case None => IO.unit
          }
          .flatTap(_ =>
            stateRef.update(s =>
              s.copy(persisted = s.persisted.copy(recentFiles = trackRecentFile(s.persisted.recentFiles, path)))
            )
          )
          .handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to load file at $path"))
          .void
    }

  private[manager] def saveBufferEffect(bufferId: BufferId): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.persisted.buffers.get(bufferId) match
        case Some(buffer) if buffer.document.filePath.isDefined =>
          saveExistingBuffer(bufferId).handleErrorWith {
            case error: com.serenity.richtext.LossyRichTextOverwriteException =>
              stateRef.get.flatMap(current => workflow.showSaveAsWorkflow(current, bufferId, error.getMessage))
            case error =>
              logger.error(error)(s"[FILE] Failed to save buffer $bufferId")
          }
        case Some(_) =>
          logger.debug(s"[FILE] Buffer $bufferId has no file path; opening native Save As dialog") >>
            requestSaveAsFileDialog(state, Some(bufferId))
        case None =>
          logger.debug(s"[FILE] Buffer $bufferId not found for save")
    }

  protected def requestOpenFileDialog: IO[Unit] =
    fileDialog match
      case Some(dialog) =>
        FileUtils.getCurrentDirectory
          .flatMap(currentDirectory => dialog.chooseOpenFile(Some(currentDirectory)))
          .flatMap {
            case Some(path) =>
              updateState(s => s.copy(runtime = s.runtime.copy(uiSurfaces = List.empty))) >> directLoadFileEffect(path)
            case None =>
              IO.unit
          }
          .handleErrorWith(ex => logger.error(ex)("[FILE] Native open-file dialog failed"))
      case None =>
        // No native dialog to show at all -- fall back to the in-app form, same as the save-as path.
        stateRef.get.flatMap(state => openFileWorkflowModal(FileWorkflowMode.Open, state))

  protected def exportCurrentThemeEffect(state: AppState): IO[Unit] =
    val config            = ThemeConfigWriter.themeToConfig(state.persisted.theme)
    val suggestedFileName = s"${ThemeConfigWriter.fileNameFor(config.name)}.conf"
    fileDialog match
      case Some(dialog) =>
        FileUtils.getCurrentDirectory
          .flatMap(currentDirectory => dialog.chooseSaveFile(Some(currentDirectory), Some(suggestedFileName)))
          .flatMap {
            case Some(path) =>
              ThemeConfigWriter
                .write(config, path)
                .flatTap(_ => logger.info(s"[THEMES] Exported current theme '${config.name}' to $path"))
            case None =>
              IO.unit
          }
          .handleErrorWith(ex => logger.error(ex)(s"[THEMES] Failed to export current theme '${config.name}'"))
      case None =>
        IO.unit

  private[manager] def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.persisted.buffers.get(bufferId) match
        case Some(_) =>
          saveBufferAs(bufferId, path)
        case None =>
          logger.debug(s"[FILE] Buffer $bufferId not found for save as")
    }

  private def findModalForState(state: AppState): Modal =
    activeEditorBufferId(state)
      .flatMap(state.persisted.buffers.get)
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
    else
      buffer.document.content
        .searchAll(query)
        .filter(offset =>
          TextEditing.isWholeGraphemeRange(RopeCharacterSource(buffer.document.content), offset, offset + query.length)
        )
        .map(offset => cursorPositionForOffset(buffer.document.content, offset))

  private def toFindResult(cursor: CursorPosition): FindResult =
    FindResult(cursor.line, cursor.column)

  private def cursorPositionForOffset(content: com.serenity.rope.Rope, offset: Int): CursorPosition =
    val (line, column) = content.offsetToLineColumn(offset)
    CursorPosition(line, column)

  protected def toggleThemeEffect(state: AppState): IO[Unit] =
    val targetThemeName =
      state.persisted.theme.name match
        case "light"                                    => "dark"
        case "dark"                                     => "light"
        case "default-light"                            => "default-dark"
        case "default-dark"                             => "default-light"
        case name if name.toLowerCase.contains("light") => "default-dark"
        case _                                          => "default-light"

    interpretEffect(AppEffect.SwitchTheme(targetThemeName))

  protected def reloadThemeEffect(state: AppState): IO[Unit] =
    interpretEffect(AppEffect.ReloadTheme(state.persisted.theme.name))

  protected def applyThemeByName(themeName: String): IO[Unit] =
    themeManager
      .loadTheme(themeName)
      .flatMap { newTheme =>
        updateState { state =>
          val transition =
            if state.persisted.theme == newTheme then None
            else
              state.persisted.config.scaledUiAnimation
                .map(config => ThemeTransition(state.persisted.theme, 0, config.steps))
          state.copy(
            persisted = state.persisted.copy(theme = newTheme),
            runtime = state.runtime.copy(themeTransition = transition)
          )
        }
      }
      .handleErrorWith(ex => logger.error(ex)(s"[THEME] Failed to switch theme to $themeName"))

  protected def reloadThemeByName(themeName: String): IO[Unit] =
    themeManager
      .loadTheme(themeName)
      .flatMap(theme => updateState(s => s.copy(persisted = s.persisted.copy(theme = theme))))
      .handleErrorWith(ex => logger.error(ex)(s"[THEME] Failed to reload theme $themeName"))

  protected def openThemePickerEffect(state: AppState): IO[Unit] =
    themeNamesRef.get.flatMap { themeNames =>
      if themeNames.isEmpty then IO.unit
      else
        val currentTheme             = state.persisted.theme.name
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
            persisted = stateWithId.persisted.copy(focus = Focus.Surface(surfaceId)),
            runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces :+ surface)
          ),
          state
        )
    }

  protected def openThemeCreatorEffect(state: AppState): IO[Unit] =
    val creatorState             = ThemeCreatorState.fromTheme(state.persisted.theme)
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.ThemeCreator(creatorState),
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
    validateAndUpdateState(
      stateWithId
        .copy(runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces.filterNot {
          _.content match
            case SurfaceContent.ThemeCreator(_) => true
            case _                              => false
        } :+ surface))
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
        persisted = stateWithId.persisted.copy(focus = Focus.Surface(surfaceId)),
        runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces :+ surface)
      ),
      state
    )

private[manager] object StateManagerEffectHandlers:
  import com.serenity.config.{
    AppConfig,
    CommandRunnerKeyAction,
    EditorKeyAction,
    FocusedKeymapConfig,
    KeymapEventAction,
    ModalKeyAction,
    PanelKeyAction,
    PeekKeyAction
  }
  import com.serenity.keystroke.events.Event

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
