package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.serenity.animation.AnimationConfig
import com.serenity.command.*
import com.serenity.config.{
  AppMode,
  CursorInfoBarSegment,
  DefaultDocumentMode,
  HotkeyTrigger,
  KeymapGroup,
  MarkdownViewMode
}
import com.serenity.document.{CommentRendering, DocumentNavigation, DocumentOutline}
import com.serenity.io.{FileEntry, FileUtils}
import com.serenity.keystroke.events.ExplorerEvent
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.project.*
import com.serenity.richtext.*
import com.serenity.rope.*
import com.serenity.session.SessionSaveTrigger
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.{ThemeConfigWriter, ThemeCreatorState}
import com.serenity.ui.tui.MarkdownPreviewWindowAvailability
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
  def createDirectories(surfaceId: SurfaceId): IO[Unit]

/** Interprets workflow effects without editor, theme, file, or runtime dependencies. */
final private[manager] class WorkflowEffectHandler(port: WorkflowEffectPort):

  def interpret(effect: WorkflowEffect): IO[Unit] =
    effect match
      case WorkflowEffect.RequestOpenFile                   => port.requestOpenFile
      case WorkflowEffect.RequestSaveAs                     => port.requestSaveAs
      case WorkflowEffect.RefreshFileWorkflow(id)           => port.refresh(id)
      case WorkflowEffect.RefreshFind(request)              => port.refreshFind(request)
      case WorkflowEffect.SubmitFileWorkflow(id)            => port.submitFile(id)
      case WorkflowEffect.SubmitReplaceWorkflow(id)         => port.submitReplace(id)
      case WorkflowEffect.SubmitCloseWorkflow(id)           => port.submitClose(id)
      case WorkflowEffect.CreateFileWorkflowDirectories(id) => port.createDirectories(id)

/** Lifecycle operation required by lifecycle effects. */
private[manager] trait LifecycleEffectPort:
  def completeQuit: IO[Unit]

/** Interprets lifecycle effects without runtime, editor, or workflow dependencies. */
final private[manager] class LifecycleEffectHandler(port: LifecycleEffectPort):

  def interpret: IO[Unit] = port.completeQuit

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
  private val DoubleTapWindow = 200.millis

  private val workflowEffects = new WorkflowEffectHandler(new WorkflowEffectPort:
    def requestOpenFile: IO[Unit] = requestOpenFileDialog
    def requestSaveAs: IO[Unit]   = stateRef.get.flatMap(state => requestSaveAsFileDialog(state, state.focusedBufferId))
    def refresh(surfaceId: SurfaceId): IO[Unit]           = refreshFileWorkflowEffect(surfaceId)
    def refreshFind(request: FindSearchRequest): IO[Unit] = scheduleFindSearch(request)
    def submitFile(surfaceId: SurfaceId): IO[Unit]        = submitFileWorkflowEffect(surfaceId)
    def submitReplace(surfaceId: SurfaceId): IO[Unit]     = submitReplaceWorkflowEffect(surfaceId)
    def submitClose(surfaceId: SurfaceId): IO[Unit]       = submitCloseWorkflowEffect(surfaceId)
    def createDirectories(surfaceId: SurfaceId): IO[Unit] = createFileWorkflowDirectoriesEffect(surfaceId))

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
      animationEffects.interpret,
      scheduleCommandRunnerBindingExpiry
    )
  )

  private[manager] def interpretEffect(effect: AppEffect): IO[Unit] =
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

  private def interpretLifecycleEffect: IO[Unit] =
    lifecycleEffects.interpret

  private def interpretCommandEffect(command: Command): IO[Unit] =
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

  private def updateAppModeConfig(
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
      case SurfaceContent.CommandPalette(_) => true
      case SurfaceContent.GhostOverlay(content, _) =>
        content match
          case SurfaceContent.CommandPalette(_) => true
          case _                                => false
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
        config.withFontConfig(update(config.editorConfig.fontConfig).resolveAutoTextScale(deviceTextScale))
      )
        .flatMap(config => onFontConfigChanged(config.editorConfig.fontConfig))
    }

  protected def updateSpellCheckConfig(
    update: com.serenity.config.SpellCheckConfig => com.serenity.config.SpellCheckConfig
  ): IO[Unit] =
    applyConfigUpdate(config => config.withSpellCheck(update(config.languageToolsConfig.spellCheck))).void >>
      scheduleDocumentAnalysis()

  protected def clampFontSize(size: Float): Float =
    size.max(8.0f).min(48.0f)

  private[manager] def interpretCommand(command: Command, state: AppState): IO[Unit] =
    command.intent match
      case CommandIntent.Lifecycle(intent)   => interpretLifecycleIntent(intent, state)
      case CommandIntent.File(intent)        => interpretFileIntent(intent, state)
      case CommandIntent.Edit(intent)        => interpretEditIntent(intent)
      case CommandIntent.RichText(intent)    => interpretRichTextIntent(intent)
      case CommandIntent.Comments(intent)    => interpretCommentsIntent(intent, state)
      case CommandIntent.Navigation(intent)  => interpretNavigationIntent(intent, state)
      case CommandIntent.Lsp(intent)         => interpretLspIntent(intent, state)
      case CommandIntent.Theme(intent)       => interpretThemeIntent(intent, state)
      case CommandIntent.View(intent)        => interpretViewIntent(intent, state)
      case CommandIntent.Project(intent)     => interpretProjectIntent(intent, state)
      case CommandIntent.Session(intent)     => interpretSessionIntent(intent, state)
      case CommandIntent.Keybindings(intent) => interpretKeybindingsIntent(intent)
      case CommandIntent.UiPresets(intent)   => interpretUiPresetsIntent(intent)
      case CommandIntent.Settings(intent)    => interpretSettingsIntent(intent, state)

  private def interpretLifecycleIntent(intent: LifecycleIntent, state: AppState): IO[Unit] =
    intent match
      case LifecycleIntent.QuitApp => beginCloseAction(CloseScope.Quit, state)

  private def interpretFileIntent(intent: FileIntent, state: AppState): IO[Unit] =
    intent match
      case FileIntent.SaveCurrentFile =>
        state.focusedBufferId match
          case Some(bufferId) => saveBufferEffect(bufferId)
          case None           => logger.debug("[CMD] No focused buffer to save")
      case FileIntent.SaveCurrentFileAs =>
        requestSaveAsFileDialog(state, state.focusedBufferId)
      case FileIntent.OpenFile =>
        requestOpenFileDialog
      case FileIntent.OpenRecentFile(path) =>
        IO.blocking(java.nio.file.Files.isRegularFile(path) && java.nio.file.Files.isReadable(path)).flatMap {
          case true =>
            // Dismiss the startup-page surface before loading, exactly like the native open-file dialog path
            // (requestOpenFileDialog) and every other startup action (new/restore/default-buffer). This intent is
            // only ever dispatched from the startup page's recent-file entries, so clearing uiSurfaces is safe here.
            // Without it the StartPage surface lingers and Renderer's `state.startPageSurface` short-circuit keeps
            // drawing the (now stale) splash over the editor: keystrokes reach the hidden buffer but nothing
            // repaints, so the app looks completely frozen (issue: opening a recent file wedges the TUI).
            updateState(state => state.copy(runtime = state.runtime.copy(uiSurfaces = List.empty))) >>
              directLoadFileEffect(path)
          case false => logger.warn(s"[STARTUP] Recent file is unavailable: $path")
        }
      case FileIntent.OpenFileSearch =>
        openFileSearchEffect(state)
      case FileIntent.CloseAll =>
        beginCloseAction(CloseScope.All, state)
      case FileIntent.CloseOthers =>
        beginCloseAction(CloseScope.Others, state)
      case FileIntent.CloseCurrentFile =>
        beginCloseAction(CloseScope.Current, state)
      case FileIntent.NewFile =>
        val registry = CommandRegistry.withToggleUI
        updateState(current =>
          AppEventReducer.reduce(com.serenity.keystroke.events.NewTab, current, registry)(using balance).state
        )
      case FileIntent.SetBufferLanguage(language) =>
        setBufferLanguage(state, language)

  private def setBufferLanguage(state: AppState, language: Option[LanguageId]): IO[Unit] =
    (state.focusedBufferId, state.focusedBufferId.flatMap(state.persisted.buffers.get)) match
      case (Some(bufferId), Some(buffer)) =>
        val updateLanguage =
          updateState(s =>
            s.copy(persisted =
              s.persisted.copy(buffers =
                s.persisted.buffers + (bufferId -> buffer.copy(document = buffer.document.copy(language = language)))
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
                if state.persisted.config.appMode != AppMode.Code then IO.unit
                else language.fold(IO.unit)(next => lspQueue.enqueue(LspEffect.FileOpened(uri, next, text)))
              closeOld >> openNew
            case _ =>
              IO.unit

        updateLanguage >> refreshLspBinding
      case _ =>
        IO.unit

  private def interpretEditIntent(intent: EditIntent): IO[Unit] =
    intent match
      case EditIntent.FindInCurrentFile =>
        updateState(current => ModalStateReducer.show(findModalForState(current), current).state)
      case EditIntent.FindAllInCurrentFile =>
        updateState(current => ModalStateReducer.show(findModalForState(current), current).state)
      case EditIntent.ReplaceInCurrentFile =>
        updateState(current => ModalStateReducer.show(Modal.ReplaceWorkflow(ReplaceWorkflowState()), current).state)
      case EditIntent.ReplaceAllInCurrentFile =>
        updateState(current =>
          ModalStateReducer
            .show(
              Modal.ReplaceWorkflow(ReplaceWorkflowState(selectedAction = ReplaceWorkflowAction.ReplaceAll)),
              current
            )
            .state
        )
      case EditIntent.Copy =>
        enqueueEvent(com.serenity.keystroke.events.Copy)
      case EditIntent.Cut =>
        enqueueEvent(com.serenity.keystroke.events.Cut)
      case EditIntent.Paste =>
        enqueueEvent(com.serenity.keystroke.events.Paste)
      case EditIntent.SelectAll =>
        enqueueEvent(com.serenity.keystroke.events.SelectAll)
      case EditIntent.Undo =>
        enqueueEvent(com.serenity.keystroke.events.Undo)
      case EditIntent.Redo =>
        enqueueEvent(com.serenity.keystroke.events.Redo)
      case EditIntent.FormatCurrentFile =>
        logger.debug("[CMD] Format command requested")

  private def interpretRichTextIntent(intent: RichTextIntent): IO[Unit] =
    intent match
      case RichTextIntent.ToggleRichTextMark(mark) =>
        updateState(current => toggleRichTextMark(current, mark))
      case RichTextIntent.SetRichTextFontFamily(family) =>
        updateState(current => setRichTextFontFamily(current, family))
      case RichTextIntent.SetRichTextFontSize(size) =>
        updateState(current => setRichTextFontSize(current, size))
      case RichTextIntent.SetRichTextColor(color) =>
        updateState(current => setRichTextColor(current, color))
      case RichTextIntent.SetRichTextParagraphRole(role) =>
        updateState(current => setRichTextParagraphRole(current, role))
      case RichTextIntent.SetRichTextParagraphAlignment(alignment) =>
        updateState(current => setRichTextParagraphAlignment(current, alignment))

  private def interpretCommentsIntent(intent: CommentsIntent, state: AppState): IO[Unit] =
    intent match
      case CommentsIntent.ToggleCommentLens =>
        toggleCommentLens(state)
      case CommentsIntent.AddDocumentComment(text) =>
        addDocumentComment(state, text)
      case CommentsIntent.DeleteDocumentComment =>
        deleteDocumentComment(state)
      case CommentsIntent.NextDocumentComment =>
        navigateDocumentComment(state, DocumentNavigation.nextSymbol)
      case CommentsIntent.PreviousDocumentComment =>
        navigateDocumentComment(state, DocumentNavigation.previousSymbol)

  private def interpretNavigationIntent(intent: NavigationIntent, state: AppState): IO[Unit] =
    intent match
      case NavigationIntent.OpenGotoLine =>
        updateState(current => ModalStateReducer.show(Modal.GotoLine(""), current).state)
      case NavigationIntent.ToggleBookmark =>
        toggleBookmark(state)
      case NavigationIntent.NextBookmark =>
        navigateBookmark(state, DocumentNavigation.nextSymbol)
      case NavigationIntent.PreviousBookmark =>
        navigateBookmark(state, DocumentNavigation.previousSymbol)
      case NavigationIntent.NextDocumentSymbol =>
        navigateDocumentSymbol(state, DocumentNavigation.nextSymbol)
      case NavigationIntent.PreviousDocumentSymbol =>
        navigateDocumentSymbol(state, DocumentNavigation.previousSymbol)
      case NavigationIntent.NavigateBack =>
        navigateHistoryBack()
      case NavigationIntent.NavigateForward =>
        navigateHistoryForward()

  private def interpretLspIntent(intent: LspIntent, state: AppState): IO[Unit] =
    intent match
      case LspIntent.RequestLspHover =>
        requestLspHover(state)
      case LspIntent.RequestLspCompletion =>
        requestLspCompletion(state)
      case LspIntent.RequestLspDefinition =>
        requestLspDefinition(state)

  private def interpretThemeIntent(intent: ThemeIntent, state: AppState): IO[Unit] =
    intent match
      case ThemeIntent.ToggleTheme =>
        toggleThemeEffect(state)
      case ThemeIntent.ReloadTheme =>
        reloadThemeEffect(state)
      case ThemeIntent.OpenThemeChooser =>
        openThemePickerEffect(state)
      case ThemeIntent.OpenThemeCreator =>
        openThemeCreatorEffect(state)
      case ThemeIntent.ExportCurrentTheme =>
        exportCurrentThemeEffect(state)
      case ThemeIntent.ReloadThemes =>
        themeManager.listAvailableThemes
          .flatMap(themeNamesRef.set)
          .handleErrorWith(ex => logger.error(ex)("[THEMES] Failed to reload theme list"))

  private def interpretViewIntent(intent: ViewIntent, state: AppState): IO[Unit] =
    intent match
      case ViewIntent.NextTab =>
        updateState(EditorState.navigateToNextBuffer)
      case ViewIntent.PreviousTab =>
        updateState(EditorState.navigateToPreviousBuffer)
      case ViewIntent.PinExplorerPanel =>
        setPanelPin(PanelKind.Explorer, Some(PanelPosition.Left))
      case ViewIntent.PinOutlinePanel =>
        setPanelPin(PanelKind.Outline, Some(PanelPosition.Right))
      case ViewIntent.PinCommentsPanel =>
        setPanelPin(PanelKind.Comments, Some(PanelPosition.Right))
      case ViewIntent.PinDiagnosticsPanel =>
        setPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Bottom))
      case ViewIntent.OpenMarkdownPreview =>
        // In-pane preview is structurally unavailable in the TUI (cell surfaces cannot `drawImage`) -- toggle the
        // spawned Swing window there instead of pinning the GUI-only panel (issue #1113).
        if state.runtime.isTuiMode then toggleMarkdownPreviewWindow(state)
        else setPanelPin(PanelKind.MarkdownPreview, Some(PanelPosition.Right))
      case ViewIntent.SetPanelPin(kind, position) =>
        setPanelPin(kind, position)
      case ViewIntent.MovePanelEarlier(kind) =>
        movePanelKind(kind, delta = -1)
      case ViewIntent.MovePanelLater(kind) =>
        movePanelKind(kind, delta = 1)
      case ViewIntent.SetMarkdownViewMode(mode) =>
        setMarkdownViewMode(mode)
      case ViewIntent.SetDefaultDocumentMode(mode) =>
        updateDocumentDefaultsConfig(_.withDefaultDocumentMode(mode)).void
      case ViewIntent.SetAppMode(mode) =>
        updateAppModeConfig(_.withAppMode(mode)).void
      case ViewIntent.SetShowAllSettingsRegardlessOfMode(value) =>
        updateAppModeConfig(_.withShowAllSettingsRegardlessOfMode(value)).void
      case ViewIntent.FocusPanel(position) =>
        switchToPinnedPanel(PanelTarget.ByPosition(position))
      case ViewIntent.UnpinPanel(position) =>
        // Closing the project-task output panel while its task is still running must actually stop it -- otherwise
        // the task's own 100ms output-refresh tick (`runProjectTask`) just re-pins it right back (issue #1294).
        val closingRunningTaskPanel = state.pinnedSurfaces.exists { surface =>
          (surface.content, surface.presentation) match
            case (SurfaceContent.Terminal(_, _), SurfacePresentation.Pinned(`position`, _)) => true
            case _                                                                           => false
        }
        unpinPanel(PanelTarget.ByPosition(position)) >> (if closingRunningTaskPanel then cancelProjectTask else IO.unit)
      case ViewIntent.ExpandPanel(position) =>
        expandPinnedPanel(PanelTarget.ByPosition(position))
      case ViewIntent.CollapseExpandedPanel =>
        collapseExpandedPanel()
      case ViewIntent.ToggleShortcutsHelp =>
        enqueueEvent(com.serenity.keystroke.events.ToggleShortcutsHelp)

  private def interpretProjectIntent(intent: ProjectIntent, state: AppState): IO[Unit] =
    intent match
      case ProjectIntent.RunProjectTask(kind) =>
        runProjectTask(state, kind)
      case ProjectIntent.CancelProjectTask =>
        cancelProjectTask

  private def interpretSessionIntent(intent: SessionIntent, state: AppState): IO[Unit] =
    intent match
      case SessionIntent.SaveSession =>
        saveSession()
      case SessionIntent.RestoreSession =>
        loadSession().flatMap {
          case Some(restored) => validateAndUpdateState(restoreSessionIntoCurrentViewport(restored, state), state)
          case None           => logger.debug("[SESSION] Restore requested without a saved session")
        }
      case SessionIntent.ClearSession =>
        clearSession()
      case SessionIntent.StartupNewSession =>
        createStartupSession()
      case SessionIntent.StartupRestoreSession =>
        restoreStartupSession()
      case SessionIntent.StartupOpenFile =>
        requestOpenFileDialog

  private def interpretKeybindingsIntent(intent: KeybindingsIntent): IO[Unit] =
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

  private def interpretUiPresetsIntent(intent: UiPresetsIntent): IO[Unit] =
    intent match
      case UiPresetsIntent.SaveUiPresetAsNew(name) =>
        saveUiPresetAsNewEffect(name)
      case UiPresetsIntent.OverwriteUiPreset(name) =>
        overwriteUiPresetEffect(name)
      case UiPresetsIntent.ApplyUiPreset(name) =>
        applyUiPresetEffect(name)
      case UiPresetsIntent.DuplicateUiPreset(sourceName, targetName) =>
        duplicateUiPresetEffect(sourceName, targetName)
      case UiPresetsIntent.RenameUiPreset(sourceName, targetName) =>
        renameUiPresetEffect(sourceName, targetName)
      case UiPresetsIntent.DeleteUiPreset(name) =>
        deleteUiPresetEffect(name)
      case UiPresetsIntent.ResetUiPreset(name) =>
        resetUiPresetEffect(name)

  private def interpretSettingsIntent(intent: SettingsIntent, state: AppState): IO[Unit] =
    intent match
      case SettingsIntent.Font(fontIntent)               => interpretFontIntent(fontIntent)
      case SettingsIntent.Motion(motionIntent)           => interpretMotionIntent(motionIntent)
      case SettingsIntent.Cursor(cursorIntent)           => interpretCursorIntent(cursorIntent)
      case SettingsIntent.PanelChrome(panelChromeIntent) => interpretPanelChromeIntent(panelChromeIntent)
      case SettingsIntent.SpellCheck(spellCheckIntent)   => interpretSpellCheckIntent(spellCheckIntent)
      case SettingsIntent.General(generalIntent)         => interpretGeneralSettingsIntent(generalIntent, state)

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

  private def interpretMotionIntent(intent: MotionIntent): IO[Unit] =
    intent match
      case MotionIntent.SetMotionPreset(preset) =>
        updateMotionConfig(_.withMotionPreset(preset)).void
      case MotionIntent.SetMotionAccessibility(accessibility) =>
        updateMotionAccessibility(accessibility).void
      case MotionIntent.SetElementTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withElementTransitionSpeedScale(scale)).void
      case MotionIntent.SetEditorTextTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withEditorTextTransitionSpeedScale(Some(scale))).void
      case MotionIntent.SetCommandRunnerTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withCommandRunnerTransitionSpeedScale(Some(scale))).void
      case MotionIntent.SetUiTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withUiTransitionSpeedScale(Some(scale))).void
      case MotionIntent.SetCursorTransitionSpeedScale(scale) =>
        updateCustomMotionConfig(_.withCursorTransitionSpeedScale(Some(scale))).void
      case MotionIntent.SetCommandRunnerAnimation(animation) =>
        updateCustomMotionConfig(_.withCommandRunnerAnimation(animation)).void
      case MotionIntent.SetUiAnimation(animation) =>
        updateCustomMotionConfig(_.withUiAnimation(animation)).void
      case MotionIntent.SetCommandRunnerVisibleRows(rows) =>
        updateAppearanceConfig(_.withCommandRunnerVisibleRows(rows)).void
      case MotionIntent.SetCommandRunnerItemGapRows(rows) =>
        updateAppearanceConfig(_.withCommandRunnerItemGapRows(rows)).void
      case MotionIntent.SetCommandRunnerCursorGapRows(rows) =>
        updateAppearanceConfig(_.withCommandRunnerCursorGapRows(rows)).void
      case MotionIntent.SetEditorInsertionTransitionKind(kind) =>
        updateCustomMotionConfig(_.withEditorInsertionTransitionKind(kind)).void
      case MotionIntent.SetCommandRunnerTransitionKind(kind) =>
        updateCustomMotionConfig(_.withCommandRunnerTransitionKind(Some(kind))).void
      case MotionIntent.SetPanelOpenTransitionKind(kind) =>
        updateCustomMotionConfig(_.withPanelOpenTransitionKind(Some(kind))).void
      case MotionIntent.SetPanelCloseTransitionKind(kind) =>
        updateCustomMotionConfig(_.withPanelCloseTransitionKind(Some(kind))).void

  private def interpretCursorIntent(intent: CursorIntent): IO[Unit] =
    intent match
      case CursorIntent.SetCursorMode(mode) =>
        updateAppearanceConfig(_.withCursorMode(mode)).void
      case CursorIntent.SetCursorInfoBarSegmentIncluded(segment, included) =>
        updateAppearanceConfig { config =>
          val current = config.cursorInfoBarSegments
          val updated =
            if included then if current.contains(segment) then current else current :+ segment
            else current.filterNot(_ == segment)
          config.withCursorInfoBarSegments(updated)
        }.void
      case CursorIntent.MoveCursorInfoBarSegmentEarlier(segment) =>
        updateAppearanceConfig(config =>
          config.withCursorInfoBarSegments(moveCursorInfoBarSegment(config.cursorInfoBarSegments, segment, -1))
        ).void
      case CursorIntent.MoveCursorInfoBarSegmentLater(segment) =>
        updateAppearanceConfig(config =>
          config.withCursorInfoBarSegments(moveCursorInfoBarSegment(config.cursorInfoBarSegments, segment, 1))
        ).void
      case CursorIntent.SetCursorInfoBarPlacement(placement) =>
        updateAppearanceConfig(_.withCursorInfoBarPlacement(placement)).void

  private def moveCursorInfoBarSegment(
    segments: List[CursorInfoBarSegment],
    segment: CursorInfoBarSegment,
    delta: Int
  ): List[CursorInfoBarSegment] =
    val index  = segments.indexOf(segment)
    val target = index + delta
    if index < 0 || target < 0 || target >= segments.length then segments
    else
      segments.zipWithIndex.map {
        case (_, `index`)  => segments(target)
        case (_, `target`) => segments(index)
        case (other, _)    => other
      }

  private def interpretPanelChromeIntent(intent: PanelChromeIntent): IO[Unit] =
    intent match
      case PanelChromeIntent.ToggleLineNumbers =>
        updateTextDisplayConfig(config => config.withLineNumbers(!config.surfaceConfig.showLineNumbers)).void
      case PanelChromeIntent.ToggleGutter =>
        updateTextDisplayConfig(config => config.withGutter(!config.surfaceConfig.showGutter)).void
      case PanelChromeIntent.ToggleWordWrap =>
        updateTextDisplayConfig(config => config.withWordWrap(!config.surfaceConfig.wordWrapEnabled)).void
      case PanelChromeIntent.ToggleFocusedTextBody =>
        updateTextDisplayConfig(config => config.withFocusedTextBody(!config.surfaceConfig.focusedTextBodyEnabled)).void
      case PanelChromeIntent.ToggleContextualToolbar =>
        enqueueEvent(com.serenity.keystroke.events.ToggleContextualToolbar)
      case PanelChromeIntent.TogglePaneHeaders =>
        updateTextDisplayConfig(config => config.withPaneHeaders(!config.surfaceConfig.showPaneHeaders)).void
      case PanelChromeIntent.ToggleVisualLineCursorNavigation =>
        updateTextDisplayConfig(config =>
          config.withVisualLineCursorNavigation(!config.surfaceConfig.visualLineCursorNavigation)
        ).void
      case PanelChromeIntent.SetLineNumbers(enabled) =>
        updateTextDisplayConfig(config => config.withLineNumbers(enabled)).void
      case PanelChromeIntent.SetGutter(enabled) =>
        updateTextDisplayConfig(config => config.withGutter(enabled)).void
      case PanelChromeIntent.SetWordWrap(enabled) =>
        updateTextDisplayConfig(config => config.withWordWrap(enabled)).void
      case PanelChromeIntent.SetVisualLineCursorNavigation(enabled) =>
        updateTextDisplayConfig(config => config.withVisualLineCursorNavigation(enabled)).void
      case PanelChromeIntent.SetFocusedTextBody(enabled) =>
        updateTextDisplayConfig(config => config.withFocusedTextBody(enabled)).void
      case PanelChromeIntent.SetContextualToolbarEnabled(enabled) =>
        updateTextDisplayConfig(config => config.withContextualToolbarEnabled(enabled)).void
      case PanelChromeIntent.SetContextualToolbarDisplayMode(mode) =>
        updateTextDisplayConfig(config => config.withContextualToolbarDisplayMode(mode)).void
      case PanelChromeIntent.SetCommandRunnerShowKeyHints(enabled) =>
        updateAppearanceConfig(_.withCommandRunnerShowKeyHints(enabled)).void
      case PanelChromeIntent.SetUiElementGap(gap) =>
        updateAppearanceConfig(_.withUiElementGap(gap)).void
      case PanelChromeIntent.SetUiCornerRadiusPx(radius) =>
        updateAppearanceConfig(_.withUiCornerRadiusPx(radius)).void
      case PanelChromeIntent.SetUiOutlineThicknessPx(thickness) =>
        updateAppearanceConfig(_.withUiOutlineThicknessPx(thickness)).void
      case PanelChromeIntent.SetInterfaceDensity(density) =>
        updateAppearanceConfig(_.withInterfaceDensity(density)).void
      case PanelChromeIntent.SetWindowChromeMode(mode) =>
        updateAppearanceConfig(_.withWindowChromeMode(mode)).void
      case PanelChromeIntent.SetWindowSitterEnabled(enabled) =>
        updateWindowSitterConfig(_.copy(enabled = enabled))
      case PanelChromeIntent.SetWindowSitterAction(action) =>
        updateWindowSitterConfig(_.copy(action = action))
      case PanelChromeIntent.SetWindowSitterFrames(frames) =>
        updateWindowSitterConfig(_.copy(frames = frames))
      case PanelChromeIntent.SetWindowSitterActiveTicks(ticks) =>
        updateWindowSitterConfig(_.copy(activeTicks = ticks))
      case PanelChromeIntent.SetWindowSitterFastActiveTicks(ticks) =>
        updateWindowSitterConfig(_.copy(fastActiveTicks = ticks))
      case PanelChromeIntent.SetWindowSitterFastTypingThresholdMs(ms) =>
        updateWindowSitterConfig(_.copy(fastTypingThresholdMs = ms))
      case PanelChromeIntent.SetWheelScrollLines(lines) =>
        updateConfig(_.withWheelScrollLines(lines)).void
      case PanelChromeIntent.SetTextAreaLeftInset(value) =>
        updateTextDisplayConfig(_.withTextAreaLeftInset(value)).void
      case PanelChromeIntent.SetTextAreaRightInset(value) =>
        updateTextDisplayConfig(_.withTextAreaRightInset(value)).void
      case PanelChromeIntent.SetTextAreaTopInset(value) =>
        updateTextDisplayConfig(_.withTextAreaTopInset(value)).void
      case PanelChromeIntent.SetTextAreaBottomInset(value) =>
        updateTextDisplayConfig(_.withTextAreaBottomInset(value)).void
      case PanelChromeIntent.SetShowWordCount(enabled) =>
        updateTextDisplayConfig(_.withWordCount(enabled)).void

  private def interpretSpellCheckIntent(intent: SpellCheckIntent): IO[Unit] =
    intent match
      case SpellCheckIntent.SetSpellCheckEnabled(enabled) =>
        updateSpellCheckConfig(_.copy(enabled = enabled))
      case SpellCheckIntent.SetSpellCheckLanguages(languages) =>
        updateSpellCheckConfig(_.copy(languages = languages))
      case SpellCheckIntent.SetSpellCheckDictionaryPaths(paths) =>
        updateSpellCheckConfig(_.copy(dictionaryPaths = paths))
      case SpellCheckIntent.SetSpellCheckWords(words) =>
        updateSpellCheckConfig(_.copy(additionalWords = words))

  private def interpretGeneralSettingsIntent(intent: GeneralSettingsIntent, state: AppState): IO[Unit] =
    intent match
      case GeneralSettingsIntent.OpenSettings =>
        updateState(current => CommandRunnerReducer.openSettings(current, CommandRegistry.withToggleUI)(using balance))
      case GeneralSettingsIntent.SaveConfig =>
        persistConfigFile(state.persisted.config)
      case GeneralSettingsIntent.SetMaterialPreset(preset) =>
        updateAppearanceConfig(_.withMaterialPreset(preset)).void
      case GeneralSettingsIntent.SetPostProcessingEffect(effect) =>
        updateAppearanceConfig(_.withPostProcessingEffect(effect)).void
      case GeneralSettingsIntent.SetUiShadowsEnabled(enabled) =>
        updateAppearanceConfig(_.withUiShadowsEnabled(enabled)).void
      case GeneralSettingsIntent.SetRenderFpsTarget(target) =>
        updateAppearanceConfig(_.withRenderFpsTarget(target)).void
      case GeneralSettingsIntent.SetRenderDamageGranularity(granularity) =>
        updateAppearanceConfig(_.withRenderDamageGranularity(granularity)).void
      case GeneralSettingsIntent.SetBackgroundStyle(style) =>
        updateAppearanceConfig(_.withBackgroundStyle(style)).void
      case GeneralSettingsIntent.SetBlurRadius(r) =>
        updateAppearanceConfig(_.withBlurRadius(r)).void
      case GeneralSettingsIntent.SetAnimationDuration(ms) =>
        updateCustomMotionConfig(withEditorTextAnimationDuration(_, ms)).void
      case GeneralSettingsIntent.SetAnimationSteps(n) =>
        updateCustomMotionConfig(withEditorTextAnimationSteps(_, n)).void

  private def withEditorTextAnimationDuration(
    config: com.serenity.config.AppConfig,
    ms: Int
  ): com.serenity.config.AppConfig =
    val newAnim =
      if ms <= 0 then None
      else
        Some(
          config.editorConfig.characterAnimation.fold(
            AnimationConfig(steps = 12, totalDuration = scala.concurrent.duration.Duration.fromNanos(ms * 1_000_000L))
          )(existing => existing.copy(totalDuration = scala.concurrent.duration.Duration.fromNanos(ms * 1_000_000L)))
        )
    config.withEditorTextAnimation(newAnim)

  private def withEditorTextAnimationSteps(
    config: com.serenity.config.AppConfig,
    n: Int
  ): com.serenity.config.AppConfig =
    val newAnim =
      if n <= 0 then None
      else
        Some(
          config.editorConfig.characterAnimation.fold(
            AnimationConfig(steps = n, totalDuration = scala.concurrent.duration.Duration.fromNanos(200_000_000L))
          )(existing => existing.copy(steps = n))
        )
    config.withEditorTextAnimation(newAnim)

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
                state.copy(runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces.map {
                  case current if current.id == surface.id =>
                    current.copy(content = SurfaceContent.CommandPalette(updatedRunner))
                  case current =>
                    current
                }))
              case None => state
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
                    _ <- onFontConfigChanged(appliedConfig.editorConfig.fontConfig)
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
              case current =>
                current
            }
            state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))
          case _ =>
            state
      case None =>
        state

  private def loadUiPresetResources(preset: UiPreset): IO[Either[String, Theme]] =
    FontLoader.missingFamilies(preset.config.editorConfig.fontConfig) match
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
              case current =>
                current
            }
            state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))
          case _ =>
            state
      case None =>
        state

  /** Focuses the just-created preset's own editing group (issue #1059: renders on the one `CommandPalette` surface,
    * like every other settings drill-in, rather than spawning a second floating one).
    */
  private def focusCreatedPresetOptions(name: String, statusMessage: String): IO[Unit] =
    stateRef.update { state =>
      state.commandRunnerSurface match
        case Some(surface) =>
          surface.content match
            case SurfaceContent.CommandPalette(runner) =>
              val updatedRunner = runner
                .withDrilledSettingsSurface(
                  SettingsSurfaceState(
                    SettingsPage.Group("settings-preset-edit"),
                    List(SettingsPage.Group("settings-ui-presets", 2))
                  )
                )
                .copy(
                  submenuSelections = runner.submenuSelections + ("settings-ui-presets" -> 2),
                  editingItemId = None,
                  editingText = "",
                  editingPresetName = Some(name.trim),
                  statusMessage = Some(statusMessage)
                )
              val updatedSurfaces = state.runtime.uiSurfaces.map {
                case current if current.id == surface.id =>
                  current.copy(content = SurfaceContent.CommandPalette(updatedRunner))
                case current =>
                  current
              }
              state.copy(
                persisted = state.persisted.copy(focus = Focus.Surface(surface.id)),
                runtime = state.runtime.copy(uiSurfaces = updatedSurfaces)
              )
            case _ =>
              state
        case None =>
          state
    }

  private def runProjectTask(state: AppState, kind: ProjectTaskKind): IO[Unit] =
    if state.persisted.config.appMode != AppMode.Code then
      pinProjectTerminal(ProjectTaskTerminal.notAvailableInProseMode(kind))
    else
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
                        outputRef.get.flatMap(output =>
                          pinProjectTerminal(ProjectTaskTerminal.running(command, output))
                        )
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
    pinOrUpdateTerminalPanel(text, PanelPosition.Bottom, 14)

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

  private def markdownPreviewBufferId(state: AppState): Option[BufferId] =
    state.focusedBufferId
      .flatMap(state.persisted.buffers.get)
      .filter(_.document.language.contains(LanguageId.Markdown))
      .map(_.id)

  /** Toggles the TUI's spawned Swing preview window (issue #1113): closes it when already open for the focused buffer,
    * opens it (following the focused buffer) when closed, and reports unavailability -- rather than silently no-op'ing
    * -- both when no display was reachable at startup and when there is no Markdown buffer to preview.
    */
  private def toggleMarkdownPreviewWindow(state: AppState): IO[Unit] =
    markdownPreviewWindow match
      case MarkdownPreviewWindowAvailability.Unavailable =>
        showMarkdownPreviewUnavailablePeek(state, "Markdown preview window needs a graphical display.")
      case MarkdownPreviewWindowAvailability.Available(window) =>
        state.runtime.markdownPreviewWindowBuffer match
          case Some(_) =>
            window.hide() >> updateState(s => s.copy(runtime = s.runtime.copy(markdownPreviewWindowBuffer = None)))
          case None =>
            markdownPreviewBufferId(state) match
              case Some(bufferId) =>
                window.show() >>
                  updateState(s => s.copy(runtime = s.runtime.copy(markdownPreviewWindowBuffer = Some(bufferId))))
              case None =>
                showMarkdownPreviewUnavailablePeek(state, "Markdown preview needs an active Markdown buffer.")

  private def showMarkdownPreviewUnavailablePeek(state: AppState, message: String): IO[Unit] =
    showPeek(PeekContent.QuickInfo(message), state.activeCursorPosition.getOrElse(CursorPosition(0, 0)))

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
        state.persisted.config.surfaceConfig.wordWrapEnabled,
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
      val cursor =
        buffer.document.content.offsetToCursorPosition(buffer.document.content.graphemeBoundaryAfterOrAt(startOffset))
      cursor -> cursor
    else
      buffer.document.content.offsetToCursorPosition(
        buffer.document.content.graphemeBoundaryBeforeOrAt(startOffset)
      ) ->
        buffer.document.content.offsetToCursorPosition(buffer.document.content.graphemeBoundaryAfterOrAt(endOffset))

  private def snapCursorAfterGrapheme(buffer: Buffer, cursor: CursorPosition): CursorPosition =
    val offset = buffer.document.content.lineColumnToOffset(cursor.line, cursor.column)
    buffer.document.content.offsetToCursorPosition(buffer.document.content.graphemeBoundaryAfterOrAt(offset))

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
      fileEntries <- fileManager.listDirectory(path)
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
      fileEntries <- fileManager.listDirectory(path)
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
            // Structural mutation (adds a buffer, reorders bufferOrder, reassigns pane focus): routed through the
            // checked commit so a drifted `nextBufferId` (see #858) can't silently duplicate a bufferOrder entry or
            // overwrite a live buffer instead of being rejected.
            stateRef.get.flatMap { state =>
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
              validateAndUpdateState(resized, state).as(loadedBuffer)
            }
          }
          .flatTap { loadedBuffer =>
            loadedBuffer.document.language match
              case Some(languageId) =>
                val uri  = path.toUri.toString
                val text = loadedBuffer.document.content.collect()
                stateRef.get.flatMap { state =>
                  if state.persisted.config.appMode == AppMode.Code then
                    lspQueue.enqueue(LspEffect.FileOpened(uri, languageId, text))
                  else IO.unit
                }
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
        .filter(offset => buffer.document.content.isWholeGraphemeRange(offset, offset + query.length))
        .map(offset => buffer.document.content.offsetToCursorPosition(offset))

  private def toFindResult(cursor: CursorPosition): FindResult =
    FindResult(cursor.line, cursor.column)

  protected def toggleThemeEffect(state: AppState): IO[Unit] =
    val targetThemeName =
      state.persisted.theme.name match
        case "light"                                    => "dark"
        case "dark"                                     => "light"
        case "default-light"                            => "default-dark"
        case "default-dark"                             => "default-light"
        case name if name.toLowerCase.contains("light") => "default-dark"
        case _                                          => "default-light"

    interpretEffect(AppEffect.Theme(ThemeEffect.SwitchTheme(targetThemeName)))

  protected def reloadThemeEffect(state: AppState): IO[Unit] =
    interpretEffect(AppEffect.Theme(ThemeEffect.ReloadTheme(state.persisted.theme.name)))

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
