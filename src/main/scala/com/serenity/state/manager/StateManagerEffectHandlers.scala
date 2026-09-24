package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.command.*
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.*
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.theme.config.ThemeConfigWriter

/** A buffer's file seen on disk at a revision other than the one the buffer held when it was read (#1623). */
final private[manager] case class ExternalRevisionObservation(
    bufferId: BufferId,
    path: Path,
    bufferRevision: Option[com.serenity.io.DocumentRevision],
    onDisk: com.serenity.io.DocumentRevision
)

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
    // `panelEffects` is declared further down this same class (below), not on `workflow` -- referencing it here is
    // safe (no construction-time cycle: `panelEffects` doesn't depend on `workflowEffects`) because this method body
    // only runs once the whole object is fully constructed, long after both `val`s are assigned.
    def openAsProjectRoot(surfaceId: SurfaceId): IO[Unit] =
      openFileWorkflowAsProjectRootEffect(
        surfaceId,
        path =>
          panelEffects.pinExplorerPanelEffect(
            PanelPosition.Left,
            path,
            PanelTransitions.defaultPanelSize(PanelKind.Explorer, PanelPosition.Left)
          )
      )
    def submitReplace(surfaceId: SurfaceId): IO[Unit]           = submitReplaceWorkflowEffect(surfaceId)
    def beginClose(scope: CloseScope): IO[Unit]                 = stateRef.get.flatMap(beginCloseAction(scope, _))
    def submitClose(surfaceId: SurfaceId): IO[Unit]             = submitCloseWorkflowEffect(surfaceId)
    def submitReloadConflict(surfaceId: SurfaceId): IO[Unit]    = submitReloadConflictEffect(surfaceId)
    def createDirectories(surfaceId: SurfaceId): IO[Unit]       = createFileWorkflowDirectoriesEffect(surfaceId)
    def submitSessionNamePrompt(surfaceId: SurfaceId): IO[Unit] = submitSessionNamePromptEffect(surfaceId)
    def submitSessionList(surfaceId: SurfaceId): IO[Unit]       = submitSessionListEffect(surfaceId))

  private val lifecycleEffects = new LifecycleEffectHandler(
    new LifecycleEffectPort:
      def completeQuit: IO[Unit] = quitSignal.complete(()).attempt.void
  )

  private val animationEffects = new AnimationEffectHandler(bufferAnimationsRef)

  private val configEffects = new StateManagerConfigEffects(
    stateRef,
    logger,
    configPersistencePath,
    sessionPersistence,
    onFontConfigChanged,
    deviceTextScaleProvider,
    editor
  )

  private val keybindingEffects = new StateManagerKeybindingEffects(stateRef, configEffects.updateConfig)

  private val richTextEffects = new StateManagerRichTextEffects(stateRef, validateAndUpdateState, interpretEffect)

  private val projectLspEffects = new StateManagerProjectLspEffects(
    lspQueue,
    projectTaskFiberRef,
    projectTaskSemaphore,
    pinOrUpdateTerminalPanel,
    showPeek,
    showModal
  )

  private val navigationEffects =
    new StateManagerNavigationEffects(stateRef, logger, validateAndUpdateState, interpretEffect)

  private val panelEffects = new StateManagerPanelEffects(
    stateRef,
    logger,
    fileManager,
    markdownPreviewWindow,
    updateModelValidated,
    enqueueEvent,
    showPeek,
    configEffects.updateConfig,
    unpinPanel,
    expandPinnedPanel,
    () => collapseExpandedPanel(),
    switchToPinnedPanel,
    resizePinnedPanel,
    projectLspEffects.cancelProjectTaskSilently
  )

  private val uiPresetEffects = new StateManagerUiPresetEffects(
    stateRef,
    logger,
    uiPresetStore,
    windowSizeProvider,
    themeManager,
    onFontConfigChanged,
    sessionPersistence,
    configEffects.persistConfigFile,
    configEffects.withUpdatedRunnerConfig,
    panelEffects.openMarkdownPreview,
    panelEffects.loadPinnedDirectoryEffect,
    validateAndUpdateState
  )

  private val surfacePopupEffects = new StateManagerSurfacePopupEffects(
    stateRef,
    logger,
    themeManager,
    themeNamesRef,
    fileDialog,
    validateAndUpdateState
  )

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
    stateRef.get.flatMap(state => interpretCommand(command, state))

  private def interpretThemeEffect(effect: ThemeEffect): IO[Unit] =
    effect match
      case ThemeEffect.SwitchTheme(themeName) => surfacePopupEffects.applyThemeByName(themeName)
      case ThemeEffect.ReloadTheme(themeName) => surfacePopupEffects.reloadThemeByName(themeName)
      case ThemeEffect.SaveThemeConfig(config) =>
        ThemeConfigWriter
          .writeUserTheme(config)
          .flatTap(path => logger.info(s"[THEMES] Saved user theme '${config.name}' to $path"))
          .flatMap(_ => surfacePopupEffects.refreshThemeNames)
          .handleErrorWith(ex => logger.error(ex)(s"[THEMES] Failed to save user theme '${config.name}'"))

  private def interpretSurfaceEffect(effect: SurfaceEffect): IO[Unit] =
    effect match
      case SurfaceEffect.OpenThemePicker =>
        stateRef.get.flatMap(surfacePopupEffects.openThemePickerEffect)
      case SurfaceEffect.OpenThemeCreator =>
        stateRef.get.flatMap(surfacePopupEffects.openThemeCreatorEffect)
      case SurfaceEffect.OpenFileSearch =>
        stateRef.get.flatMap(surfacePopupEffects.openFileSearchEffect)

  private def interpretFileEffect(effect: FileEffect): IO[Unit] =
    effect match
      case FileEffect.SaveBuffer(bufferId)         => saveBufferEffect(bufferId)
      case FileEffect.SaveBufferAs(bufferId, path) => saveBufferAsEffect(bufferId, path)
      case FileEffect.DirectLoadFile(path)         => directLoadFileEffect(path)

  private def interpretExplorerEffect(effect: ExplorerEffect): IO[Unit] =
    effect match
      case ExplorerEffect.OpenRoot(position, path, size) =>
        panelEffects.pinExplorerPanelEffect(position, path, size)
      case ExplorerEffect.LoadDirectory(position, path) =>
        panelEffects.loadPinnedDirectoryEffect(position, path)

  private def interpretWorkflowEffect(effect: WorkflowEffect): IO[Unit] =
    workflowEffects.interpret(effect)

  private def interpretLspQueueEffect(effect: LspQueueEffect): IO[Unit] =
    effect match
      case LspQueueEffect.Enqueue(effect) =>
        lspQueue.enqueue(effect)
      case LspQueueEffect.DocumentChanged(uri, languageId, text) =>
        lspQueue.enqueueDocumentChange(uri, languageId, text)

  private[manager] def updateConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
  ): IO[com.serenity.config.AppConfig] =
    configEffects.updateConfig(update)

  // The single command execution+observability chokepoint: every entry path (reducer/keybinding via
  // interpretCommandEffect -- which mouse menus now reach too, by emitting AppEffect.ExecuteCommand -- the command
  // palette via ComponentResult.ExecuteCommand, and CommandExecutor) calls this, so logging the [COMMAND] line here logs each command exactly once regardless of
  // how it was triggered -- rather than only on the effect path, which used to leave palette/mouse-driven commands
  // silent.
  private[manager] def interpretCommand(command: Command, state: AppState): IO[Unit] =
    val dispatch = command.intent match
      case CommandIntent.Lifecycle(intent)   => interpretLifecycleIntent(intent, state)
      case CommandIntent.File(intent)        => interpretFileIntent(intent, state)
      case CommandIntent.Edit(intent)        => interpretEditIntent(intent)
      case CommandIntent.RichText(intent)    => richTextEffects.interpret(intent)
      case CommandIntent.Comments(intent)    => navigationEffects.interpretComments(intent)
      case CommandIntent.Navigation(intent)  => navigationEffects.interpretNavigation(intent)
      case CommandIntent.Lsp(intent)         => projectLspEffects.interpretLsp(intent, state)
      case CommandIntent.Theme(intent)       => surfacePopupEffects.interpretThemeIntent(intent, state)
      case CommandIntent.View(intent)        => panelEffects.interpret(intent, state)
      case CommandIntent.Project(intent)     => projectLspEffects.interpretProject(intent, state)
      case CommandIntent.Session(intent)     => interpretSessionIntent(intent, state)
      case CommandIntent.Keybindings(intent) => keybindingEffects.interpret(intent)
      case CommandIntent.UiPresets(intent)   => uiPresetEffects.interpret(intent)
      case CommandIntent.Settings(intent)    => configEffects.interpret(intent, state)
    // issue #1048: MRU tracking -- every executed command counts toward its recency, regardless of what triggered
    // it (palette, mouse click, contextual toolbar, ...), living on `runtime` since `CommandRunner` itself is
    // reconstructed fresh each time the palette opens (`CommandRunner.recordCommandUsage`'s own doc).
    logger.info(s"[COMMAND] ${StateManager.describeCommandExecution(command)}") >>
      updateModelValidated(model =>
        Some(model.copy(app = StateManagerEffectHandlers.withCommandUsageRecorded(model.app, command.name)))
      ) >> dispatch

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
        loadFile(path)
      case FileIntent.OpenFileSearch =>
        surfacePopupEffects.openFileSearchEffect(state)
      case FileIntent.CloseAll =>
        beginCloseAction(CloseScope.All, state)
      case FileIntent.CloseOthers =>
        beginCloseAction(CloseScope.Others, state)
      case FileIntent.CloseCurrentFile =>
        beginCloseAction(CloseScope.Current, state)
      case FileIntent.NewFile =>
        val registry = CommandRegistry.withToggleUI
        stateRef.get.flatMap(current =>
          validateAndUpdateState(
            AppEventReducer.reduce(com.serenity.keystroke.events.NewTab, current, registry)(using balance).state,
            current
          )
        )
      case FileIntent.SetBufferLanguage(language) =>
        setBufferLanguage(state, language)

  private def setBufferLanguage(state: AppState, language: Option[LanguageId]): IO[Unit] =
    (state.focusedBufferId, state.focusedBufferId.flatMap(state.persisted.buffers.get)) match
      case (Some(bufferId), Some(buffer)) =>
        val updateLanguage =
          stateRef.get.flatMap(current =>
            validateAndUpdateState(
              current.copy(persisted =
                current.persisted.copy(buffers =
                  current.persisted.buffers.updatedWith(bufferId)(
                    _.map(live => live.copy(document = live.document.copy(language = language)))
                  )
                )
              ),
              current
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
                if !state.editingContext.hasCodeTooling then IO.unit
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
      case SessionIntent.ReturnToStartPage =>
        beginCloseAction(CloseScope.ReturnToStartPage, state)
      case SessionIntent.OpenSaveSessionAsPrompt =>
        openSaveSessionAsPrompt(state)
      case SessionIntent.OpenSessionPicker =>
        openSessionPicker(state, SessionListPurpose.Open)
      case SessionIntent.OpenRenameSessionPicker =>
        openSessionPicker(state, SessionListPurpose.Rename)

  /** Reads the focused buffer's on-disk revision (#1623), for the window focus-gain re-check. Runs off the dispatcher;
    * the decision is `resolveExternalRevisionEffect`'s.
    */
  private[manager] def observeFocusedExternalRevisionEffect: IO[Option[ExternalRevisionObservation]] =
    stateRef.get.flatMap(_.focusedBufferId.flatTraverse(observeExternalRevisionEffect))

  /** Reads one buffer's on-disk revision (#1623) when it differs from the revision the buffer holds -- the blocking
    * half of the check both the focus-gain callback and `AppRuntime.externalChangeWatchLoop` drive, run off the
    * dispatcher.
    */
  private[manager] def observeExternalRevisionEffect(bufferId: BufferId): IO[Option[ExternalRevisionObservation]] =
    stateRef.get.flatMap { state =>
      state.persisted.buffers.get(bufferId).flatMap(buffer => buffer.document.filePath.map(buffer -> _)) match
        case Some((buffer, path)) =>
          fileManager.currentRevision(path).map {
            case Some(onDisk) if Some(onDisk) != buffer.document.revision =>
              Some(ExternalRevisionObservation(bufferId, path, buffer.document.revision, onDisk))
            case _ => None
          }
        case None => IO.none
    }

  /** Decides an external change on the dispatcher. An observation whose buffer has since been saved, reloaded, closed
    * or re-pathed is stale and dropped: a save's own disk write is not an external change, and the watcher sees the
    * file again on its next poll anyway. A clean buffer is reloaded silently; a dirty one is prompted, exactly like a
    * stale save.
    */
  private[manager] def resolveExternalRevisionEffect(observation: ExternalRevisionObservation): IO[Unit] =
    isSaving(observation.path).ifM(IO.unit, decideExternalRevision(observation))

  private def decideExternalRevision(observation: ExternalRevisionObservation): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.persisted.buffers
        .get(observation.bufferId)
        .filter(buffer =>
          buffer.document.filePath.contains(observation.path) &&
            buffer.document.revision == observation.bufferRevision
        ) match
        case Some(buffer) if buffer.hasUnsavedChanges =>
          // A blocking modal already up (most likely this buffer's own reload-conflict prompt from an earlier poll or
          // focus-gain) must not get a second one stacked on top of it -- code review finding on PR #1664.
          if state.hasBlockingModal then IO.unit
          else openReloadConflictModal(state, buffer.id, bufferLabelFor(buffer))
        case Some(buffer) => reloadBuffer(buffer.id)
        case None         => IO.unit
    }

  /** The paths of every currently open local buffer, for `FileChangeWatcher.sync`'s directory set -- `AppRuntime`'s
    * background watch loop re-derives this each poll cycle so it tracks buffers opening and closing over time.
    */
  private[manager] def openBufferPathsEffect: IO[Map[Path, BufferId]] =
    stateRef.get.map(state =>
      state.persisted.buffers.values.flatMap(buffer => buffer.document.filePath.map(_ -> buffer.id)).toMap
    )

  private def bufferLabelFor(buffer: Buffer): String =
    buffer.document.filePath
      .map(path => Option(path.getFileName).fold(path.toString)(_.toString))
      .getOrElse(s"Buffer ${buffer.id.value} - unsaved")

  /** Same label, looked up fresh from `state` -- used where the caller only has a `bufferId` and wants the label as of
    * a specific (usually just-re-read) state snapshot rather than one captured earlier.
    */
  private def bufferLabelFor(state: AppState, bufferId: BufferId): String =
    state.persisted.buffers.get(bufferId).fold(s"Buffer ${bufferId.value} - unsaved")(bufferLabelFor)

  /** Opens `path` in the background (#1672): the read runs on the file's lane and the buffer lands when it is done. */
  private[manager] def directLoadFileEffect(path: Path): IO[Unit] =
    loadFile(path)

  private[manager] def saveBufferEffect(bufferId: BufferId): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.persisted.buffers.get(bufferId) match
        case Some(buffer) if buffer.document.filePath.isDefined =>
          submitSave(bufferId, saveFailed(bufferId))
        case Some(_) =>
          logger.debug(s"[FILE] Buffer $bufferId has no file path; opening native Save As dialog") >>
            requestSaveAsFileDialog(state, Some(bufferId))
        case None =>
          logger.debug(s"[FILE] Buffer $bufferId not found for save")
    }

  /** Runs on the dispatcher once a background save has failed. */
  private def saveFailed(bufferId: BufferId)(error: Throwable): IO[Unit] =
    error match
      case lossy: com.serenity.richtext.LossyRichTextOverwriteException =>
        stateRef.get.flatMap(current => workflow.showSaveAsWorkflow(current, bufferId, lossy.getMessage))
      case _: com.serenity.io.FileManagerError.ExternalConflict =>
        // Label from state re-read after the failure, not the pre-save `buffer` snapshot above -- keeps this
        // consistent with StateManagerWorkflowCapability's own ExternalConflict handler, which does the same
        // (code review finding on PR #1664: the two copies previously sourced the label from different points
        // in time, which could show different labels for the same conflict if the buffer changed in between).
        stateRef.get.flatMap(current =>
          workflow.openReloadConflictModal(current, bufferId, bufferLabelFor(current, bufferId))
        )
      case other =>
        logger.error(other)(s"[FILE] Failed to save buffer $bufferId")

  protected def requestOpenFileDialog: IO[Unit] =
    fileDialog match
      case Some(dialog) =>
        openFromDialog(dialog)
      case None =>
        // No native dialog to show at all -- fall back to the in-app form, same as the save-as path.
        stateRef.get.flatMap(state => openFileWorkflowModal(FileWorkflowMode.Open, state))

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

  private[manager] def updateFontConfig(
    update: com.serenity.ui.fonts.FontLoader.FontConfig => com.serenity.ui.fonts.FontLoader.FontConfig
  ): IO[Unit] =
    configEffects.updateFontConfig(update)

private[manager] object StateManagerEffectHandlers:

  def withCommandUsageRecorded(state: AppState, commandName: String): AppState =
    val nextGeneration = state.runtime.commandUsage.values.maxOption.getOrElse(0) + 1
    state.copy(runtime =
      state.runtime.copy(commandUsage = state.runtime.commandUsage + (commandName -> nextGeneration))
    )
