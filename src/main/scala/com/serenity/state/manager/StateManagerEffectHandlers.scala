package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.command.*
import com.serenity.io.FileUtils
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.*
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.theme.config.ThemeConfigWriter

/** Workflow operations selected by command effects. */
private[manager] trait WorkflowEffectPort:
  def requestOpenFile: IO[Unit]
  def requestSaveAs: IO[Unit]
  def refresh(surfaceId: SurfaceId): IO[Unit]
  def refreshFind(request: FindSearchRequest): IO[Unit]
  def submitFile(surfaceId: SurfaceId): IO[Unit]
  def openAsProjectRoot(surfaceId: SurfaceId): IO[Unit]
  def submitReplace(surfaceId: SurfaceId): IO[Unit]
  def submitClose(surfaceId: SurfaceId): IO[Unit]
  def submitReloadConflict(surfaceId: SurfaceId): IO[Unit]
  def createDirectories(surfaceId: SurfaceId): IO[Unit]
  def submitSessionNamePrompt(surfaceId: SurfaceId): IO[Unit]
  def submitSessionList(surfaceId: SurfaceId): IO[Unit]

/** Interprets workflow effects without editor, theme, file, or runtime dependencies. */
final private[manager] class WorkflowEffectHandler(port: WorkflowEffectPort):

  def interpret(effect: WorkflowEffect): IO[Unit] =
    effect match
      case WorkflowEffect.RequestOpenFile                   => port.requestOpenFile
      case WorkflowEffect.RequestSaveAs                     => port.requestSaveAs
      case WorkflowEffect.RefreshFileWorkflow(id)           => port.refresh(id)
      case WorkflowEffect.RefreshFind(request)              => port.refreshFind(request)
      case WorkflowEffect.SubmitFileWorkflow(id)            => port.submitFile(id)
      case WorkflowEffect.OpenFileWorkflowAsProjectRoot(id) => port.openAsProjectRoot(id)
      case WorkflowEffect.SubmitReplaceWorkflow(id)         => port.submitReplace(id)
      case WorkflowEffect.SubmitCloseWorkflow(id)           => port.submitClose(id)
      case WorkflowEffect.SubmitReloadConflict(id)          => port.submitReloadConflict(id)
      case WorkflowEffect.CreateFileWorkflowDirectories(id) => port.createDirectories(id)
      case WorkflowEffect.SubmitSessionNamePrompt(id)       => port.submitSessionNamePrompt(id)
      case WorkflowEffect.SubmitSessionList(id)             => port.submitSessionList(id)

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
            panelEffects.defaultPanelSize(PanelKind.Explorer, PanelPosition.Left)
          )
      )
    def submitReplace(surfaceId: SurfaceId): IO[Unit]           = submitReplaceWorkflowEffect(surfaceId)
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
    bufferAnimationsRef,
    onFontConfigChanged,
    deviceTextScaleProvider,
    editor
  )

  private val keybindingEffects = new StateManagerKeybindingEffects(stateRef, configEffects.updateConfig)

  private val richTextEffects = new StateManagerRichTextEffects(stateRef)

  private val projectLspEffects = new StateManagerProjectLspEffects(
    lspQueue,
    projectTaskFiberRef,
    projectTaskSemaphore,
    pinOrUpdateTerminalPanel,
    showPeek,
    showModal
  )

  private val navigationEffects =
    new StateManagerNavigationEffects(stateRef, bufferAnimationsRef, logger, validateAndUpdateState)

  private val panelEffects = new StateManagerPanelEffects(
    stateRef,
    logger,
    fileManager,
    markdownPreviewWindow,
    validateAndUpdateState,
    enqueueEvent,
    showPeek,
    configEffects.updateConfig,
    unpinPanel,
    expandPinnedPanel,
    () => collapseExpandedPanel(),
    switchToPinnedPanel,
    resizePinnedPanel,
    projectLspEffects.cancelProjectTaskSilently,
    recordUndoBoundary
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
  // interpretCommandEffect, the command palette via ComponentResult.ExecuteCommand, and mouse menus via the
  // executeCommand port) calls this, so logging the [COMMAND] line here logs each command exactly once regardless of
  // how it was triggered -- rather than only on the effect path, which used to leave palette/mouse-driven commands
  // silent.
  private[manager] def interpretCommand(command: Command, state: AppState): IO[Unit] =
    val dispatch = command.intent match
      case CommandIntent.Lifecycle(intent)   => interpretLifecycleIntent(intent, state)
      case CommandIntent.File(intent)        => interpretFileIntent(intent, state)
      case CommandIntent.Edit(intent)        => interpretEditIntent(intent)
      case CommandIntent.RichText(intent)    => richTextEffects.interpret(intent)
      case CommandIntent.Comments(intent)    => navigationEffects.interpretComments(intent, state)
      case CommandIntent.Navigation(intent)  => navigationEffects.interpretNavigation(intent, state)
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
      stateRef.update(recordCommandUsage(command.name)) >> dispatch

  private def recordCommandUsage(name: String)(state: AppState): AppState =
    val nextGeneration = state.runtime.commandUsage.values.maxOption.getOrElse(0) + 1
    state.copy(runtime = state.runtime.copy(commandUsage = state.runtime.commandUsage + (name -> nextGeneration)))

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
        surfacePopupEffects.openFileSearchEffect(state)
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

  private def trackRecentFile(current: List[Path], path: Path): List[Path] =
    (path :: current.filterNot(_ == path)).take(20)

  /** Re-checks the focused buffer's on-disk revision against its captured one (#1623), called on window focus-gain. */
  private[manager] def checkExternalChangesOnFocusEffect: IO[Unit] =
    stateRef.get.flatMap { state =>
      state.focusedBufferId match
        case Some(bufferId) => checkBufferForExternalChangesEffect(bufferId)
        case None            => IO.unit
    }

  /** Re-checks one buffer's on-disk revision against its captured one (#1623) -- the shared decision both the
    * focus-gain check and `FileChangeWatcher`'s background poll loop (`AppRuntime.externalChangeWatchLoop`) drive.
    * A clean buffer (no unsaved edits) that changed externally is reloaded silently -- there's nothing of the user's
    * to lose. A dirty one is left alone but prompted, exactly like a stale save: the user decides whether to keep
    * their edits or take the external change.
    */
  private[manager] def checkBufferForExternalChangesEffect(bufferId: BufferId): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.persisted.buffers.get(bufferId) match
        case Some(buffer) =>
          buffer.document.filePath match
            case Some(path) =>
              fileManager.currentRevision(path).flatMap {
                case Some(onDisk) if Some(onDisk) != buffer.document.revision =>
                  if buffer.hasUnsavedChanges then
                    openReloadConflictModal(state, buffer.id, bufferLabelFor(buffer))
                  else
                    reloadBuffer(buffer.id)
                case _ => IO.unit
              }
            case None => IO.unit
        case None => IO.unit
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

  private[manager] def directLoadFileEffect(path: Path): IO[Unit] =
    IO.blocking(FileUtils.isReadableFile(path)).flatMap {
      case false => logger.debug(s"[FILE] DirectLoad: file not readable: $path")
      case true =>
        val load =
          for
            bufferId <- stateRef.modify { state =>
              val bufferId = state.runtime.nextBufferId
              (state.copy(runtime = state.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))), bufferId)
            }
            loadedBuffer <- fileManager.loadFile(path, bufferId)
            // Structural mutation (adds a buffer, reorders bufferOrder, reassigns pane focus): routed through the
            // checked commit so a drifted `nextBufferId` (see #858) can't silently duplicate a bufferOrder entry or
            // overwrite a live buffer instead of being rejected.
            state <- stateRef.get
            newBufferId = loadedBuffer.id
            stateWithBuffer = state
              .copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (newBufferId -> loadedBuffer)))
            updatedState = EditorState.insertBufferInOrder(stateWithBuffer, newBufferId)
            rebalanced   = EditorState.rebalancePanes(updatedState, Some(newBufferId))
            focused      = EditorState.focusBuffer(rebalanced, newBufferId)
            resized =
              focused.runtime.viewportSize
                .map(viewportSize => com.serenity.ui.layout.LayoutEngine.syncViewportDimensions(focused, viewportSize))
                .getOrElse(focused)
            _ <- validateAndUpdateState(resized, state)
            _ <- loadedBuffer.document.language match
              case Some(languageId) =>
                val uri  = path.toUri.toString
                val text = loadedBuffer.document.content.collect()
                stateRef.get.flatMap { currentState =>
                  if currentState.editingContext.hasCodeTooling then
                    lspQueue.enqueue(LspEffect.FileOpened(uri, languageId, text))
                  else IO.unit
                }
              case None => IO.unit
            _ <- stateRef.update(s =>
              s.copy(persisted =
                s.persisted.copy(
                  recentFiles = trackRecentFile(s.persisted.recentFiles, path),
                  recentFilesByMode =
                    Persisted.trackRecentFile(s.persisted.recentFilesByMode, s.persisted.config.appMode, path)
                )
              )
            )
          yield ()

        load.handleErrorWith(ex => logger.error(ex)(s"[FILE] Failed to load file at $path")).void
    }

  private[manager] def saveBufferEffect(bufferId: BufferId): IO[Unit] =
    stateRef.get.flatMap { state =>
      state.persisted.buffers.get(bufferId) match
        case Some(buffer) if buffer.document.filePath.isDefined =>
          saveExistingBuffer(bufferId).handleErrorWith {
            case error: com.serenity.richtext.LossyRichTextOverwriteException =>
              stateRef.get.flatMap(current => workflow.showSaveAsWorkflow(current, bufferId, error.getMessage))
            case _: com.serenity.io.FileManagerError.ExternalConflict =>
              stateRef.get.flatMap(current =>
                workflow.openReloadConflictModal(current, bufferId, bufferLabelFor(buffer))
              )
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
