package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.command.*
import com.serenity.keystroke.events.{CutToDarlings, RestoreDarling}
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.*
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.{PanelPosition, PeekContent}

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
    def requestSaveAs: IO[Unit]   = currentState.flatMap(state => requestSaveAsFileDialog(state, state.focusedBufferId))
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
            PanelRegistry.registrationFor(PanelId.Explorer).defaultSize(PanelPosition.Left)
          )
      )
    def submitReplace(surfaceId: SurfaceId): IO[Unit]           = submitReplaceWorkflowEffect(surfaceId)
    def beginClose(scope: CloseScope): IO[Unit]                 = currentState.flatMap(beginCloseAction(scope, _))
    def createDirectories(surfaceId: SurfaceId): IO[Unit]       = createFileWorkflowDirectoriesEffect(surfaceId)
    def submitSessionNamePrompt(surfaceId: SurfaceId): IO[Unit] = submitSessionNamePromptEffect(surfaceId))

  private val lifecycleEffects = new LifecycleEffectHandler(
    new LifecycleEffectPort:
      def completeQuit: IO[Unit] = quitSignal.complete(()).attempt.void
  )

  private val reopenEffects     = new ReopenWithEncodingEffects(currentState, commitState, editor, fileManager)
  private val lineEndingEffects = new LineEndingEffects(currentState, commitState, updateModelValidated)
  private val manuscriptExport  = new ManuscriptExportEffects(logger, fileDialog, editor, currentState, commitState)

  private val configEffects = new StateManagerConfigEffects(
    currentState,
    logger,
    configPersistencePath,
    sessionPersistence,
    onFontConfigChanged,
    deviceTextScaleProvider,
    editor,
    runtime.renderCaches
  )

  private def commitAppValidated(transition: AppState => AppState): IO[Unit] =
    updateModelValidated(model => Some(model.copy(app = transition(model.app))))

  private val keybindingEffects =
    new StateManagerKeybindingEffects(currentState, commitAppValidated, configEffects.updateConfig)

  private val richTextEffects =
    new StateManagerRichTextEffects(currentState, updateModelValidated, interpretEffect, showPeek)

  private val projectLspEffects = new StateManagerProjectLspEffects(
    lspQueue,
    currentState,
    commitAppValidated,
    editor,
    runProjectTask,
    pinOrUpdateTerminalPanel,
    showPeek,
    showModal
  )

  private val spellingEffects =
    new StateManagerSpellingEffects(editor, currentState, interpretEffect, configEffects.addWordToDictionary)

  private val navigationEffects =
    new StateManagerNavigationEffects(
      currentState,
      logger,
      commitState,
      interpretEffect,
      wrapCache = runtime.renderCaches.wrappedLines
    )

  private val panelEffects = new StateManagerPanelEffects(
    currentState,
    logger,
    markdownPreviewWindow,
    updateModelValidated,
    enqueueEvent,
    showPeek,
    configEffects.updateConfig,
    unpinPanel,
    expandPinnedPanel,
    () => collapseExpandedPanel(),
    switchToPinnedPanel,
    resizePinnedPanel
  )

  private val externalChangeEffects = new StateManagerExternalChangeEffects(
    currentState,
    fileManager,
    isSaving,
    reloadBuffer,
    openReloadConflictModal,
    bufferLabelFor
  )

  private val editIntentEffects =
    new StateManagerEditIntentEffects(logger, enqueueEvent, updateModelValidated, activeEditorBufferId)

  export externalChangeEffects.{
    observeExternalRevisionEffect,
    observeFocusedExternalRevisionEffect,
    openBufferPathsEffect,
    resolveExternalRevisionEffect
  }
  export editIntentEffects.interpret as interpretEditIntent

  private val uiPresetEffects = new StateManagerUiPresetEffects(
    currentState,
    logger,
    uiPresetStore,
    windowSizeProvider,
    themeManager,
    onFontConfigChanged,
    sessionPersistence,
    configEffects.persistConfigFile,
    StateManagerConfigEffects.withUpdatedRunnerConfig,
    panelEffects.openMarkdownPreview,
    commitAppValidated,
    editor
  )

  private val surfacePopupEffects = new StateManagerSurfacePopupEffects(
    currentState,
    logger,
    themeManager,
    themeNamesRef,
    fileDialog,
    commitState,
    editor,
    interpretEffect
  )

  private[manager] val behavior = new CommandEffectInterpreter(
    CommandEffectInterpreter.Dependencies(
      interpretLifecycleEffect,
      interpretCommandEffect,
      interpretUnrecordedCommandEffect,
      interpretThemeEffect,
      interpretSurfaceEffect,
      interpretFileEffect,
      interpretExplorerEffect,
      interpretWorkflowEffect,
      interpretLspQueueEffect,
      scheduleCommandRunnerBindingExpiry
    )
  )

  private[manager] def interpretEffect(effect: AppEffect): IO[Unit] =
    behavior.interpret(effect)

  // Switch-latest: a binding is only recorded while none is pending, so a newer timer's binding has already replaced
  // the older one's, whose expiry would find nothing to do.
  private def scheduleCommandRunnerBindingExpiry(recordedAtMillis: Long): IO[Unit] =
    submitEffect(
      Lane.Keyed(LaneKey.Timer, LanePolicy.SwitchLatest),
      IO.sleep(DoubleTapWindow) >>
        dispatchEffectResult(EffectResult.CommandRunnerBindingExpired(recordedAtMillis), _ => IO.unit)
    )

  private def interpretLifecycleEffect: IO[Unit] =
    lifecycleEffects.interpret

  private def interpretCommandEffect(command: Command): IO[Unit] =
    currentState.flatMap(state => interpretCommand(command, state))

  private def interpretUnrecordedCommandEffect(command: Command): IO[Unit] =
    currentState.flatMap(state => scopeCheckedCommand(command, state, recordUsage = false))

  private def interpretThemeEffect(effect: ThemeEffect): IO[Unit] =
    surfacePopupEffects.interpretThemeEffect(effect)

  private def interpretSurfaceEffect(effect: SurfaceEffect): IO[Unit] =
    effect match
      case SurfaceEffect.OpenThemePicker =>
        currentState.flatMap(surfacePopupEffects.openThemePickerEffect)
      case SurfaceEffect.OpenThemeCreator =>
        currentState.flatMap(surfacePopupEffects.openThemeCreatorEffect)
      case SurfaceEffect.OpenFileSearch =>
        currentState.flatMap(surfacePopupEffects.openFileSearchEffect)

  private def interpretFileEffect(effect: FileEffect): IO[Unit] =
    effect match
      case FileEffect.SaveBuffer(bufferId)         => saveBufferEffect(bufferId)
      case FileEffect.SaveBufferAs(bufferId, path) => saveBufferAsEffect(bufferId, path)
      case FileEffect.DirectLoadFile(path)         => directLoadFileEffect(path)

  private def interpretExplorerEffect(effect: ExplorerEffect): IO[Unit] =
    effect match
      case ExplorerEffect.OpenRoot(position, path, size) =>
        panelEffects.pinExplorerPanelEffect(position, path, size)

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
  // palette via ComponentResult.ExecuteCommand, and StateManagerComposition.executeCommand, the test harness's entry
  // point since #1724) calls this, so logging the [COMMAND] line here logs each command exactly once regardless of
  // how it was triggered -- rather than only on the effect path, which used to leave palette/mouse-driven commands
  // silent.
  private[manager] def interpretCommand(command: Command, state: AppState): IO[Unit] =
    scopeCheckedCommand(command, state, recordUsage = true)

  private def scopeCheckedCommand(command: Command, state: AppState, recordUsage: Boolean): IO[Unit] =
    // Every entry path lands here, so this one check also covers hotkeys, menus and the toolbar -- which is what
    // keeps a command hidden from the palette in this mode/frontend from still running by another route.
    command.scope.unavailableReason(state.editingContext) match
      case Some(reason) =>
        logger.info(s"[COMMAND] refused ${command.name}: $reason") >>
          showPeek(PeekContent.QuickInfo(reason), state.activeCursorPosition.getOrElse(CursorPosition(0, 0)))
      case None => dispatchCommand(command, state, recordUsage)

  private def dispatchCommand(command: Command, state: AppState, recordUsage: Boolean): IO[Unit] =
    val dispatch = command.intent match
      case CommandIntent.Lifecycle(intent)                      => interpretLifecycleIntent(intent, state)
      case CommandIntent.File(intent)                           => interpretFileIntent(intent, state)
      case CommandIntent.Edit(intent)                           => interpretEditIntent(intent)
      case CommandIntent.RichText(intent)                       => richTextEffects.interpret(intent)
      case CommandIntent.Comments(intent)                       => navigationEffects.interpretComments(intent)
      case CommandIntent.Placeholders(intent)                   => navigationEffects.interpretPlaceholders(intent)
      case CommandIntent.Darlings(DarlingIntent.CutToDarlings)  => enqueueEvent(CutToDarlings)
      case CommandIntent.Darlings(DarlingIntent.RestoreDarling) => enqueueEvent(RestoreDarling)
      case CommandIntent.Spelling(intent)                       => spellingEffects.interpret(intent)
      case CommandIntent.Navigation(intent)                     => navigationEffects.interpretNavigation(intent)
      case CommandIntent.Lsp(intent)                            => projectLspEffects.interpretLsp(intent, state)
      case CommandIntent.Theme(intent)       => surfacePopupEffects.interpretThemeIntent(intent, state)
      case CommandIntent.View(intent)        => panelEffects.interpret(intent, state)
      case CommandIntent.Project(intent)     => projectLspEffects.interpretProject(intent, state)
      case CommandIntent.Session(intent)     => interpretSessionIntent(intent, state)
      case CommandIntent.Keybindings(intent) => keybindingEffects.interpret(intent)
      case CommandIntent.UiPresets(intent)   => uiPresetEffects.interpret(intent)
      case CommandIntent.Settings(intent)    => configEffects.interpret(intent, state)
    // issue #1048: MRU tracking -- every executed registry command counts toward its recency, regardless of what
    // triggered it (palette, hotkey, mouse click, ...), living on `persisted` since `CommandRunner` itself is
    // reconstructed fresh each time the palette opens (`CommandRunner.recordCommandUsage`'s own doc). Left out:
    // commands the user did not choose (`AppEffect.ExecuteCommandUnrecorded`: picker previews and restores), and
    // anything the palette cannot offer -- settings rows, theme picks, toolbar buttons -- whose ids would only clutter
    // the table (#1877).
    logger.info(s"[COMMAND] ${StateManager.describeCommandExecution(command)}") >>
      updateModelValidated(model =>
        Some(model.copy(app = StateManagerEffectHandlers.withCommandUsageRecorded(model.app, command.name)))
      ).whenA(recordUsage && StateManagerEffectHandlers.recordsUsage(command.name)) >> dispatch

  private def interpretLifecycleIntent(intent: LifecycleIntent, state: AppState): IO[Unit] =
    intent match
      case LifecycleIntent.QuitApp              => beginCloseAction(CloseScope.Quit, state)
      case LifecycleIntent.Restart(mode)        => beginCloseAction(CloseScope.Restart(mode), state)
      case LifecycleIntent.ResolveClose(choice) => resolveClose(choice)

  private def interpretFileIntent(intent: FileIntent, state: AppState): IO[Unit] =
    intent match
      case FileIntent.SaveCurrentFile =>
        state.focusedBufferId match
          case Some(bufferId) => saveBufferEffect(bufferId)
          case None           => logger.debug("[CMD] No focused buffer to save")
      case FileIntent.SaveCurrentFileAs =>
        requestSaveAsFileDialog(state, state.focusedBufferId)
      case FileIntent.ExportManuscript(request) => manuscriptExport.run(request, state)
      case FileIntent.OpenFile =>
        requestOpenFileDialog
      case FileIntent.OpenRecentFile(path) =>
        loadFile(path)
      case FileIntent.OpenFileSearch =>
        interpretSurfaceEffect(SurfaceEffect.OpenFileSearch)
      case FileIntent.GoToFile =>
        openFileFinder
      case FileIntent.CloseAll =>
        beginCloseAction(CloseScope.All, state)
      case FileIntent.CloseOthers =>
        beginCloseAction(CloseScope.Others, state)
      case FileIntent.CloseCurrentFile =>
        beginCloseAction(CloseScope.Current, state)
      case FileIntent.NewFile =>
        val registry = CommandRegistry.withToggleUI
        currentState.flatMap(current =>
          commitState(
            AppEventReducer.reduce(com.serenity.keystroke.events.NewTab, current, registry)(using balance).state,
            current
          )
        )
      case FileIntent.ShowLicenceAndNotices | FileIntent.ShowAbout => com.serenity.io.AboutDocument.open(loadFile)
      case FileIntent.OpenReleasesPage            => ReleasesPageEffect.open(openExternalUrl, showNotice)
      case FileIntent.ShowPrivacyStatement        => com.serenity.io.PrivacyStatement.open(loadFile)
      case FileIntent.SetBufferLanguage(language) => setBufferLanguage(state, language)
      case FileIntent.ReloadFromDisk(bufferId) =>
        reloadBuffer(bufferId)
      case FileIntent.OverwriteOnDisk(bufferId) =>
        forceSaveExistingBuffer(bufferId)
      case FileIntent.SaveWithoutFormatting(bufferId) =>
        currentState.flatMap(current => commitState(RichTextReducer.withoutFormatting(bufferId, current), current)) >>
          saveBufferEffect(bufferId)
      case FileIntent.ChooseReopenEncoding => reopenEffects.chooseEncoding
      case FileIntent.ReopenWithEncoding(bufferId, encoding, discardEdits) =>
        reopenEffects.reopen(bufferId, encoding, discardEdits)
      case FileIntent.ChooseLineEnding                => lineEndingEffects.chooseLineEnding
      case FileIntent.SetLineEnding(bufferId, ending) => lineEndingEffects.setLineEnding(bufferId, ending)

  private def setBufferLanguage(state: AppState, language: Option[LanguageId]): IO[Unit] =
    (state.focusedBufferId, state.focusedBufferId.flatMap(state.persisted.buffers.get)) match
      case (Some(bufferId), Some(buffer)) =>
        val updateLanguage =
          currentState.flatMap(current =>
            commitState(
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
              val text = buffer.document.content
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

  private def interpretSessionIntent(intent: SessionIntent, state: AppState): IO[Unit] =
    intent match
      case SessionIntent.SaveSession =>
        saveSession()
      case SessionIntent.RestoreSession =>
        loadSession().flatMap {
          case Some(restored) => commitState(restoreSessionIntoCurrentViewport(restored, state), state)
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
      case SessionIntent.OpenNamedSession(sessionId) =>
        openNamedSession(sessionId, state)
      case SessionIntent.RenameNamedSession(sessionId, currentName) =>
        openRenameSessionPrompt(sessionId, currentName)

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
    currentState.flatMap { state =>
      state.persisted.buffers.get(bufferId) match
        case Some(buffer) if buffer.formattingLostOnSave =>
          val prompt = ConfirmPrompt.formattingWouldBeLost(bufferId, bufferLabelFor(buffer))
          commitState(ModalStateReducer.show(Modal.Confirm(prompt), state).state, state)
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
        currentState.flatMap(current => workflow.showSaveAsWorkflow(current, bufferId, lossy.getMessage))
      case _: com.serenity.io.FileManagerError.ExternalConflict =>
        // Label from state re-read after the failure, not the pre-save `buffer` snapshot above -- keeps this
        // consistent with StateManagerWorkflowCapability's own ExternalConflict handler, which does the same
        // (code review finding on PR #1664: the two copies previously sourced the label from different points
        // in time, which could show different labels for the same conflict if the buffer changed in between).
        currentState.flatMap(current =>
          workflow.openReloadConflictModal(current, bufferId, bufferLabelFor(current, bufferId))
        )
      case other =>
        logger.error(other)(s"[FILE] Failed to save buffer $bufferId") >>
          currentState.flatMap(current => showNotice(FileFailureNotice.forBuffer(current, bufferId, other)))

  protected def requestOpenFileDialog: IO[Unit] =
    fileDialog match
      case Some(dialog) =>
        openFromDialog(dialog)
      case None =>
        // No native dialog to show at all -- fall back to the in-app form, same as the save-as path.
        currentState.flatMap(state => openFileWorkflowModal(FileWorkflowMode.Open, state))

  private[manager] def saveBufferAsEffect(bufferId: BufferId, path: Path): IO[Unit] =
    currentState.flatMap { state =>
      state.persisted.buffers.get(bufferId) match
        case Some(_) =>
          saveBufferAs(bufferId, path).handleErrorWith(error =>
            showNotice(FileFailureNotice.fileSaveFailed(bufferId, path, error, state.persisted.config))
          )
        case None =>
          logger.debug(s"[FILE] Buffer $bufferId not found for save as")
    }

  private[manager] def updateFontConfig(
    update: com.serenity.ui.fonts.FontLoader.FontConfig => com.serenity.ui.fonts.FontLoader.FontConfig
  ): IO[Unit] =
    configEffects.updateFontConfig(update)

private[manager] object StateManagerEffectHandlers:

  def recordsUsage(commandName: String): Boolean =
    CommandRegistry.withToggleUI.isRegistered(CommandId(commandName))

  def withCommandUsageRecorded(state: AppState, commandName: String): AppState =
    if !recordsUsage(commandName) then state
    else
      state.copy(persisted =
        state.persisted.copy(commandUsage =
          CommandUsageHistory.recorded(state.persisted.commandUsage, CommandId(commandName))
        )
      )
