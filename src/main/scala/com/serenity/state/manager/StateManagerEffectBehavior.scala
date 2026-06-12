package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.animation.AnimationConfig
import com.serenity.command.*
import com.serenity.config.MarkdownViewMode
import com.serenity.io.{FileEntry, FileUtils}
import com.serenity.keystroke.events.ExplorerEvent
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.session.SessionSaveTrigger
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.{DirEntry, PanelContent, PanelPosition}
import com.serenity.ui.presets.UiPreset

private[manager] trait StateManagerEffectBehavior extends StateManagerWorkflowBehavior:
  this: StateManager =>

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

  private def interpretSurfaceEffect(effect: SurfaceEffect): IO[Unit] =
    effect match
      case SurfaceEffect.OpenThemePicker =>
        stateRef.get.flatMap(openThemePickerEffect)
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
        stateRef.get.flatMap(state => openFileWorkflowModal(FileWorkflowMode.Open, state))
      case WorkflowEffect.RequestSaveAs =>
        stateRef.get.flatMap(state => openFileWorkflowModal(FileWorkflowMode.SaveAs, state))
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
    state.commandRunnerSurface match
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            val updatedRunner = runner.updateInputItems(config)
            val updatedSurfaces = state.uiSurfaces.map {
              case current if current.id == surface.id =>
                current.copy(content = SurfaceContent.CommandPalette(updatedRunner))
              case current @ UiSurface(_, SurfaceContent.CommandPaletteSubmenu(_, groupId, previewOnly), _, _) =>
                current.copy(content = SurfaceContent.CommandPaletteSubmenu(updatedRunner, groupId, previewOnly))
              case other => other
            }
            state.copy(uiSurfaces = updatedSurfaces)
          case _ => state
      case None => state

  protected def updateConfig(
    update: com.serenity.config.AppConfig => com.serenity.config.AppConfig
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
      .flatTap(_ =>
        stateRef.get
          .flatMap(state => sessionPersistence.maybeSaveSession(state, SessionSaveTrigger.Manual))
          .handleErrorWith(error => logger.error(error)("[SESSION] Auto-save after config change failed"))
      )

  protected def updateFontConfig(
    update: com.serenity.ui.fonts.FontLoader.FontConfig => com.serenity.ui.fonts.FontLoader.FontConfig
  ): IO[Unit] =
    updateConfig(config => config.withFontConfig(update(config.fontConfig)))
      .flatMap(config => onFontConfigChanged(config.fontConfig))

  protected def clampFontSize(size: Float): Float =
    size.max(8.0f).min(48.0f)

  protected def interpretCommand(command: Command, state: AppState): IO[Unit] =
    command.intent match
      case CommandIntent.ToggleLineNumbers =>
        updateState(s => s.copy(config = s.config.copy(showLineNumbers = !s.config.showLineNumbers)))
      case CommandIntent.ToggleGutter =>
        updateState(s => s.copy(config = s.config.copy(showGutter = !s.config.showGutter)))
      case CommandIntent.SaveCurrentFile =>
        state.focusedBufferId match
          case Some(bufferId) => saveBufferEffect(bufferId)
          case None           => logger.debug("[CMD] No focused buffer to save")
      case CommandIntent.SaveCurrentFileAs =>
        openFileWorkflowModal(FileWorkflowMode.SaveAs, state)
      case CommandIntent.SaveSession =>
        saveSession()
      case CommandIntent.RestoreSession =>
        loadSession().flatMap {
          case Some(restored) => validateAndUpdateState(restored, state)
          case None           => logger.debug("[SESSION] Restore requested without a saved session")
        }
      case CommandIntent.ClearSession =>
        clearSession()
      case CommandIntent.OpenFile =>
        openFileWorkflowModal(FileWorkflowMode.Open, state)
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
      case CommandIntent.OpenGotoLine =>
        updateState(current => ModalStateReducer.show(Modal.GotoLine(""), current).state)
      case CommandIntent.ToggleTheme =>
        toggleThemeEffect(state)
      case CommandIntent.ReloadTheme =>
        reloadThemeEffect(state)
      case CommandIntent.OpenThemeChooser =>
        openThemePickerEffect(state)
      case CommandIntent.ReloadThemes =>
        themeManager.listAvailableThemes
          .flatMap(themeNamesRef.set)
          .handleErrorWith(ex => logger.error(ex)("[THEMES] Failed to reload theme list"))
      case CommandIntent.PinExplorerPanel =>
        FileUtils.getCurrentDirectory.flatMap(path =>
          interpretEffect(AppEffect.Explorer(ExplorerEffect.OpenRoot(PanelPosition.Left, path, 30)))
        )
      case CommandIntent.PinOutlinePanel =>
        pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30)
      case CommandIntent.PinDiagnosticsPanel =>
        pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Bottom, 10)
      case CommandIntent.OpenMarkdownPreview =>
        openMarkdownPreview(state)
      case CommandIntent.SetMarkdownViewMode(mode) =>
        setMarkdownViewMode(state, mode)
      case CommandIntent.SetInterfaceDensity(density) =>
        updateConfig(_.withInterfaceDensity(density)).void
      case CommandIntent.FocusPanel(position) =>
        switchToPinnedPanel(position)
      case CommandIntent.UnpinPanel(position) =>
        unpinPanel(position)
      case CommandIntent.ExpandPanel(position) =>
        expandPinnedPanel(position)
      case CommandIntent.CollapseExpandedPanel =>
        collapseExpandedPanel()
      case CommandIntent.FormatCurrentFile =>
        logger.debug("[CMD] Format command requested")
      case CommandIntent.SetAnimationMode(mode) =>
        updateState { s =>
          mode match
            case AnimationMode.None =>
              s.copy(config = s.config.withoutCharacterAnimation)
            case AnimationMode.Quick =>
              s.copy(config = s.config.copy(characterAnimation = AnimationConfig.quick))
            case AnimationMode.Smooth =>
              s.copy(config = s.config.copy(characterAnimation = AnimationConfig.smooth))
            case AnimationMode.Subtle =>
              s.copy(config = s.config.copy(characterAnimation = AnimationConfig.subtle))
        }
      case CommandIntent.SetBackgroundStyle(style) =>
        updateState { s =>
          val newConfig = s.config.withBackgroundStyle(style)
          withUpdatedRunnerConfig(s.copy(config = newConfig), newConfig)
        }
      case CommandIntent.SetBlurRadius(r) =>
        updateState { s =>
          val newConfig = s.config.withBlurRadius(r)
          withUpdatedRunnerConfig(s.copy(config = newConfig), newConfig)
        }
      case CommandIntent.SetAnimationDuration(ms) =>
        updateState { s =>
          val newAnim =
            if ms <= 0 then None
            else
              Some(
                s.config.characterAnimation.fold(
                  AnimationConfig(
                    steps = 12,
                    totalDuration = scala.concurrent.duration.Duration.fromNanos(ms * 1_000_000L)
                  )
                )(existing =>
                  existing.copy(totalDuration = scala.concurrent.duration.Duration.fromNanos(ms * 1_000_000L))
                )
              )
          val newConfig = s.config.copy(characterAnimation = newAnim)
          withUpdatedRunnerConfig(s.copy(config = newConfig), newConfig)
        }
      case CommandIntent.SetAnimationSteps(n) =>
        updateState { s =>
          val newAnim =
            if n <= 0 then None
            else
              Some(
                s.config.characterAnimation.fold(
                  AnimationConfig(
                    steps = n,
                    totalDuration = scala.concurrent.duration.Duration.fromNanos(200_000_000L)
                  )
                )(existing => existing.copy(steps = n))
              )
          val newConfig = s.config.copy(characterAnimation = newAnim)
          withUpdatedRunnerConfig(s.copy(config = newConfig), newConfig)
        }
      case CommandIntent.SetCursorMode(mode) =>
        updateState { s =>
          val newConfig = s.config.withCursorMode(mode)
          withUpdatedRunnerConfig(s.copy(config = newConfig), newConfig)
        }
      case CommandIntent.SetCursorInfoBarMode(mode) =>
        updateConfig(_.withCursorInfoBarMode(mode)).void
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
      case CommandIntent.SaveUiPreset(name) =>
        saveUiPresetEffect(name)
      case CommandIntent.ApplyUiPreset(name) =>
        applyUiPresetEffect(name)
      case CommandIntent.SetTextAreaLeftInset(value) =>
        updateConfig(_.withTextAreaLeftInset(value)).void
      case CommandIntent.SetTextAreaRightInset(value) =>
        updateConfig(_.withTextAreaRightInset(value)).void
      case CommandIntent.ToggleLigatures =>
        updateFontConfig(config =>
          config.copy(enableLigatures = !config.enableLigatures, textLigatures = !config.textLigatures)
        )
      case CommandIntent.StartupNewSession =>
        createStartupSession()
      case CommandIntent.StartupRestoreSession =>
        restoreStartupSession()
      case CommandIntent.StartupOpenFile =>
        openFileWorkflowModal(FileWorkflowMode.Open, state)
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
        yield ()

  protected def applyUiPresetEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")
      case Some(presetName) =>
        uiPresetStore
          .find(presetName)
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
                preferredWindowSize <- stateRef.modify { state =>
                  val restored = withUpdatedRunnerConfig(UiPreset.applyToState(preset, state, theme), preset.config)
                  (restored, restored.config.preferredWindowSize)
                }
                _ <- persistConfigFile(preset.config)
                _ <- onFontConfigChanged(preset.config.fontConfig)
                  .handleErrorWith(error => logger.error(error)("[PRESET] Failed to apply preset font config"))
                _ <- preferredWindowSize.traverse_(size =>
                  onPreferredWindowSizeChanged(size)
                    .handleErrorWith(error => logger.error(error)("[PRESET] Failed to apply preset window size"))
                )
                _ <- reloadPresetDirectories(preset)
                _ <- stateRef.get
                  .flatMap(state => sessionPersistence.maybeSaveSession(state, SessionSaveTrigger.Manual))
                  .handleErrorWith(error => logger.error(error)("[SESSION] Auto-save after preset apply failed"))
              yield ()
          }
          .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to apply UI preset $presetName"))

  private def normalizedPresetName(name: String): Option[String] =
    Option(name.trim).filter(_.nonEmpty)

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

  private def openMarkdownPreview(state: AppState): IO[Unit] =
    state.focusedBufferId
      .flatMap(state.buffers.get)
      .filter(_.language.contains(LanguageId.Markdown))
      .map { buffer =>
        val title = buffer.filePath
          .flatMap(path => Option(path.getFileName).map(_.toString))
          .getOrElse("Untitled")
        pinPanel(
          PanelContent.MarkdownPreview(buffer.id, title),
          PanelPosition.Right,
          40
        )
      }
      .getOrElse(logger.debug("[CMD] Markdown preview requested without an active Markdown buffer"))

  private def setMarkdownViewMode(state: AppState, mode: MarkdownViewMode): IO[Unit] =
    val updateConfig = updateState { s =>
      val newConfig = s.config.withMarkdownViewMode(mode)
      withUpdatedRunnerConfig(s.copy(config = newConfig), newConfig)
    }
    mode match
      case MarkdownViewMode.SplitPreview =>
        updateConfig >> openMarkdownPreview(state)
      case MarkdownViewMode.Source | MarkdownViewMode.InlineLens =>
        updateConfig >> unpinMarkdownPreviewPanel()

  private def unpinMarkdownPreviewPanel(): IO[Unit] =
    updateState { state =>
      val markdownPreviewPositions = state.pinnedSurfaces.collect {
        case UiSurface(_, SurfaceContent.MarkdownPreview(_, _), SurfacePresentation.Pinned(position, _), _) => position
      }
      markdownPreviewPositions.foldLeft(state)((current, position) => PanelStateReducer.unpin(position, current).state)
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
              (focused, loadedBuffer)
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
          logger.debug(s"[FILE] Buffer $bufferId has no file path; opening Save As workflow") >>
            openFileWorkflowModal(FileWorkflowMode.SaveAs, state, Some(bufferId))
        case None =>
          logger.debug(s"[FILE] Buffer $bufferId not found for save")
    }

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
    else
      val text = buffer.content.collect()
      buffer.content.searchAll(query).map(offset => cursorPositionForOffset(text, offset))

  private def toFindResult(cursor: CursorPosition): FindResult =
    FindResult(cursor.line, cursor.column)

  private def cursorPositionForOffset(text: String, offset: Int): CursorPosition =
    val clamped = math.max(0, math.min(offset, text.length))
    text.take(clamped).foldLeft(CursorPosition(0, 0)) { (cursor, char) =>
      if char == '\n' then CursorPosition(cursor.line + 1, 0)
      else cursor.copy(column = cursor.column + 1)
    }

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
            else Some(ThemeTransition(state.theme, 0, AnimationConfig.smooth.get.steps))
          state.copy(theme = newTheme, themeTransition = transition)
        }
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
