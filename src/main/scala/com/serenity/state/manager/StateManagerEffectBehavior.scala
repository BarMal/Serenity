package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import com.serenity.animation.AnimationConfig
import com.serenity.command.*
import com.serenity.io.{FileEntry, FileUtils}
import com.serenity.keystroke.events.ExplorerEvent
import com.serenity.lsp.LspEffect
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.{DirEntry, PanelContent, PanelPosition}

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
    stateRef.modify { state =>
      val newConfig = update(state.config)
      val newState  = withUpdatedRunnerConfig(state.copy(config = newConfig), newConfig)
      (newState, newConfig)
    }

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
        updateState(current => ModalStateReducer.show(Modal.Find("", Nil, 0), current).state)
      case CommandIntent.ReplaceInCurrentFile =>
        updateState(current => ModalStateReducer.show(Modal.ReplaceWorkflow(ReplaceWorkflowState()), current).state)
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
      case CommandIntent.FocusPanel(position) =>
        switchToPinnedPanel(position)
      case CommandIntent.UnpinPanel(position) =>
        unpinPanel(position)
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
      case CommandIntent.IncreaseFontSize =>
        updateFontConfig(config => config.copy(fontSize = clampFontSize(config.fontSize + 1.0f)))
      case CommandIntent.DecreaseFontSize =>
        updateFontConfig(config => config.copy(fontSize = clampFontSize(config.fontSize - 1.0f)))
      case CommandIntent.SetFontSize(size) =>
        updateFontConfig(_.copy(fontSize = clampFontSize(size)))
      case CommandIntent.SetUiFontSize(size) =>
        updateFontConfig(_.copy(uiFontSize = clampFontSize(size)))
      case CommandIntent.SetCodeFontFamily(family) =>
        updateFontConfig(_.copy(codeFontFamily = family))
      case CommandIntent.SetTextFontFamily(family) =>
        updateFontConfig(_.copy(textFontFamily = family))
      case CommandIntent.SetLigatures(enabled) =>
        updateFontConfig(_.copy(enableLigatures = enabled))
      case CommandIntent.ToggleLigatures =>
        updateFontConfig(config => config.copy(enableLigatures = !config.enableLigatures))
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
    if !FileUtils.isReadableFile(path) then logger.debug(s"[FILE] DirectLoad: file not readable: $path")
    else
      fileManager
        .loadFile(path)
        .flatMap { loadedBuffer =>
          stateRef.modify { state =>
            val newBufferId    = state.nextBufferId
            val bufferToInsert = loadedBuffer.copy(id = newBufferId)
            val stateWithBuffer = state.copy(
              buffers = state.buffers + (newBufferId -> bufferToInsert),
              nextBufferId = BufferId(newBufferId.value + 1)
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

  protected def trackRecentFile(current: List[Path], path: Path): List[Path] =
    (path :: current.filterNot(_ == path)).take(20)
