package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.animation.AnimationConfig
import com.serenity.command.*
import com.serenity.config.MarkdownViewMode
import com.serenity.document.{CommentRendering, DocumentNavigation, DocumentOutline}
import com.serenity.io.{FileEntry, FileUtils}
import com.serenity.keystroke.events.ExplorerEvent
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.project.*
import com.serenity.session.SessionSaveTrigger
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.*
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
        stateRef.get.flatMap(requestOpenFileDialog)
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

  protected def updateSpellCheckConfig(
    update: com.serenity.config.SpellCheckConfig => com.serenity.config.SpellCheckConfig
  ): IO[Unit] =
    updateConfig(config => config.withSpellCheck(update(config.spellCheck))).void >>
      stateRef.update(SpellChecker.refreshDiagnostics)

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
        requestSaveAsFileDialog(state, state.focusedBufferId)
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
        requestOpenFileDialog(state)
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
      case CommandIntent.ToggleCommentLens =>
        toggleCommentLens(state)
      case CommandIntent.OpenGotoLine =>
        updateState(current => ModalStateReducer.show(Modal.GotoLine(""), current).state)
      case CommandIntent.ToggleBookmark =>
        toggleBookmark(state)
      case CommandIntent.NextBookmark =>
        navigateBookmark(state, DocumentNavigation.nextSymbol)
      case CommandIntent.PreviousBookmark =>
        navigateBookmark(state, DocumentNavigation.previousSymbol)
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
      case CommandIntent.ReloadThemes =>
        themeManager.listAvailableThemes
          .flatMap(themeNamesRef.set)
          .handleErrorWith(ex => logger.error(ex)("[THEMES] Failed to reload theme list"))
      case CommandIntent.PinExplorerPanel =>
        FileUtils.getCurrentDirectory.flatMap(path =>
          interpretEffect(AppEffect.Explorer(ExplorerEffect.OpenRoot(PanelPosition.Left, path, 30)))
        )
      case CommandIntent.PinOutlinePanel =>
        val symbols = outlineSymbols(state)
        pinPanel(PanelContent.Outline(symbols, currentOutlineActiveLocation(symbols, state)), PanelPosition.Right, 30)
      case CommandIntent.PinDiagnosticsPanel =>
        pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Bottom, 10)
      case CommandIntent.OpenMarkdownPreview =>
        openMarkdownPreview(state)
      case CommandIntent.SetMarkdownViewMode(mode) =>
        setMarkdownViewMode(state, mode)
      case CommandIntent.SetSpellCheckEnabled(enabled) =>
        updateSpellCheckConfig(_.copy(enabled = enabled))
      case CommandIntent.SetSpellCheckLanguages(languages) =>
        updateSpellCheckConfig(_.copy(languages = languages))
      case CommandIntent.SetSpellCheckWords(words) =>
        updateSpellCheckConfig(_.copy(additionalWords = words))
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
        updateConfig { config =>
          mode match
            case AnimationMode.None   => config.withoutCharacterAnimation
            case AnimationMode.Quick  => config.withMotionPreset(com.serenity.config.MotionPreset.Expressive)
            case AnimationMode.Smooth => config.withMotionPreset(com.serenity.config.MotionPreset.Smooth)
            case AnimationMode.Subtle => config.withMotionPreset(com.serenity.config.MotionPreset.Subtle)
        }.void
      case CommandIntent.SetMaterialPreset(preset) =>
        updateConfig(_.withMaterialPreset(preset)).void
      case CommandIntent.SetMotionPreset(preset) =>
        updateConfig(_.withMotionPreset(preset)).void
      case CommandIntent.SetBackgroundStyle(style) =>
        updateConfig(_.withBackgroundStyle(style)).void
      case CommandIntent.SetBlurRadius(r) =>
        updateConfig(_.withBlurRadius(r)).void
      case CommandIntent.SetAnimationDuration(ms) =>
        updateConfig { config =>
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
        updateConfig { config =>
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
        updateConfig(_.withCursorMode(mode)).void
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
        requestOpenFileDialog(state)
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
          case Some((cursor, comment)) =>
            updateState { current =>
              val surface = UiSurface(
                id = SurfaceId("comment-lens"),
                content = SurfaceContent.CommentLens(comment),
                presentation = SurfacePresentation.Floating(Some(cursor), SurfacePlacement.AboveCursor),
                dismissOnMove = true
              )
              current
                .copy(uiSurfaces = current.uiSurfaces.filterNot(isCommentLensSurface) :+ surface)
                .pushFocus(Focus.Surface(surface.id))
            }
          case None =>
            logger.debug("[CMD] Comment lens requested without an active comment")

  private def activeEditorComment(state: AppState): Option[(CursorPosition, com.serenity.document.RenderedComment)] =
    for
      paneId   <- state.layout.activeEditorPaneId
      pane     <- state.layout.editorPanes.get(paneId)
      bufferId <- pane.bufferId
      buffer   <- state.buffers.get(bufferId)
      cursor   <- buffer.cursors.headOption
      comment  <- CommentRendering.atCursor(buffer)
    yield (cursor, comment)

  private def dismissCommentLens(state: AppState): AppState =
    val nextFocus = state.layout.activeEditorPaneId.map(Focus.EditorPane.apply).getOrElse(state.focus)
    state.copy(uiSurfaces = state.uiSurfaces.filterNot(isCommentLensSurface), focus = nextFocus)

  private def isCommentLensSurface(surface: UiSurface): Boolean =
    surface.content match
      case SurfaceContent.CommentLens(_) => true
      case _                             => false

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

  private def outlineSymbols(state: AppState): List[Symbol] =
    state.focusedBufferId
      .flatMap(state.buffers.get)
      .map(outlineSymbolsForBuffer)
      .getOrElse(Nil)

  private def outlineSymbolsForBuffer(buffer: Buffer): List[Symbol] =
    (DocumentOutline.forBuffer(buffer) ++ DocumentNavigation.bookmarkSymbols(buffer.bookmarks))
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
        updateState { current =>
          moveToNavigationPoint(current, after).copy(
            navigationBackStack = pushNavigationPoint(before, current.navigationBackStack),
            navigationForwardStack = Nil
          )
        }
      case Some(_) =>
        logger.debug(s"[CMD] $label navigation requested for the current location")
      case None =>
        logger.debug(s"[CMD] $label navigation requested without a target")

  private def navigateHistoryBack(): IO[Unit] =
    updateState { current =>
      current.navigationBackStack match
        case target :: remaining =>
          currentNavigationPoint(current) match
            case Some(point) =>
              moveToNavigationPoint(current, target).copy(
                navigationBackStack = remaining,
                navigationForwardStack = pushNavigationPoint(point, current.navigationForwardStack)
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
              moveToNavigationPoint(current, target).copy(
                navigationBackStack = pushNavigationPoint(point, current.navigationBackStack),
                navigationForwardStack = remaining
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
        val updatedBuffer = buffer.copy(
          cursors = List(point.cursor),
          selection = None,
          selections = Nil,
          preferredColumn = Some(point.cursor.column),
          preferredXPx = None,
          multiCursorVerticalStates = Nil
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

  private def activeEditorBuffer(state: AppState): Option[(PaneId, Buffer)] =
    for
      paneId   <- state.layout.activeEditorPaneId
      pane     <- state.layout.editorPanes.get(paneId)
      bufferId <- pane.bufferId
      buffer   <- state.buffers.get(bufferId)
    yield (paneId, buffer)

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
          logger.debug(s"[FILE] Buffer $bufferId has no file path; opening native Save As dialog") >>
            requestSaveAsFileDialog(state, Some(bufferId))
        case None =>
          logger.debug(s"[FILE] Buffer $bufferId not found for save")
    }

  protected def requestOpenFileDialog(state: AppState): IO[Unit] =
    FileUtils.getCurrentDirectory
      .flatMap(currentDirectory => fileDialog.chooseOpenFile(Some(currentDirectory)))
      .flatMap {
        case Some(path) =>
          updateState(_.copy(uiSurfaces = List.empty)) >> directLoadFileEffect(path)
        case None =>
          IO.unit
      }
      .handleErrorWith(ex => logger.error(ex)("[FILE] Native open-file dialog failed"))

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
