package com.serenity.app

import java.nio.file.{Files, Path}

import cats.effect.IO
import com.serenity.command.{Command, CommandIntent, FileIntent, SessionIntent, UiPresetsIntent}
import com.serenity.config.AppConfig
import com.serenity.keystroke.KeyboardFidelityTier
import com.serenity.state.manager.*
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.AppThemeManager

object AppStartup:

  /** `recentFiles` must already be filtered to existing, readable files -- `createStartPage` does no filesystem access
    * of its own, so callers filter via `IO.blocking` before calling this.
    */
  def createStartPage(
    sessionExists: Boolean,
    recentFiles: List[Path] = Nil,
    configNotice: Option[String] = None,
    resumeIdentifier: Option[String] = None
  ): StartupPage =
    // A configuration that could not be read takes the status line: it is the more consequential of the two, and the
    // start page is on screen at exactly the moment it happened. Its only other report is a log line the TUI discards.
    val statusMessage =
      configNotice.orElse(Option.when(!sessionExists)("No previous session found"))

    val primaryActions = List(
      StartupAction(
        "new-session",
        "New document",
        Command
          .typed("startup.new-session", "Start a new session", CommandIntent.Session(SessionIntent.StartupNewSession)),
        Some('1'),
        Some("Enter")
      ),
      StartupAction(
        "open-file",
        "Open file or folder",
        Command.typed(
          "startup.open-file",
          "Open an existing file or directory",
          CommandIntent.Session(SessionIntent.StartupOpenFile)
        ),
        Some('2'),
        Some("Enter")
      )
    )
    val resume = Option.when(sessionExists)(
      StartupResumeHint(
        resumeIdentifier.getOrElse("previous session"),
        Command.typed(
          "startup.restore-session",
          "Restore an existing session",
          CommandIntent.Session(SessionIntent.StartupRestoreSession)
        )
      )
    )
    val recentActions = recentFiles
      .map(path => path.toAbsolutePath.normalize())
      .distinct
      .take(5)
      .map { path =>
        StartupAction(
          s"recent:${path.toString}",
          path.toString,
          Command.typed(
            s"startup.open-recent.${path.getFileName}",
            s"Open recent file $path",
            CommandIntent.File(FileIntent.OpenRecentFile(path))
          ),
          detail = Some("Recent")
        )
      }
    val workflowActions = List(("Writing", 'W'), ("Code", 'C'), ("Compact", 'M')).map { (name, key) =>
      StartupAction(
        s"workflow-${name.toLowerCase}",
        name,
        Command
          .typed(
            s"startup.workflow.${name.toLowerCase}",
            s"Use the $name workflow",
            CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset(name))
          ),
        shortcut = Some(key),
        section = StartupActionSection.Workflow
      )
    }
    val actions = primaryActions ++ recentActions
    StartupPage(
      "Welcome to Serenity",
      options = actions.map(_.renderedLabel),
      statusMessage = statusMessage,
      actions = actions,
      workflows = workflowActions,
      resume = resume
    )

  /** The previous session's main file name (active pane's buffer, else the first buffered file), for the quick-resume
    * hint -- so the user recognises what Tab would resume. Falls back to a generic label for an empty/file-less session.
    */
  private def sessionResumeIdentifier(session: AppState): String =
    val persisted = session.persisted
    val activeFile = persisted.layout.activeEditorPaneId
      .flatMap(persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(persisted.buffers.get)
      .flatMap(_.document.filePath)
    val fallbackFile =
      persisted.bufferOrder.flatMap(persisted.buffers.get).flatMap(_.document.filePath).headOption
    activeFile
      .orElse(fallbackFile)
      .flatMap(path => Option(path.getFileName).map(_.toString))
      .getOrElse("previous session")

  def startPageState(
    sessionService: SessionService,
    sessionStartupInfo: SessionStartupInfo,
    theme: Theme,
    initialViewportSize: ViewportSize,
    appConfig: AppConfig = AppConfig.default,
    isTuiMode: Boolean = false,
    keyboardFidelityTier: KeyboardFidelityTier = KeyboardFidelityTier.Full,
    configNotice: Option[String] = None
  ): IO[AppState] =
    for
      sessionExists <- sessionStartupInfo.sessionExists
      loadedSession <- sessionService.loadSession
      recentFiles = loadedSession.fold(List.empty[Path])(_.persisted.recentFiles)
      readableRecentFiles <- IO.blocking(
        recentFiles.filter(path => Files.isRegularFile(path) && Files.isReadable(path))
      )
      resumeIdentifier = loadedSession.map(sessionResumeIdentifier)
      startPage        = createStartPage(sessionExists, readableRecentFiles, configNotice, resumeIdentifier)
    yield
      val startPageSurfaceId = SurfaceId("surface-0")
      val base               = AppState.empty(appConfig)
      base.copy(
        persisted = base.persisted.copy(
          focus = Focus.Surface(startPageSurfaceId),
          theme = theme
        ),
        runtime = base.runtime.copy(
          uiSurfaces = List(
            UiSurface(
              id = startPageSurfaceId,
              content = SurfaceContent.StartPage(startPage),
              presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
            )
          ),
          viewportSize = Some(initialViewportSize),
          nextSurfaceId = 1,
          isTuiMode = isTuiMode,
          keyboardFidelityTier = keyboardFidelityTier
        )
      )

  def startupTheme(
    sessionStartupInfo: SessionStartupInfo,
    themeManager: AppThemeManager,
    fallbackThemeName: String = "dark"
  ): IO[Theme] =
    for
      savedThemeName <- sessionStartupInfo.currentSessionThemeName
      theme          <- themeManager.initializeWithTheme(savedThemeName.getOrElse(fallbackThemeName))
    yield theme

  def initializeState(
    stateManager: StateManager,
    sessionStartupInfo: SessionStartupInfo,
    theme: Theme,
    initialViewportSize: ViewportSize,
    appConfig: AppConfig = AppConfig.default,
    openPath: Option[Path] = None,
    isTuiMode: Boolean = false,
    keyboardFidelityTier: KeyboardFidelityTier = KeyboardFidelityTier.Full,
    configNotice: Option[String] = None
  ): IO[AppState] =
    openPath match
      case Some(path) =>
        for
          _ <- stateManager.updateState { _ =>
            val base = AppState.empty(appConfig)
            base.copy(
              persisted = base.persisted.copy(theme = theme),
              // The companion sprite surface is added together with its tree dock below, once `openFile` has built a
              // real workspace tree to dock it into -- adding it here (as `AppState.empty` otherwise would) leaves
              // uiSurfaces carrying a `Docked` surface the tree doesn't have yet, which fails `openFile`'s own state
              // validation (issue #817) and silently discards the newly-opened buffer entirely.
              runtime = base.runtime.copy(
                uiSurfaces = Nil,
                viewportSize = Some(initialViewportSize),
                isTuiMode = isTuiMode,
                keyboardFidelityTier = keyboardFidelityTier
              )
            )
          }
          _ <- stateManager.fileOpener.openFile(path)
          // Now that `openFile` has built a real workspace tree for the newly-opened buffer's pane, add the
          // companion sprite surface (if enabled) and dock it in the same update, so uiSurfaces and the tree change
          // together instead of passing through an inconsistent intermediate state (issue #817: idempotent/no-op if
          // it's already present, already docked, or the sprite is disabled).
          _ <- stateManager.updateState { state =>
            state.copy(
              persisted = state.persisted
                .copy(layout = AppState.dockCompanionSprite(state.persisted.layout, state.persisted.config)),
              runtime = state.runtime.copy(uiSurfaces =
                state.runtime.uiSurfaces ++ AppState
                  .companionSpriteSurfaces(state.persisted.config)
                  .filterNot(surface => state.runtime.uiSurfaces.exists(_.id == surface.id))
              )
            )
          }
          state <- stateManager.getCurrentState
        yield state
      case None =>
        for
          startState <- startPageState(
            stateManager.sessionService,
            sessionStartupInfo,
            theme,
            initialViewportSize,
            appConfig,
            isTuiMode,
            keyboardFidelityTier,
            configNotice
          )
          _     <- stateManager.updateState(_ => startState)
          state <- stateManager.getCurrentState
        yield state
