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
    configNotice: Option[String] = None
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
    val restoreAction = Option.when(sessionExists)(
      StartupAction(
        "restore-session",
        "Restore previous session",
        Command.typed(
          "startup.restore-session",
          "Restore an existing session",
          CommandIntent.Session(SessionIntent.StartupRestoreSession)
        ),
        detail = Some("Enter")
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
    val workflowActions = List("Writing", "Code", "Compact").map { name =>
      StartupAction(
        s"workflow-${name.toLowerCase}",
        s"Use $name workflow",
        Command
          .typed(
            s"startup.workflow.${name.toLowerCase}",
            s"Use the $name workflow",
            CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset(name))
          ),
        detail = Some("Enter"),
        section = StartupActionSection.Workflow
      )
    }
    val actions = primaryActions ++ restoreAction.toList ++ recentActions ++ workflowActions
    StartupPage(
      "Welcome to Serenity",
      options = actions.map(_.renderedLabel),
      statusMessage = statusMessage,
      actions = actions
    )

  def startPageState(
    stateManager: SessionStartupInfo & SessionService,
    theme: Theme,
    initialViewportSize: ViewportSize,
    appConfig: AppConfig = AppConfig.default,
    isTuiMode: Boolean = false,
    keyboardFidelityTier: KeyboardFidelityTier = KeyboardFidelityTier.Full,
    configNotice: Option[String] = None
  ): IO[AppState] =
    for
      sessionExists <- stateManager.sessionExists
      recentFiles   <- stateManager.loadSession().map(_.fold(Nil)(_.persisted.recentFiles))
      readableRecentFiles <- IO.blocking(
        recentFiles.filter(path => Files.isRegularFile(path) && Files.isReadable(path))
      )
      startPage = createStartPage(sessionExists, readableRecentFiles, configNotice)
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

  /** Resolve the theme to use for startup before a saved session is restored. */
  def startupTheme(
    stateManager: SessionStartupInfo,
    themeManager: AppThemeManager,
    fallbackThemeName: String = "dark"
  ): IO[Theme] =
    for
      savedThemeName <- stateManager.currentSessionThemeName
      theme          <- themeManager.initializeWithTheme(savedThemeName.getOrElse(fallbackThemeName))
    yield theme

  /** Initialize the application state for first render using the active theme and current viewport size. */
  def initializeState(
    stateManager: StateUpdater & StateReader & FileOpener & SessionStartupInfo & SessionService,
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
              // uiSurfaces is left as `base.runtime.uiSurfaces` (not reset to empty): AppState.empty already seeds it
              // with only the config-driven surfaces a fresh session should start with -- the companion sprite pane
              // when enabled -- so opening straight to a file keeps that instead of silently dropping it.
              runtime = base.runtime.copy(
                viewportSize = Some(initialViewportSize),
                isTuiMode = isTuiMode,
                keyboardFidelityTier = keyboardFidelityTier
              )
            )
          }
          _     <- stateManager.openFile(path)
          state <- stateManager.getCurrentState
        yield state
      case None =>
        for
          startState <- startPageState(
            stateManager,
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
