package com.serenity.app

import java.nio.file.{Files, Path}

import cats.effect.IO
import com.serenity.command.{Command, CommandIntent}
import com.serenity.config.AppConfig
import com.serenity.state.manager.*
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.AppThemeManager

object AppStartup:

  def createStartPage(sessionExists: Boolean, recentFiles: List[Path] = Nil): StartupPage =
    val statusMessage =
      if sessionExists then None
      else Some("No previous session found")

    val primaryActions = List(
      StartupAction("new-session", "New document", Command.typed("startup.new-session", "Start a new session", CommandIntent.StartupNewSession), Some('1'), Some("Enter")),
      StartupAction("open-file", "Open file or folder", Command.typed("startup.open-file", "Open an existing file or directory", CommandIntent.StartupOpenFile), Some('2'), Some("Enter"))
    )
    val restoreAction = Option.when(sessionExists)(
      StartupAction("restore-session", "Restore previous session", Command.typed("startup.restore-session", "Restore an existing session", CommandIntent.StartupRestoreSession), detail = Some("Enter"))
    )
    val recentActions = recentFiles
      .filter(path => Files.isRegularFile(path) && Files.isReadable(path))
      .map(path => path.toAbsolutePath.normalize())
      .distinct
      .take(5)
      .map { path =>
        StartupAction(
          s"recent:${path.toString}",
          path.toString,
          Command.typed(s"startup.open-recent.${path.getFileName}", s"Open recent file $path", CommandIntent.OpenRecentFile(path)),
          detail = Some("Recent")
        )
      }
    val workflowActions = List("Writing", "Code", "Compact").map { name =>
      StartupAction(
        s"workflow-${name.toLowerCase}",
        s"Use $name workflow",
        Command.typed(s"startup.workflow.${name.toLowerCase}", s"Use the $name workflow", CommandIntent.ApplyUiPreset(name)),
        detail = Some("Enter")
      )
    }
    val actions = primaryActions ++ restoreAction.toList ++ recentActions ++ workflowActions
    StartupPage("Welcome to Serenity", options = actions.map(_.renderedLabel), statusMessage = statusMessage, actions = actions)

  def startPageState(
    stateManager: SessionStartupInfo & SessionService,
    theme: Theme,
    initialViewportSize: ViewportSize,
    appConfig: AppConfig = AppConfig.default
  ): IO[AppState] =
    for
      sessionExists <- stateManager.sessionExists
      recentFiles   <- stateManager.loadSession().map(_.fold(Nil)(_.recentFiles))
      startPage = createStartPage(sessionExists, recentFiles)
    yield
      val startPageSurfaceId = SurfaceId("surface-0")
      AppState.empty.copy(
        focus = Focus.Surface(startPageSurfaceId),
        config = appConfig,
        uiSurfaces = List(
          UiSurface(
            id = startPageSurfaceId,
            content = SurfaceContent.StartPage(startPage),
            presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        ),
        viewportSize = Some(initialViewportSize),
        theme = theme,
        nextSurfaceId = 1
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
    openPath: Option[Path] = None
  ): IO[AppState] =
    openPath match
      case Some(path) =>
        for
          _ <- stateManager.updateState(_ =>
            AppState.empty.copy(
              uiSurfaces = List.empty,
              config = appConfig,
              viewportSize = Some(initialViewportSize),
              theme = theme
            )
          )
          _     <- stateManager.openFile(path)
          state <- stateManager.getCurrentState
        yield state
      case None =>
        for
          startState <- startPageState(stateManager, theme, initialViewportSize, appConfig)
          _          <- stateManager.updateState(_ => startState)
          state      <- stateManager.getCurrentState
        yield state
