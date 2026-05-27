package com.serenity.app

import cats.effect.IO
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, Focus, StartupPage, SurfaceContent, SurfaceId, SurfacePlacement, SurfacePresentation, UiSurface}
import com.serenity.ui.layout.TerminalSize
import com.serenity.ui.theme.Theme

object AppStartup:

  val defaultStartPage: StartupPage = StartupPage(
    title = "What would you like to do?",
    options = List(
      "1. Start a new session",
      "2. Restore an existing session",
      "3. Open an existing file or directory"
    )
  )

  def startPageState(
    theme: Theme,
    initialTerminalSize: TerminalSize
  ): AppState =
    val startPageSurfaceId = SurfaceId("surface-0")
    AppState.empty.copy(
      focus = Focus.Surface(startPageSurfaceId),
      uiSurfaces = List(
        UiSurface(
          id = startPageSurfaceId,
          content = SurfaceContent.StartPage(defaultStartPage),
          presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      terminalSize = Some(initialTerminalSize),
      theme = theme,
      nextSurfaceId = 1
    )

  /** Initialize the application state for first render using the active theme and current terminal size. */
  def initializeState(
    stateManager: StateManager,
    theme: Theme,
    initialTerminalSize: TerminalSize
  ): IO[AppState] =
    for
      _     <- stateManager.updateState(_ => startPageState(theme, initialTerminalSize))
      state <- stateManager.getCurrentState
    yield state
