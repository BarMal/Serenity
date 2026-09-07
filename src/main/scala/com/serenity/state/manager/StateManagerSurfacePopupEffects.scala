package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.command.ThemeIntent
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.io.FileUtils
import com.serenity.state.models.*
import com.serenity.ui.theme.config.{AppThemeManager, ThemeConfigWriter}

/** Floating popup surfaces triggered by commands or effects: the theme picker/creator, theme switching, theme export,
  * and the file-search overlay.
  */
final private[manager] class StateManagerSurfacePopupEffects(
    stateRef: Ref[IO, AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    themeManager: AppThemeManager,
    themeNamesRef: Ref[IO, List[String]],
    fileDialog: Option[com.serenity.io.FileDialog],
    validateAndUpdateState: (AppState, AppState) => IO[Unit]
):

  private def updateState(update: AppState => AppState): IO[Unit] = stateRef.update(update)

  private[manager] def interpretThemeIntent(intent: ThemeIntent, state: AppState): IO[Unit] =
    intent match
      case ThemeIntent.ToggleTheme =>
        toggleThemeEffect(state)
      case ThemeIntent.ReloadTheme =>
        reloadThemeEffect(state)
      case ThemeIntent.OpenThemeChooser =>
        openThemePickerEffect(state)
      case ThemeIntent.OpenThemeCreator =>
        openThemeCreatorEffect(state)
      case ThemeIntent.ExportCurrentTheme =>
        exportCurrentThemeEffect(state)
      case ThemeIntent.ReloadThemes =>
        themeManager.listAvailableThemes
          .flatMap(themeNamesRef.set)
          .handleErrorWith(ex => logger.error(ex)("[THEMES] Failed to reload theme list"))

  private def toggleThemeEffect(state: AppState): IO[Unit] =
    val targetThemeName =
      state.persisted.theme.name match
        case "light"                                    => "dark"
        case "dark"                                     => "light"
        case "default-light"                            => "default-dark"
        case "default-dark"                             => "default-light"
        case name if name.toLowerCase.contains("light") => "default-dark"
        case _                                          => "default-light"

    applyThemeByName(targetThemeName)

  private def reloadThemeEffect(state: AppState): IO[Unit] =
    reloadThemeByName(state.persisted.theme.name)

  private[manager] def applyThemeByName(themeName: String): IO[Unit] =
    themeManager
      .loadTheme(themeName)
      .flatMap { newTheme =>
        updateState { state =>
          val transition =
            if state.persisted.theme == newTheme then None
            else
              state.persisted.config.scaledUiAnimation
                .map(config => ThemeTransition(state.persisted.theme, 0, config.steps))
          state.copy(
            persisted = state.persisted.copy(theme = newTheme),
            runtime = state.runtime.copy(themeTransition = transition)
          )
        }
      }
      .handleErrorWith(ex => logger.error(ex)(s"[THEME] Failed to switch theme to $themeName"))

  private[manager] def reloadThemeByName(themeName: String): IO[Unit] =
    themeManager
      .loadTheme(themeName)
      .flatMap(theme => updateState(s => s.copy(persisted = s.persisted.copy(theme = theme))))
      .handleErrorWith(ex => logger.error(ex)(s"[THEME] Failed to reload theme $themeName"))

  private[manager] def openThemePickerEffect(state: AppState): IO[Unit] =
    themeNamesRef.get.flatMap { themeNames =>
      if themeNames.isEmpty then IO.unit
      else
        val currentTheme             = state.persisted.theme.name
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
            persisted = stateWithId.persisted.copy(focus = Focus.Surface(surfaceId)),
            runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces :+ surface)
          ),
          state
        )
    }

  private[manager] def openThemeCreatorEffect(state: AppState): IO[Unit] =
    val creatorState             = com.serenity.ui.theme.config.ThemeCreatorState.fromTheme(state.persisted.theme)
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.ThemeCreator(creatorState),
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
    validateAndUpdateState(
      stateWithId
        .copy(runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces.filterNot {
          _.content match
            case SurfaceContent.ThemeCreator(_) => true
            case _                              => false
        } :+ surface))
        .pushFocus(Focus.Surface(surfaceId)),
      state
    )

  private[manager] def exportCurrentThemeEffect(state: AppState): IO[Unit] =
    val config            = ThemeConfigWriter.themeToConfig(state.persisted.theme)
    val suggestedFileName = s"${ThemeConfigWriter.fileNameFor(config.name)}.conf"
    fileDialog match
      case Some(dialog) =>
        FileUtils.getCurrentDirectory
          .flatMap(currentDirectory => dialog.chooseSaveFile(Some(currentDirectory), Some(suggestedFileName)))
          .flatMap {
            case Some(path) =>
              ThemeConfigWriter
                .write(config, path)
                .flatTap(_ => logger.info(s"[THEMES] Exported current theme '${config.name}' to $path"))
            case None =>
              IO.unit
          }
          .handleErrorWith(ex => logger.error(ex)(s"[THEMES] Failed to export current theme '${config.name}'"))
      case None =>
        IO.unit

  private[manager] def openFileSearchEffect(state: AppState): IO[Unit] =
    val (stateWithId, surfaceId) = state.allocateSurfaceId
    val surface = UiSurface(
      id = surfaceId,
      content = SurfaceContent.FileSearch(FileSearchState("", Nil, 0)),
      presentation = SurfacePresentation.Floating(state.activeCursorPosition, SurfacePlacement.BelowCursor)
    )
    validateAndUpdateState(
      stateWithId.copy(
        persisted = stateWithId.persisted.copy(focus = Focus.Surface(surfaceId)),
        runtime = stateWithId.runtime.copy(uiSurfaces = stateWithId.runtime.uiSurfaces :+ surface)
      ),
      state
    )
