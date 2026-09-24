package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.command.ThemeIntent
import com.serenity.io.FileUtils
import com.serenity.state.models.*
import com.serenity.state.reducers.{PopupSurfaceReducer, ReducerResult, ThemeStateReducer}
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

  /** Commits against the state as it is once the theme I/O has finished, not the snapshot the command started from. */
  private def commitCurrent(reduce: AppState => ReducerResult): IO[Unit] =
    stateRef.get.flatMap(state => validateAndUpdateState(reduce(state).state, state))

  private[manager] def interpretThemeIntent(intent: ThemeIntent, state: AppState): IO[Unit] =
    intent match
      case ThemeIntent.ToggleTheme =>
        toggleThemeEffect(state)
      case ThemeIntent.ApplyTheme(name) =>
        applyThemeByName(name)
      case ThemeIntent.ReloadTheme =>
        reloadThemeEffect(state)
      case ThemeIntent.OpenThemeChooser =>
        openThemePickerEffect(state)
      case ThemeIntent.OpenThemeCreator =>
        openThemeCreatorEffect(state)
      case ThemeIntent.ExportCurrentTheme =>
        exportCurrentThemeEffect(state)
      case ThemeIntent.ReloadThemes =>
        refreshThemeNames

  /** Re-list the themes on disk into both the picker's ref and `runtime.availableThemeNames` (the settings picker). */
  private[manager] def refreshThemeNames: IO[Unit] =
    themeManager.listAvailableThemes
      .flatMap(names => themeNamesRef.set(names) *> commitCurrent(ThemeStateReducer.withAvailableThemeNames(names, _)))
      .handleErrorWith(ex => logger.error(ex)("[THEMES] Failed to reload theme list"))

  private def toggleThemeEffect(state: AppState): IO[Unit] =
    applyThemeByName(ThemeStateReducer.toggleTarget(state))

  private def reloadThemeEffect(state: AppState): IO[Unit] =
    reloadThemeByName(state.persisted.theme.name)

  private[manager] def applyThemeByName(themeName: String): IO[Unit] =
    themeManager
      .loadTheme(themeName)
      .flatMap(newTheme => commitCurrent(ThemeStateReducer.applyTheme(newTheme, _)))
      .handleErrorWith(ex => logger.error(ex)(s"[THEME] Failed to switch theme to $themeName"))

  private[manager] def reloadThemeByName(themeName: String): IO[Unit] =
    themeManager
      .loadTheme(themeName)
      .flatMap(theme => commitCurrent(ThemeStateReducer.replaceTheme(theme, _)))
      .handleErrorWith(ex => logger.error(ex)(s"[THEME] Failed to reload theme $themeName"))

  private[manager] def openThemePickerEffect(state: AppState): IO[Unit] =
    themeNamesRef.get.flatMap { themeNames =>
      PopupSurfaceReducer
        .openThemePicker(themeNames, state)
        .traverse_(result => validateAndUpdateState(result.state, state))
    }

  private[manager] def openThemeCreatorEffect(state: AppState): IO[Unit] =
    validateAndUpdateState(PopupSurfaceReducer.openThemeCreator(state).state, state)

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
    validateAndUpdateState(PopupSurfaceReducer.openFileSearch(state).state, state)
