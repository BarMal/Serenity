package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.command.ThemeIntent
import com.serenity.io.FileUtils
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*
import com.serenity.state.reducers.{
  AppEffect,
  PopupSurfaceReducer,
  ReducerResult,
  SurfaceEffect,
  ThemeEffect,
  ThemeStateReducer
}
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.{AppThemeManager, ThemeConfig, ThemeConfigWriter}

/** Floating popup surfaces triggered by commands or effects: the theme picker/creator, theme switching, theme export,
  * and the file-search overlay.
  */
final private[manager] class StateManagerSurfacePopupEffects(
    stateRef: Ref[IO, AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    themeManager: AppThemeManager,
    themeNamesRef: Ref[IO, List[String]],
    fileDialog: Option[com.serenity.io.FileDialog],
    validateAndUpdateState: (AppState, AppState) => IO[Unit],
    lanes: EffectLanePort,
    interpretEffect: AppEffect => IO[Unit]
):

  private val ThemeLoadLane: Lane.Keyed = Lane.Keyed(LaneKey.Theme, LanePolicy.SwitchLatest)

  // Writes and listings share one FIFO lane: a listing queued behind a save sees the saved file, and a theme switch --
  // on the SwitchLatest lane, which is a separate lane despite the shared key -- can never cancel a write.
  private val ThemeFileLane: Lane.Keyed = Lane.Keyed(LaneKey.Theme, LanePolicy.Sequential)

  /** Commits against the live state, not the snapshot the command started from. */
  private def commitCurrent(reduce: AppState => ReducerResult): IO[Unit] =
    stateRef.get.flatMap(state => validateAndUpdateState(reduce(state).state, state))

  // The chooser and creator go through `SurfaceEffect`, which reads the live state: `state` predates the command-usage
  // record `interpretCommand` commits just before, and committing from it would drop that record (#1714).
  private[manager] def interpretThemeIntent(intent: ThemeIntent, state: AppState): IO[Unit] =
    intent match
      case ThemeIntent.ToggleTheme =>
        interpretEffect(AppEffect.Theme(ThemeEffect.SwitchTheme(ThemeStateReducer.toggleTarget(state))))
      case ThemeIntent.ApplyTheme(name) =>
        interpretEffect(AppEffect.Theme(ThemeEffect.SwitchTheme(name)))
      case ThemeIntent.ReloadTheme =>
        interpretEffect(AppEffect.Theme(ThemeEffect.ReloadTheme(state.persisted.theme.name)))
      case ThemeIntent.OpenThemeChooser =>
        interpretEffect(AppEffect.Surface(SurfaceEffect.OpenThemePicker))
      case ThemeIntent.OpenThemeCreator =>
        interpretEffect(AppEffect.Surface(SurfaceEffect.OpenThemeCreator))
      case ThemeIntent.ExportCurrentTheme =>
        interpretEffect(AppEffect.Theme(ThemeEffect.ExportCurrentTheme))
      case ThemeIntent.ReloadThemes =>
        interpretEffect(AppEffect.Theme(ThemeEffect.RefreshThemeNames))

  private[manager] def interpretThemeEffect(effect: ThemeEffect): IO[Unit] =
    effect match
      case ThemeEffect.SwitchTheme(themeName) =>
        requestTheme(themeName, EffectResult.ThemeLoaded(themeName, _), s"[THEME] Failed to switch theme to $themeName")
      case ThemeEffect.ReloadTheme(themeName) =>
        requestTheme(themeName, EffectResult.ThemeReloaded(themeName, _), s"[THEME] Failed to reload theme $themeName")
      case ThemeEffect.SaveThemeConfig(config) =>
        lanes.submitEffect(ThemeFileLane, saveUserTheme(config))
      case ThemeEffect.RefreshThemeNames =>
        lanes.submitEffect(ThemeFileLane, listThemeNames)
      case ThemeEffect.ExportCurrentTheme =>
        stateRef.get.flatMap(exportCurrentThemeEffect)

  /** Records `themeName` as the latest request before loading it, so a slower load of an earlier request is dropped. */
  private def requestTheme(themeName: String, loaded: Theme => EffectResult, failure: => String): IO[Unit] =
    commitCurrent(ThemeStateReducer.withRequestedTheme(themeName, _)) >>
      lanes.submitEffect(
        ThemeLoadLane,
        themeManager
          .loadTheme(themeName)
          .flatMap(theme => lanes.dispatchEffectResult(loaded(theme), _ => IO.unit))
          .handleErrorWith(ex => logger.error(ex)(failure))
      )

  private def saveUserTheme(config: ThemeConfig): IO[Unit] =
    themeManager
      .writeUserTheme(config)
      .flatTap(path => logger.info(s"[THEMES] Saved user theme '${config.name}' to $path"))
      .flatMap(_ => listThemeNames)
      .handleErrorWith(ex => logger.error(ex)(s"[THEMES] Failed to save user theme '${config.name}'"))

  /** Re-list the themes on disk into both the picker's ref and `runtime.availableThemeNames` (the settings picker). */
  private def listThemeNames: IO[Unit] =
    themeManager.listAvailableThemes
      .flatMap(names =>
        themeNamesRef.set(names) >> lanes.dispatchEffectResult(EffectResult.ThemeNamesListed(names), _ => IO.unit)
      )
      .handleErrorWith(ex => logger.error(ex)("[THEMES] Failed to reload theme list"))

  private[manager] def openThemePickerEffect(state: AppState): IO[Unit] =
    themeNamesRef.get.flatMap { themeNames =>
      PopupSurfaceReducer
        .openThemePicker(themeNames, state)
        .traverse_(result => validateAndUpdateState(result.state, state))
    }

  private[manager] def openThemeCreatorEffect(state: AppState): IO[Unit] =
    validateAndUpdateState(PopupSurfaceReducer.openThemeCreator(state).state, state)

  private def exportCurrentThemeEffect(state: AppState): IO[Unit] =
    val config            = ThemeConfigWriter.themeToConfig(state.persisted.theme)
    val suggestedFileName = s"${ThemeConfigWriter.fileNameFor(config.name)}.conf"
    fileDialog match
      case Some(dialog) =>
        lanes.submitEffect(
          ThemeFileLane,
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
        )
      case None =>
        IO.unit

  private[manager] def openFileSearchEffect(state: AppState): IO[Unit] =
    validateAndUpdateState(PopupSurfaceReducer.openFileSearch(state).state, state)
