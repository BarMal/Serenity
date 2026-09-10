package com.serenity.state.manager

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.command.{SettingsPage, SettingsSurfaceState, UiPresetsIntent}
import com.serenity.config.{AppConfig, DefaultDocumentMode, MarkdownViewMode}
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.RichTextDocument
import com.serenity.session.{SessionPersistence, SessionSaveTrigger}
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.AppThemeManager

/** Saves, applies, and manages named UI presets (workspace layout + theme + config snapshots). */
final private[manager] class StateManagerUiPresetEffects(
    stateRef: Ref[IO, AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    uiPresetStore: com.serenity.ui.presets.UiPresetStore,
    windowSizeProvider: IO[Option[com.serenity.config.PreferredWindowSize]],
    themeManager: AppThemeManager,
    onFontConfigChanged: com.serenity.ui.fonts.FontLoader.FontConfig => IO[Unit],
    sessionPersistence: SessionPersistence,
    persistConfigFile: AppConfig => IO[Unit],
    withUpdatedRunnerConfig: (AppState, AppConfig) => AppState,
    openMarkdownPreview: IO[Unit],
    loadPinnedDirectoryEffect: (com.serenity.ui.layout.PanelPosition, java.nio.file.Path) => IO[Unit]
):

  private[manager] def interpret(intent: UiPresetsIntent): IO[Unit] =
    intent match
      case UiPresetsIntent.SaveUiPresetAsNew(name) =>
        saveUiPresetAsNewEffect(name)
      case UiPresetsIntent.OverwriteUiPreset(name) =>
        overwriteUiPresetEffect(name)
      case UiPresetsIntent.ApplyUiPreset(name) =>
        applyUiPresetEffect(name)
      case UiPresetsIntent.DuplicateUiPreset(sourceName, targetName) =>
        duplicateUiPresetEffect(sourceName, targetName)
      case UiPresetsIntent.RenameUiPreset(sourceName, targetName) =>
        renameUiPresetEffect(sourceName, targetName)
      case UiPresetsIntent.DeleteUiPreset(name) =>
        deleteUiPresetEffect(name)
      case UiPresetsIntent.ResetUiPreset(name) =>
        resetUiPresetEffect(name)

  /** Captures the live workspace as a new custom preset, rejecting names that already exist. */
  private def saveUiPresetAsNewEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")
      case Some(presetName) =>
        capturedPreset(presetName).flatMap { preset =>
          uiPresetStore.create(preset).attempt.flatMap {
            case Left(error) =>
              reportPresetFailure(presetName, s"Could not save $presetName", error)
            case Right(_) =>
              refreshCommandRunnerUiPresetPreviews >>
                focusCreatedPresetOptions(presetName, s"Preset saved. Configure $presetName.")
          }
        }

  /** Overwrites an existing custom preset with the live workspace; the last write wins. */
  private def overwriteUiPresetEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")
      case Some(presetName) if UiPreset.builtIn(presetName).nonEmpty =>
        updateCommandRunnerPresetContext(
          Some(presetName),
          s"Built-in preset cannot be overwritten. Duplicate $presetName first."
        )
      case Some(presetName) =>
        uiPresetStore
          .find(presetName)
          .flatMap {
            case None =>
              updateCommandRunnerPresetContext(
                Some(presetName),
                s"Custom preset '$presetName' was not found. Use Save As New Preset."
              )
            case Some(existing) =>
              capturedPreset(existing.name).flatMap { preset =>
                uiPresetStore.upsert(preset).attempt.flatMap {
                  case Left(error) =>
                    reportPresetFailure(existing.name, s"Could not save ${existing.name}", error)
                  case Right(_) =>
                    refreshCommandRunnerUiPresetPreviews >>
                      updateCommandRunnerPresetContext(
                        Some(existing.name),
                        s"Preset overwritten. Configure ${existing.name}."
                      )
                }
              }
          }
          .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to overwrite UI preset $presetName"))

  private def capturedPreset(presetName: String): IO[UiPreset] =
    for
      state <- stateRef.get
      windowSize <- windowSizeProvider.handleErrorWith(error =>
        logger.error(error)("[PRESET] Window size capture failed").as(None)
      )
    yield UiPreset.capture(presetName, state, windowSize)

  private def reportPresetFailure(presetName: String, summary: String, error: Throwable): IO[Unit] =
    logger.error(error)(s"[PRESET] $summary") >>
      updateCommandRunnerPresetContext(
        Some(presetName),
        s"$summary: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}"
      )

  private def applyUiPresetEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")
      case Some(presetName) =>
        uiPresetStore
          .find(presetName)
          .map { customPreset =>
            customPreset
              .map(_ -> false)
              .orElse(UiPreset.builtIn(presetName).map(_ -> true))
          }
          .flatMap {
            case None =>
              logger.warn(s"[PRESET] UI preset not found: $presetName")
            case Some((preset, isBuiltInWorkflow)) =>
              loadUiPresetResources(preset).flatMap {
                case Left(reason) =>
                  rejectUiPresetPreview(presetName, reason)
                case Right(theme) =>
                  applyLoadedUiPreset(preset, isBuiltInWorkflow, theme)
              }
          }
          .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to apply UI preset $presetName"))

  private def applyLoadedUiPreset(preset: UiPreset, isBuiltInWorkflow: Boolean, theme: Theme): IO[Unit] =
    for
      appliedConfig <- stateRef.modify { state =>
        val restoredPresetState =
          if isBuiltInWorkflow then UiPreset.applyBuiltInWorkflowToState(preset, state, theme)
          else UiPreset.applyToState(preset, state, theme)
        val restoredDocumentState =
          applyPresetDocumentModeToActiveEmptyBuffer(restoredPresetState, preset.config.defaultDocumentMode)
        val restoredOutlineState = hydratePresetSymbolPanels(restoredDocumentState)
        val restored             = withUpdatedRunnerConfig(restoredOutlineState, restoredOutlineState.persisted.config)
        (restored, restored.persisted.config)
      }
      _ <- persistConfigFile(appliedConfig)
      _ <- onFontConfigChanged(appliedConfig.editorConfig.fontConfig)
        .handleErrorWith(error => logger.error(error)("[PRESET] Failed to apply preset font config"))
      _ <- reloadPresetDirectories(preset)
      _ <- openPresetMarkdownPreviewIfNeeded(preset)
      _ <- stateRef.get
        .flatMap(state => sessionPersistence.maybeSaveSession(state, SessionSaveTrigger.Manual))
        .handleErrorWith(error => logger.error(error)("[SESSION] Auto-save after preset apply failed"))
    yield ()

  private def applyPresetDocumentModeToActiveEmptyBuffer(state: AppState, mode: DefaultDocumentMode): AppState =
    state.focusedBufferId.flatMap(state.persisted.buffers.get) match
      case Some(buffer)
          if buffer.document.isNewEmpty && buffer.document.content.weight == 0 && buffer.document.filePath.isEmpty =>
        val updatedBuffer =
          mode match
            case DefaultDocumentMode.PlainText =>
              buffer.copy(
                document = buffer.document.copy(language = None),
                richText = buffer.richText.copy(richTextDocument = None)
              )
            case DefaultDocumentMode.Markdown =>
              buffer.copy(
                document = buffer.document.copy(language = Some(LanguageId.Markdown)),
                richText = buffer.richText.copy(richTextDocument = None)
              )
            case DefaultDocumentMode.RichText =>
              buffer.copy(
                document = buffer.document.copy(language = None),
                richText = buffer.richText.copy(richTextDocument = Some(RichTextDocument.fromPlainText("")))
              )
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> updatedBuffer)))
      case _ =>
        state

  private def hydratePresetSymbolPanels(state: AppState): AppState =
    val outlineSymbolsList = PanelSymbolLookup.outlineSymbols(state)
    val outlineActive      = PanelSymbolLookup.currentSymbolActiveLocation(outlineSymbolsList, state)
    val commentSymbolsList = PanelSymbolLookup.commentPanelSymbols(state)
    val commentActive      = PanelSymbolLookup.currentSymbolActiveLocation(commentSymbolsList, state)
    val hydratedSurfaces = state.runtime.uiSurfaces.map {
      case surface @ UiSurface(_, SurfaceContent.Outline(_, _), SurfacePresentation.Docked, _) =>
        surface.copy(content = SurfaceContent.Outline(outlineSymbolsList, outlineActive))
      case surface @ UiSurface(_, SurfaceContent.Comments(_, _), SurfacePresentation.Docked, _) =>
        surface.copy(content = SurfaceContent.Comments(commentSymbolsList, commentActive))
      case surface =>
        surface
    }
    state.copy(runtime = state.runtime.copy(uiSurfaces = hydratedSurfaces))

  private def openPresetMarkdownPreviewIfNeeded(preset: UiPreset): IO[Unit] =
    if preset.config.markdownViewMode == MarkdownViewMode.SplitPreview then openMarkdownPreview
    else IO.unit

  private def reloadPresetDirectories(preset: UiPreset): IO[Unit] =
    preset.pinnedPanels.traverse_ { panel =>
      panel.content match
        case com.serenity.ui.layout.SessionPanelContent.DirectoryTree(rootPath, _, expandedPaths) =>
          (rootPath :: expandedPaths).distinct.traverse_(path =>
            loadPinnedDirectoryEffect(panel.position, java.nio.file.Path.of(path))
          )
        case _ =>
          IO.unit
    }

  private def duplicateUiPresetEffect(sourceName: String, targetName: String): IO[Unit] =
    (normalizedPresetName(sourceName), normalizedPresetName(targetName)) match
      case (Some(source), Some(target)) =>
        uiPresetStore
          .find(source)
          .map(_.orElse(UiPreset.builtIn(source)))
          .flatMap {
            case Some(preset) =>
              uiPresetStore.create(preset.copy(name = target)).attempt.flatMap {
                case Left(error) =>
                  reportPresetFailure(target, s"Could not duplicate $source", error)
                case Right(_) =>
                  refreshCommandRunnerUiPresetPreviews >>
                    updateCommandRunnerPresetContext(Some(target), s"Preset duplicated. Configure $target.")
              }
            case None =>
              logger.warn(s"[PRESET] UI preset not found: $source")
          }
          .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to duplicate UI preset $source"))
      case _ =>
        logger.warn("[PRESET] Ignoring duplicate request with empty UI preset name")

  private def renameUiPresetEffect(sourceName: String, targetName: String): IO[Unit] =
    (normalizedPresetName(sourceName), normalizedPresetName(targetName)) match
      case (Some(source), _) if UiPreset.builtIn(source).nonEmpty =>
        updateCommandRunnerPresetContext(Some(source), s"Built-in preset cannot be renamed. Duplicate $source first.")
      case (Some(source), Some(target)) =>
        uiPresetStore
          .rename(source, target)
          .flatTap(_ =>
            refreshCommandRunnerUiPresetPreviews >>
              updateCommandRunnerPresetContext(Some(target), s"Preset renamed. Configure $target.")
          )
          .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to rename UI preset $source"))
      case _ =>
        logger.warn("[PRESET] Ignoring rename request with empty UI preset name")

  private def deleteUiPresetEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case Some(presetName) if UiPreset.builtIn(presetName).nonEmpty =>
        updateCommandRunnerPresetContext(
          Some(presetName),
          "Built-in preset cannot be deleted. Use Reset Preset to discard overrides."
        )
      case Some(presetName) =>
        uiPresetStore
          .delete(presetName)
          .flatTap(_ =>
            refreshCommandRunnerUiPresetPreviews >> updateCommandRunnerPresetContext(None, "Preset deleted.")
          )
          .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to delete UI preset $presetName"))
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")

  private def resetUiPresetEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case Some(presetName) =>
        UiPreset.builtIn(presetName) match
          case Some(_) =>
            uiPresetStore
              .delete(presetName)
              .flatTap(_ =>
                refreshCommandRunnerUiPresetPreviews >>
                  updateCommandRunnerPresetContext(Some(presetName), s"Preset reset. Configure $presetName.")
              )
              .handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to reset UI preset $presetName"))
          case None =>
            logger.warn(s"[PRESET] Built-in UI preset not found: $presetName")
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")

  private def normalizedPresetName(name: String): Option[String] =
    Option(UiPreset.normalizedName(name)).filter(_.nonEmpty)

  private def refreshCommandRunnerUiPresetPreviews: IO[Unit] =
    uiPresetStore
      .list()
      .map(_.map(UiPreset.Preview.fromPreset))
      .handleErrorWith(error => logger.error(error)("[PRESET] Failed to list UI presets").as(Nil))
      .flatMap(previews => stateRef.update(state => updateCommandRunnerUiPresetPreviews(state, previews)))

  private def updateCommandRunnerUiPresetPreviews(state: AppState, previews: List[UiPreset.Preview]): AppState =
    state.commandRunnerSurface match
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            val updatedRunner = runner.withUiPresetPreviews(previews)
            val updatedSurfaces = state.runtime.uiSurfaces.replacedWhere(_.id == surface.id)(
              _.copy(content = SurfaceContent.CommandPalette(updatedRunner))
            )
            state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))
          case _ =>
            state
      case None =>
        state

  private def loadUiPresetResources(preset: UiPreset): IO[Either[String, Theme]] =
    FontLoader.missingFamilies(preset.config.editorConfig.fontConfig) match
      case missing :: _ => IO.pure(Left(s"Preset requires unavailable $missing."))
      case Nil =>
        themeManager.loadTheme(preset.themeName).attempt.map {
          case Right(theme) => Right(theme)
          case Left(error) =>
            val detail = Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
            Left(s"Theme '${preset.themeName}' could not be loaded: $detail")
        }

  private def rejectUiPresetPreview(name: String, reason: String): IO[Unit] =
    logger.warn(s"[PRESET] Cannot preview UI preset '$name': $reason") >>
      updateCommandRunnerPresetContext(Some(name), s"Cannot preview $name: $reason")

  private def updateCommandRunnerPresetContext(presetName: Option[String], statusMessage: String): IO[Unit] =
    stateRef.update(updateCommandRunnerPresetContextInState(_, presetName, statusMessage))

  private def updateCommandRunnerPresetContextInState(
    state: AppState,
    presetName: Option[String],
    statusMessage: String
  ): AppState =
    state.commandRunnerSurface match
      case Some(surface) =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) =>
            val updatedRunner = runner.copy(
              editingPresetName = presetName,
              editingItemId = None,
              editingText = "",
              statusMessage = Some(statusMessage)
            )
            val updatedSurfaces = state.runtime.uiSurfaces.replacedWhere(_.id == surface.id)(
              _.copy(content = SurfaceContent.CommandPalette(updatedRunner))
            )
            state.copy(runtime = state.runtime.copy(uiSurfaces = updatedSurfaces))
          case _ =>
            state
      case None =>
        state

  /** Focuses the just-created preset's own editing group (issue #1059: renders on the one `CommandPalette` surface,
    * like every other settings drill-in, rather than spawning a second floating one).
    */
  private def focusCreatedPresetOptions(name: String, statusMessage: String): IO[Unit] =
    stateRef.update { state =>
      state.commandRunnerSurface match
        case Some(surface) =>
          surface.content match
            case SurfaceContent.CommandPalette(runner) =>
              val updatedRunner = runner
                .withDrilledSettingsSurface(
                  SettingsSurfaceState(
                    SettingsPage.Group("settings-preset-edit"),
                    List(SettingsPage.Group("settings-ui-presets", 2))
                  )
                )
                .copy(
                  submenuSelections = runner.submenuSelections + ("settings-ui-presets" -> 2),
                  editingItemId = None,
                  editingText = "",
                  editingPresetName = Some(name.trim),
                  statusMessage = Some(statusMessage)
                )
              val updatedSurfaces = state.runtime.uiSurfaces.replacedWhere(_.id == surface.id)(
                _.copy(content = SurfaceContent.CommandPalette(updatedRunner))
              )
              state.copy(
                persisted = state.persisted.copy(focus = Focus.Surface(surface.id)),
                runtime = state.runtime.copy(uiSurfaces = updatedSurfaces)
              )
            case _ =>
              state
        case None =>
          state
    }
