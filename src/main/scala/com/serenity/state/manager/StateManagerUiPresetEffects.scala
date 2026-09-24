package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.command.UiPresetsIntent
import com.serenity.config.{AppConfig, MarkdownViewMode}
import com.serenity.session.{SessionPersistence, SessionSaveTrigger}
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.presets.{UiPreset, UiPresetDiff}
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.AppThemeManager

/** Saves, applies, and manages named UI presets (workspace layout + theme + config snapshots).
  *
  * Every preset-store read and write runs FIFO on the Presets lane, off the dispatcher (#1697); what it means for the
  * state comes back as an [[EffectResult]] and is committed through the validated path.
  */
final private[manager] class StateManagerUiPresetEffects(
    currentState: IO[AppState],
    logger: org.typelevel.log4cats.Logger[IO],
    uiPresetStore: com.serenity.ui.presets.UiPresetStore,
    windowSizeProvider: IO[Option[com.serenity.config.PreferredWindowSize]],
    themeManager: AppThemeManager,
    onFontConfigChanged: com.serenity.ui.fonts.FontLoader.FontConfig => IO[Unit],
    sessionPersistence: SessionPersistence,
    persistConfigFile: AppConfig => IO[Unit],
    withUpdatedRunnerConfig: (AppState, AppConfig) => AppState,
    openMarkdownPreview: IO[Unit],
    loadPinnedDirectoryEffect: (com.serenity.ui.layout.PanelPosition, java.nio.file.Path) => IO[Unit],
    commitValidated: (AppState => AppState) => IO[Unit],
    lanes: EffectLanePort
)(using balance: com.serenity.rope.Balance):

  private[manager] def interpret(intent: UiPresetsIntent): IO[Unit] =
    intent match
      case UiPresetsIntent.SaveUiPresetAsNew(name) =>
        saveUiPresetAsNewEffect(name)
      case UiPresetsIntent.OverwriteUiPreset(name) =>
        overwriteUiPresetEffect(name)
      case UiPresetsIntent.ApplyUiPreset(name) =>
        applyUiPresetEffect(name)
      case UiPresetsIntent.ReviewUiPreset(name) =>
        reviewUiPresetEffect(name)
      case UiPresetsIntent.ConfirmUiPresetDiffApply(name, selectedKeys) =>
        confirmUiPresetDiffApplyEffect(name, selectedKeys.toSet)
      case UiPresetsIntent.DuplicateUiPreset(sourceName, targetName) =>
        duplicateUiPresetEffect(sourceName, targetName)
      case UiPresetsIntent.RenameUiPreset(sourceName, targetName) =>
        renameUiPresetEffect(sourceName, targetName)
      case UiPresetsIntent.DeleteUiPreset(name) =>
        deleteUiPresetEffect(name)
      case UiPresetsIntent.ResetUiPreset(name) =>
        resetUiPresetEffect(name)

  /** Captures the workspace as it is when the command runs as a new custom preset, rejecting existing names. */
  private def saveUiPresetAsNewEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")
      case Some(presetName) =>
        currentState.flatMap { snapshot =>
          onPresetsLane(s"save UI preset $presetName") {
            capturedPreset(presetName, snapshot).flatMap { preset =>
              uiPresetStore.create(preset).attempt.flatMap {
                case Left(error) =>
                  reportPresetFailure(presetName, s"Could not save $presetName", error)
                case Right(_) =>
                  reportWithPreviews(
                    UiPresetContext.CreatedPresetFocused(presetName, s"Preset saved. Configure $presetName.")
                  )
              }
            }
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
        currentState.flatMap { snapshot =>
          onPresetsLane(s"overwrite UI preset $presetName") {
            uiPresetStore.find(presetName).flatMap {
              case None =>
                report(
                  UiPresetContext.Status(
                    Some(presetName),
                    s"Custom preset '$presetName' was not found. Use Save As New Preset."
                  )
                )
              case Some(existing) =>
                capturedPreset(existing.name, snapshot).flatMap { preset =>
                  uiPresetStore.upsert(preset).attempt.flatMap {
                    case Left(error) =>
                      reportPresetFailure(existing.name, s"Could not save ${existing.name}", error)
                    case Right(_) =>
                      reportWithPreviews(
                        UiPresetContext.Status(Some(existing.name), s"Preset overwritten. Configure ${existing.name}.")
                      )
                  }
                }
            }
          }
        }

  private def capturedPreset(presetName: String, snapshot: AppState): IO[UiPreset] =
    windowSizeProvider
      .handleErrorWith(error => logger.error(error)("[PRESET] Window size capture failed").as(None))
      .map(UiPreset.capture(presetName, snapshot, _))

  private def reportPresetFailure(presetName: String, summary: String, error: Throwable): IO[Unit] =
    logger.error(error)(s"[PRESET] $summary") >>
      report(
        UiPresetContext.Status(
          Some(presetName),
          s"$summary: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}"
        )
      )

  /** A custom preset shadows a built-in of the same name; the flag says whether the built-in is what was found. */
  private def resolveUiPreset(presetName: String): IO[Option[(UiPreset, Boolean)]] =
    uiPresetStore.find(presetName).map(_.map(_ -> false).orElse(UiPreset.builtIn(presetName).map(_ -> true)))

  /** The one-shot "just apply it" path -- the splash's workflow shortcuts and the top-level searchable "Apply <Name>
    * Preset" commands both use this, applying every setting immediately via the real, unconditional
    * `UiPreset.applyBuiltInWorkflowToState`/`applyToState` rather than `UiPresetDiff.applySelected` with every known
    * key selected: `UiPresetDiff`'s selective merge only knows the fields it explicitly enumerates
    * (`ConfigRegistry.fields` plus its five composite groups), so "select everything it knows about" is not actually
    * equivalent to a true full replace -- `preferredWindowSize`, uncovered by either, is the concrete field that
    * exposed this. [[reviewUiPresetEffect]]/[[confirmUiPresetDiffApplyEffect]] are the deliberate, genuinely-partial
    * alternative, where that limitation is inherent to "apply only some of the changes" anyway.
    *
    * *Which* source the preset came from decides `applyBuiltInWorkflowToState` vs. `applyToState` -- not
    * `UiPresetDiff`'s own name-only check, which does not have "was this shadowed" to go on.
    */
  private def applyUiPresetEffect(name: String): IO[Unit] =
    requestApply(name)((preset, isBuiltInWorkflow, theme) =>
      base =>
        if isBuiltInWorkflow then UiPreset.applyBuiltInWorkflowToState(preset, base, theme)
        else UiPreset.applyToState(preset, base, theme)
    )

  /** Opens the preset diff-toggle review rather than applying immediately -- [[confirmUiPresetDiffApplyEffect]] does
    * the actual apply once the user confirms (default: every change selected).
    */
  private def reviewUiPresetEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")
      case Some(presetName) =>
        onPresetsLane(s"review UI preset $presetName") {
          resolveUiPreset(presetName).flatMap {
            case None =>
              logger.warn(s"[PRESET] UI preset not found: $presetName")
            case Some((preset, _)) =>
              lanes.dispatchEffectResult(EffectResult.UiPresetReviewReady(preset), _ => IO.unit)
          }
        }

  /** Re-resolves and re-validates the preset rather than trusting what the review opened with -- the store or the theme
    * could plausibly have changed in the time the review sat open.
    */
  private def confirmUiPresetDiffApplyEffect(name: String, selectedKeys: Set[String]): IO[Unit] =
    requestApply(name)((preset, _, theme) => base => UiPresetDiff.applySelected(base, theme, preset, selectedKeys))

  /** Records the request on the dispatcher, then loads the preset and its theme on the Presets lane. The loaded preset
    * is applied only if no later apply was requested meanwhile.
    */
  private def requestApply(name: String)(restoreWith: (UiPreset, Boolean, Theme) => AppState => AppState): IO[Unit] =
    normalizedPresetName(name) match
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")
      case Some(presetName) =>
        commitValidated(UiPresetTransitions.requestApply(_)._1) >>
          currentState
            .map(_.runtime.pendingUiPresetApply)
            .flatMap(_.traverse_ { request =>
              lanes.submitEffect(
                PersistenceLanes.Presets,
                loadPresetResolution(presetName, restoreWith)
                  .handleErrorWith(error =>
                    logger
                      .error(error)(s"[PRESET] Failed to apply UI preset $presetName")
                      .as(UiPresetApplyResolution.Abandon)
                  )
                  .flatMap(resolution =>
                    lanes.dispatchEffectResult(
                      EffectResult.UiPresetApplyResolved(request, resolution),
                      committed =>
                        resolution match
                          case UiPresetApplyResolution.Apply(preset, _) => afterPresetApplied(preset, committed)
                          case _                                        => IO.unit
                    )
                  )
              )
            })

  private def loadPresetResolution(
    presetName: String,
    restoreWith: (UiPreset, Boolean, Theme) => AppState => AppState
  ): IO[UiPresetApplyResolution] =
    resolveUiPreset(presetName).flatMap {
      case None =>
        logger.warn(s"[PRESET] UI preset not found: $presetName").as(UiPresetApplyResolution.Abandon)
      case Some((preset, isBuiltInWorkflow)) =>
        loadUiPresetResources(preset).flatMap {
          case Left(reason) =>
            logger
              .warn(s"[PRESET] Cannot preview UI preset '$presetName': $reason")
              .as(UiPresetApplyResolution.Reject(presetName, s"Cannot preview $presetName: $reason"))
          case Right(theme) =>
            IO.pure(
              UiPresetApplyResolution.Apply(
                preset,
                UiPresetTransitions.restored(
                  preset,
                  restoreWith(preset, isBuiltInWorkflow, theme),
                  withUpdatedRunnerConfig
                )
              )
            )
        }
    }

  /** Runs on the dispatcher with the committed state, so a preset that validation rejected (#1183) never gets here. */
  private def afterPresetApplied(preset: UiPreset, committed: AppState): IO[Unit] =
    val appliedConfig = committed.persisted.config
    persistConfigFile(appliedConfig) >>
      onFontConfigChanged(appliedConfig.editorConfig.fontConfig)
        .handleErrorWith(error => logger.error(error)("[PRESET] Failed to apply preset font config")) >>
      reloadPresetDirectories(preset) >>
      openPresetMarkdownPreviewIfNeeded(preset) >>
      lanes.submitEffect(
        PersistenceLanes.Config,
        currentState
          .flatMap(state => sessionPersistence.maybeSaveSession(state, SessionSaveTrigger.Manual))
          .handleErrorWith(error => logger.error(error)("[SESSION] Auto-save after preset apply failed"))
      )

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
        onPresetsLane(s"duplicate UI preset $source") {
          uiPresetStore.find(source).map(_.orElse(UiPreset.builtIn(source))).flatMap {
            case Some(preset) =>
              uiPresetStore.create(preset.copy(name = target)).attempt.flatMap {
                case Left(error) =>
                  reportPresetFailure(target, s"Could not duplicate $source", error)
                case Right(_) =>
                  reportWithPreviews(UiPresetContext.Status(Some(target), s"Preset duplicated. Configure $target."))
              }
            case None =>
              logger.warn(s"[PRESET] UI preset not found: $source")
          }
        }
      case _ =>
        logger.warn("[PRESET] Ignoring duplicate request with empty UI preset name")

  private def renameUiPresetEffect(sourceName: String, targetName: String): IO[Unit] =
    (normalizedPresetName(sourceName), normalizedPresetName(targetName)) match
      case (Some(source), _) if UiPreset.builtIn(source).nonEmpty =>
        updateCommandRunnerPresetContext(Some(source), s"Built-in preset cannot be renamed. Duplicate $source first.")
      case (Some(source), Some(target)) =>
        onPresetsLane(s"rename UI preset $source") {
          uiPresetStore.rename(source, target) >>
            reportWithPreviews(UiPresetContext.Status(Some(target), s"Preset renamed. Configure $target."))
        }
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
        onPresetsLane(s"delete UI preset $presetName") {
          uiPresetStore.delete(presetName) >> reportWithPreviews(UiPresetContext.Status(None, "Preset deleted."))
        }
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")

  private def resetUiPresetEffect(name: String): IO[Unit] =
    normalizedPresetName(name) match
      case Some(presetName) =>
        UiPreset.builtIn(presetName) match
          case Some(_) =>
            onPresetsLane(s"reset UI preset $presetName") {
              uiPresetStore.delete(presetName) >>
                reportWithPreviews(UiPresetContext.Status(Some(presetName), s"Preset reset. Configure $presetName."))
            }
          case None =>
            logger.warn(s"[PRESET] Built-in UI preset not found: $presetName")
      case None =>
        logger.warn("[PRESET] Ignoring empty UI preset name")

  private def normalizedPresetName(name: String): Option[String] =
    Option(UiPreset.normalizedName(name)).filter(_.nonEmpty)

  /** Queues `job` on the Presets lane; a failure is logged against `operation` the way the inline code used to. */
  private def onPresetsLane(operation: String)(job: IO[Unit]): IO[Unit] =
    lanes.submitEffect(
      PersistenceLanes.Presets,
      job.handleErrorWith(error => logger.error(error)(s"[PRESET] Failed to $operation"))
    )

  private def report(context: UiPresetContext): IO[Unit] =
    lanes.dispatchEffectResult(EffectResult.UiPresetFeedback(None, context), _ => IO.unit)

  private def reportWithPreviews(context: UiPresetContext): IO[Unit] =
    uiPresetStore
      .list()
      .map(_.map(UiPreset.Preview.fromPreset))
      .handleErrorWith(error => logger.error(error)("[PRESET] Failed to list UI presets").as(Nil))
      .flatMap(previews =>
        lanes.dispatchEffectResult(EffectResult.UiPresetFeedback(Some(previews), context), _ => IO.unit)
      )

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

  private def updateCommandRunnerPresetContext(presetName: Option[String], statusMessage: String): IO[Unit] =
    commitValidated(UiPresetTransitions.withPresetContext(_, presetName, statusMessage))
