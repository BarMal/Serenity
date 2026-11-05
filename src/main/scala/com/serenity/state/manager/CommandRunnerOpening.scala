package com.serenity.state.manager

import cats.effect.IO
import com.serenity.command.CommandRunner
import com.serenity.project.ProjectPresence
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.{
  AppState,
  ListPickerSearch,
  Modal,
  PickerSource,
  SurfaceContent,
  SurfaceId,
  replacedWhere
}
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import org.typelevel.log4cats.Logger

/** What opening the command palette needs from disk, read off the dispatcher (#1911). The palette opens at once with no
  * presets listed and its project presence `Unchecked`; each answer comes back as a result for that palette and applies
  * only while it is still open.
  */
private[manager] object CommandRunnerOpening:

  val PresetLane: Lane.Keyed = PersistenceLanes.Presets
  val ProbeLane: Lane.Keyed  = Lane.Keyed(LaneKey.ProjectProbe, LanePolicy.SwitchLatest)

  /** The palette `after` has open that `before` did not. */
  def openedBy(before: AppState, after: AppState): Option[SurfaceId] =
    activePalette(after).filterNot(activePalette(before).contains)

  def withPresetsListed(state: AppState, surfaceId: SurfaceId, previews: List[UiPreset.Preview]): AppState =
    withRunnerOn(state, surfaceId)(_.withUiPresetPreviews(previews)).getOrElse(state)

  def withPresenceDetected(state: AppState, surfaceId: SurfaceId, presence: ProjectPresence): AppState =
    withRunnerOn(state, surfaceId)(_.withProjectPresence(presence))
      .map(updated => updated.copy(runtime = updated.runtime.copy(projectPresence = presence)))
      .getOrElse(state)

  /** The open settings view or command picker -- each searches with a runner the reads fill in. */
  private def activePalette(state: AppState): Option[SurfaceId] =
    state.commandPickerSurface
      .map(_.id)
      .orElse(
        state.commandRunnerSurface.flatMap(surface =>
          surface.content match
            case SurfaceContent.CommandPalette(runner) if runner.isActive => Some(surface.id)
            case _                                                        => None
        )
      )

  /** Updates the runner on `surfaceId`: the settings view's own, or the one a command picker searches with, whose
    * choices are then recomputed for its current query with the highlight kept on the same row where it still exists.
    */
  private def withRunnerOn(state: AppState, surfaceId: SurfaceId)(
    update: CommandRunner => CommandRunner
  ): Option[AppState] =
    state.runtime.uiSurfaces
      .find(_.id == surfaceId)
      .flatMap(surface =>
        surface.content match
          case SurfaceContent.CommandPalette(runner) if runner.isActive =>
            Some(replacedContent(state, surfaceId, SurfaceContent.CommandPalette(update(runner))))
          case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) =>
            picker.source.collect {
              case PickerSource.Commands(runner, registry) =>
                val refreshed = ListPickerSearch.refreshed(
                  picker.copy(source = Some(PickerSource.Commands(update(runner), registry))),
                  state
                )
                val highlighted = picker.selectedChoice
                  .flatMap(choice => refreshed.items.toOption.map(_.items.indexWhere(_.label == choice.label)))
                  .filter(_ >= 0)
                val kept = highlighted.fold(refreshed)(index =>
                  refreshed.copy(items =
                    refreshed.items.map(_.select(index, com.serenity.ui.layout.ListPickerComposition.VisibleRows))
                  )
                )
                replacedContent(state, surfaceId, SurfaceContent.ModalWorkflow(Modal.ListPicker(kept)))
            }
          case _ => None
      )

  private def replacedContent(state: AppState, surfaceId: SurfaceId, content: SurfaceContent): AppState =
    state.copy(runtime =
      state.runtime.copy(uiSurfaces =
        state.runtime.uiSurfaces.replacedWhere(_.id == surfaceId)(_.copy(content = content))
      )
    )

/** Starts the reads for a palette that has just opened, each on its own lane. */
final private[manager] class CommandRunnerOpeningLoads(
    uiPresetStore: UiPresetStore,
    detectPresence: AppState => IO[ProjectPresence],
    lanes: EffectLanePort,
    logger: Logger[IO]
):
  import CommandRunnerOpening.{PresetLane, ProbeLane}

  def request(surfaceId: SurfaceId, state: AppState): IO[Unit] =
    lanes.submitEffect(PresetLane, listPresets(surfaceId)) >>
      lanes.submitEffect(ProbeLane, probeProject(surfaceId, state))

  private def listPresets(surfaceId: SurfaceId): IO[Unit] =
    uiPresetStore.list().map(_.map(UiPreset.Preview.fromPreset)).attempt.flatMap {
      case Right(previews) =>
        lanes.dispatchEffectResult(EffectResult.CommandRunnerPresetsListed(surfaceId, previews), _ => IO.unit)
      case Left(error) => logger.error(error)("[PRESET] Failed to list UI presets")
    }

  private def probeProject(surfaceId: SurfaceId, state: AppState): IO[Unit] =
    detectPresence(state).flatMap(presence =>
      lanes.dispatchEffectResult(EffectResult.ProjectPresenceDetected(surfaceId, presence), _ => IO.unit)
    )
