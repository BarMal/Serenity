package com.serenity.state.manager

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.*

/** The effect behind [[com.serenity.state.reducers.WorkflowEffect.RefreshPicker]]: a picker's match run on its own
  * lane, so the keystroke that asked for it has already been shown.
  */
private[manager] object PickerSearchEffect:

  val PickerSearchLane: Lane.Keyed = Lane.Keyed(LaneKey.PickerSearch, LanePolicy.SwitchLatest)

  /** A newer request supersedes one still matching, and a result that finds the picker's query changed is dropped. */
  def refresh(
    lanes: EffectLanePort,
    currentState: IO[AppState],
    surfaceId: SurfaceId,
    query: String,
    search: (AppState, SurfaceId, String) => IO[Option[ListPicker]] = delayedSearch
  ): IO[Unit] =
    lanes.submitEffect(
      PickerSearchLane,
      currentState.flatMap(state =>
        search(state, surfaceId, query).flatMap(
          _.traverse_(searched =>
            lanes.dispatchEffectResult(EffectResult.PickerSearched(surfaceId, query, searched), _ => IO.unit)
          )
        )
      )
    )

  private def delayedSearch(state: AppState, surfaceId: SurfaceId, query: String): IO[Option[ListPicker]] =
    IO.delay(PickerSearchTransitions.searched(state, surfaceId, query))
