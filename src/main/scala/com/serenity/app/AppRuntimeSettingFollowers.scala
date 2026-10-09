package com.serenity.app

import cats.effect.IO
import com.serenity.state.models.AppState
import fs2.concurrent.SignallingRef

private[serenity] object AppRuntimeSettingFollowers:

  /** Publishes `ui.render.frame_timing` only when a commit changes it, so the report stream sleeps through ordinary
    * edits.
    */
  private[serenity] def followFrameTimingSetting(
    frameTimingEnabled: SignallingRef[IO, Boolean]
  )(before: AppState, after: AppState): IO[Unit] =
    val enabled = after.persisted.config.surfaceConfig.frameTimingEnabled
    IO.whenA(enabled != before.persisted.config.surfaceConfig.frameTimingEnabled)(frameTimingEnabled.set(enabled))

  /** Publishes `ui.render.latency_trace` only when a commit changes it. */
  private[serenity] def followLatencyTraceSetting(
    latencyTraceEnabled: SignallingRef[IO, Boolean]
  )(before: AppState, after: AppState): IO[Unit] =
    val enabled = after.persisted.config.surfaceConfig.latencyTraceEnabled
    IO.whenA(enabled != before.persisted.config.surfaceConfig.latencyTraceEnabled)(latencyTraceEnabled.set(enabled))
