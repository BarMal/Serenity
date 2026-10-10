package com.serenity.state.manager

import scala.concurrent.duration.*

import com.serenity.state.effects.{Lane, LaneKey, LanePolicy}
import com.serenity.state.models.BufferId

/** A lane's work that waits out `delay` after the latest request before it runs, or `maxWait` after the first. */
final private[manager] case class DebouncedEffect(
    lane: Lane.Keyed,
    delay: FiniteDuration,
    maxWait: Option[FiniteDuration] = None
)

private[manager] object DebouncedEffects:

  // Typing faster than the debounce must not postpone spell check for as long as it lasts (#1929).
  val DocumentAnalysis: DebouncedEffect = DebouncedEffect(switchLatest(LaneKey.Analysis), 150.millis, Some(1.second))
  val FindSearch: DebouncedEffect       = DebouncedEffect(switchLatest(LaneKey.Search), 50.millis)
  val OutlineRefresh: DebouncedEffect   = DebouncedEffect(switchLatest(LaneKey.OutlineRefresh), 150.millis)

  def markdownPreviewCommit(bufferId: BufferId): DebouncedEffect =
    DebouncedEffect(switchLatest(LaneKey.MarkdownPreview(bufferId)), 150.millis)

  def editIdleSessionSave(idle: FiniteDuration): DebouncedEffect =
    DebouncedEffect(switchLatest(LaneKey.EditIdleSessionSave), idle)

  def autoSave(bufferId: BufferId, delay: FiniteDuration): DebouncedEffect =
    DebouncedEffect(switchLatest(LaneKey.AutoSaveDelay(bufferId)), delay)

  private def switchLatest(key: LaneKey): Lane.Keyed = Lane.Keyed(key, LanePolicy.SwitchLatest)
