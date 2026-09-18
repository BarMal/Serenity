package com.serenity.state.models

import com.serenity.animation.{ScalarTimeline, TransitionDirection}

/** Column-based document layout (issue #1338, Phase 1 animation): mid-flight state for the transition painted between
  * one column's content and the next, keyed by the buffer whose active column just moved (`Runtime.columnTransitions`).
  *
  * Seeded by `CursorViewport.ensureVisibleCursors` whenever `ColumnLeft`/`ColumnRight` changes which column is showing,
  * and advanced once per render tick by `StateManagerEditorCapability.advanceAnimationsOnTick` -- mirroring
  * `Runtime.themeTransition`'s tick-driven advance -- until `isComplete`, at which point it is dropped from the map.
  *
  * `previousTopLine`/`previousTopVisualLine` are the OLD column's own viewport anchor (before the move), captured so
  * the renderer can rebuild that column's `TextLayoutSnapshot` for as long as it is still fading/sweeping out --
  * `direction` says which way the sweep reads (see `RendererColumnTransition`'s doc comment for the chosen mapping).
  */
final case class ColumnTransitionState(
    timeline: ScalarTimeline,
    direction: TransitionDirection,
    previousTopLine: Int,
    previousTopVisualLine: Int
):

  def progress: Double = timeline.progress

  def advance: ColumnTransitionState = copy(timeline = timeline.advance)

  def isComplete: Boolean = timeline.isComplete
