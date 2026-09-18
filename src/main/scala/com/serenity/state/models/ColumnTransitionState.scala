package com.serenity.state.models

import com.serenity.animation.Interpolator.given
import com.serenity.animation.{EasingCurve, TransitionDirection, Tween}

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
  *
  * Built on `Tween[Double]` (issue #1083) rather than the retired `ScalarTimeline`, sweeping a plain `0.0 -> 1.0`
  * progress value: `retarget` is what `ScalarTimeline` never supported, and is what
  * `CursorViewport.seedColumnTransition` now uses instead of always reseeding at progress 0 when the cursor crosses
  * another column boundary before the current sweep finishes -- fixing that jump-cut as a consequence of switching
  * primitives, not a separate change.
  */
final case class ColumnTransitionState(
    tween: Tween[Double],
    direction: TransitionDirection,
    previousTopLine: Int,
    previousTopVisualLine: Int
):

  def progress: Double = tween.currentValue

  def advance: ColumnTransitionState = copy(tween = tween.advance)

  def isComplete: Boolean = tween.isComplete

  /** Continues the sweep smoothly toward full progress from wherever it currently is, keeping this state's own
    * `direction`/`previousTop*` -- the outgoing column being swept away doesn't change just because a second move
    * landed before the first sweep finished, only how much further there is left to sweep.
    */
  def retarget: ColumnTransitionState = copy(tween = tween.retarget(1.0))

object ColumnTransitionState:

  def seeded(
    steps: Int,
    curve: EasingCurve,
    direction: TransitionDirection,
    previousTopLine: Int,
    previousTopVisualLine: Int
  ): ColumnTransitionState =
    ColumnTransitionState(
      tween = Tween(start = 0.0, end = 1.0, curve = curve, steps = steps),
      direction = direction,
      previousTopLine = previousTopLine,
      previousTopVisualLine = previousTopVisualLine
    )
