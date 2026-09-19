package com.serenity.state.models

import com.serenity.animation.Interpolator.given
import com.serenity.animation.{EasingCurve, Tween}
import com.serenity.ui.layout.LayoutRect

/** A visual line's identity for selection-highlight animation purposes: its own `bufferLine`/`startColumn`, the same
  * pair `TextVisualLine` itself carries. Keying by identity rather than list position means a line never needs to be
  * re-indexed as neighbouring lines are added or removed -- `Selection`'s anchor-stability (`DirectedRange`) already
  * guarantees a line's own identity is stable across every transition this animates (resize, append, truncate, create,
  * clear), so there is nothing "index from the anchor" bookkeeping would buy that keying by identity doesn't already
  * give for free.
  */
final case class SelectionLineKey(bufferLine: Int, startColumn: Int)

/** One visual line's mid-flight highlight extent. `removing` marks a line that is shrinking to a zero-width sliver on
  * its way out (a truncated line, or every line of a selection being cleared entirely) -- [[SelectionGeometryState
  * .advance]] drops it once its tween completes, rather than leaving a zero-width entry to paint nothing forever.
  */
final case class SelectionLineGeometry(key: SelectionLineKey, tween: Tween[LayoutRect], removing: Boolean = false)

/** Selection grow/settle (issue #1085 phase 3): mid-flight geometry for one cursor's selection highlight, one
  * `Tween[LayoutRect]` per visual line the highlight touches. Each `LayoutRect` is a *column* extent local to its own
  * visual line -- `x` the buffer column the highlight starts at, `width` its column span, `y`/`height` fixed at `0`/`1`
  * and unused -- not a screen-pixel rect the way `PanelGeometryState`'s is; a selection highlight's real screen position
  * still comes from the renderer's own per-visual-line pixel measurement (`RendererHighlights`), exactly as it does for
  * the live (non-animated) case. This is also why one shared model serves both the GUI's sub-pixel-measured painting
  * and TUI's whole-cell painting: both already resolve a `(startColumn, endColumn)` pair down to pixels/cells
  * themselves, so animating at column granularity here is a single model neither path needs its own version of.
  *
  * Composes with multi-cursor for free: each `Cursor.selectionGeometry` is independent state on that cursor, not a
  * shared `Runtime` map keyed by anything that would collide between cursors.
  */
final case class SelectionGeometryState(lines: List[SelectionLineGeometry]):

  def rectFor(bufferLine: Int, startColumn: Int): Option[LayoutRect] =
    lines.find(line => line.key.bufferLine == bufferLine && line.key.startColumn == startColumn).map(_.tween.currentValue)

  /** Advances every line's tween by one step, dropping a `removing` line the moment its shrink completes. A
    * non-removing line whose tween completes is kept -- its `currentValue` has already settled at its final rect,
    * which is exactly what the live (non-animated) selection would paint there anyway, so there is nothing wrong with
    * leaving it in place until the whole state is replaced or cleared.
    */
  def advance: SelectionGeometryState =
    SelectionGeometryState(
      lines.map(line => line.copy(tween = line.tween.advance)).filterNot(line => line.removing && line.tween.isComplete)
    )

  def isComplete: Boolean = lines.forall(_.tween.isComplete)

object SelectionGeometryState:

  /** Turns `before`/`after` per-line rects (each a map from a visual line's [[SelectionLineKey]] to its column extent,
    * computed by `state.manager.SelectionGeometry` against the buffer's own text layout) into the tween list this
    * state wraps -- the one place every transition anchor-stability permits is handled uniformly:
    *
    *   - a key only in `after` grows in from a zero-width sliver at its own start column (a newly appended line, or
    *     every line of a selection created fresh when `before` is empty)
    *   - a key only in `before` shrinks to a zero-width sliver at its own start column and is marked `removing` (a
    *     truncated line, or every line of a selection cleared entirely when `after` is empty)
    *   - a key in both, with an unchanged rect, needs no tween at all -- only the line whose extent actually moved
    *     (the one adjacent to the moving focus) is worth animating
    *   - a key in both, with a changed rect, tweens directly from the old rect to the new one
    *
    * `existing`'s own in-flight tweens are retargeted (`Tween.retarget`) rather than restarted from `before`, the same
    * jump-cut fix `CursorViewport.seedCursorGlide`/`seedColumnTransition` already apply to their own tweens, for
    * whichever keys still have a live (non-complete) tween in `existing` when this is called again before the
    * previous animation finished.
    *
    * Returns `None` when there is nothing to animate (`before == after`, both empty, or every difference between them
    * is already fully settled), which callers read as "paint the live selection extent directly, no geometry to
    * consult."
    */
  def diff(
    existing: Option[SelectionGeometryState],
    before: Map[SelectionLineKey, LayoutRect],
    after: Map[SelectionLineKey, LayoutRect],
    curve: EasingCurve,
    steps: Int
  ): Option[SelectionGeometryState] =
    val existingByKey  = existing.map(_.lines.map(line => line.key -> line).toMap).getOrElse(Map.empty)
    val allKeys         = (before.keySet ++ after.keySet ++ existingByKey.keySet).toList
      .sortBy(key => (key.bufferLine, key.startColumn))

    def sliverAt(rect: LayoutRect): LayoutRect = rect.copy(width = 0)
    def inFlightTween(key: SelectionLineKey): Option[Tween[LayoutRect]] =
      existingByKey.get(key).map(_.tween).filterNot(_.isComplete)
    def freshTween(start: LayoutRect, end: LayoutRect): Tween[LayoutRect] =
      Tween(start = start, end = end, curve = curve, steps = steps)

    val lines = allKeys.flatMap { key =>
      (before.get(key), after.get(key), inFlightTween(key)) match
        case (_, Some(target), Some(inFlight)) =>
          Some(SelectionLineGeometry(key, inFlight.retarget(target), removing = false))
        case (None, Some(target), None) =>
          Some(SelectionLineGeometry(key, freshTween(sliverAt(target), target), removing = false))
        case (Some(source), Some(target), None) if source == target =>
          None
        case (Some(source), Some(target), None) =>
          Some(SelectionLineGeometry(key, freshTween(source, target), removing = false))
        case (Some(source), None, Some(inFlight)) =>
          Some(SelectionLineGeometry(key, inFlight.retarget(sliverAt(source)), removing = true))
        case (Some(source), None, None) =>
          Some(SelectionLineGeometry(key, freshTween(source, sliverAt(source)), removing = true))
        case (None, None, _) =>
          None
    }

    Option.when(lines.nonEmpty)(SelectionGeometryState(lines))
