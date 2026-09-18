package com.serenity.state.models

import com.serenity.animation.Interpolator.given
import com.serenity.animation.{EasingCurve, Tween}
import com.serenity.ui.layout.LayoutRect

/** Panel scale-in/out (issue #1085 phase 1): mid-flight geometry for a pinned/docked panel opening or closing, keyed by
  * the `SurfaceId` it paints against (`Runtime.panelGeometry`) -- the real panel's own id on open, or the transient
  * ghost overlay's id `PinnedPanelAnimations.close` allocates on close.
  *
  * Wraps `Tween[LayoutRect]` (issue #1083) the same way `ColumnTransitionState` wraps `Tween[Double]`: the tween's own
  * `start`/`end` already carry the anchor-collapsed rect and the panel's full rect, so no extra fields are needed here.
  * Seeded and advanced independently of `Runtime.surfaceAnimations`' colour fade -- see `MotionFamily.PanelGeometry`'s
  * doc comment for why the two are separate.
  */
final case class PanelGeometryState(tween: Tween[LayoutRect]):

  def currentRect: LayoutRect = tween.currentValue

  def advance: PanelGeometryState = copy(tween = tween.advance)

  def isComplete: Boolean = tween.isComplete

object PanelGeometryState:

  def seeded(start: LayoutRect, end: LayoutRect, curve: EasingCurve, steps: Int): PanelGeometryState =
    PanelGeometryState(Tween(start = start, end = end, curve = curve, steps = steps))
