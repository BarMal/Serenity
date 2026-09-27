package com.serenity.state.models

/** Surface/panel/column motion state one grab-bag category of `Runtime` used to hold directly (issue #1693): the
  * per-surface fade animation (`surfaceAnimations`, seeded/advanced by `PinnedPanelAnimations`/`AnimationChoreography`
  * and read back by `OverlayViewModel`/`RendererFloatingPanels`), the per-buffer column-to-column sweep
  * (`columnTransitions`, seeded by `CursorViewport.seedColumnTransition` when `MotionFamily.ColumnTransitions` is
  * enabled), and the per-surface panel scale-in/out geometry (`panelGeometry`, seeded by `PinnedPanelAnimations` when
  * `MotionFamily.PanelGeometry` is enabled). All three are written from the same handful of reducers/effects
  * (`AnimationChoreography`, `PinnedPanelAnimations`, `CursorViewport`, `MotionCancellation`) and read back together by
  * `DamageProducer.fullRenderDamage` and `StateManagerEditorCapability`'s tick-active check/advance, the same way
  * `Runtime.themeDiscovery`'s fields are.
  */
final case class MotionState(
    surfaceAnimations: Map[SurfaceId, SurfaceAnimationState] = Map.empty,
    columnTransitions: Map[BufferId, ColumnTransitionState] = Map.empty,
    panelGeometry: Map[SurfaceId, PanelGeometryState] = Map.empty
)
