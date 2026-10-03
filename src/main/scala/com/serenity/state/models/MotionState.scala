package com.serenity.state.models

/** The per-surface fade animation (`surfaceAnimations`, seeded/advanced by `PinnedPanelAnimations`/
  * `AnimationChoreography` and read back by `OverlayViewModel`/`RendererFloatingPanels`), read together with
  * `Runtime.themeDiscovery`'s transition by `DamageProducer.fullRenderDamage` and `StateManagerEditorCapability`'s
  * tick-active check/advance.
  */
final case class MotionState(
    surfaceAnimations: Map[SurfaceId, SurfaceAnimationState] = Map.empty
)
