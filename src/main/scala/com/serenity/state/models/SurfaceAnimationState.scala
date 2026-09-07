package com.serenity.state.models

import com.serenity.animation.AnimationState

enum SurfacePhase:
  case BufferFadingOut // surface not rendered; buffer chars in overlay area fading out
  case Visible         // surface rendered (may have fade-in animation)
  case Exiting         // ghost surface fading out; focus already restored

final case class SurfaceAnimationState(
    phase: SurfacePhase = SurfacePhase.Visible,
    animationState: AnimationState = AnimationState.empty,
    overlayHeight: Int = 0,    // rows in overlay (excluding border) for building fade-in
    bufferFadeLength: Int = 0, // ticks to stay in BufferFadingOut before transitioning
    phaseTick: Int = 0         // ticks elapsed in current phase
)
