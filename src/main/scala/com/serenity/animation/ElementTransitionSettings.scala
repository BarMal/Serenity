package com.serenity.animation

/** Semantic UI element scope used to choose transition behavior before lowering to cell animations. */
enum TransitionScope:
  case SurfaceFrame
  case SurfaceHeader
  case Row
  case Glyph
  case CommandRunner
  case EditorInsertion
  case PanelOpen
  case PanelClose

/** Transition strategy chosen for a semantic UI element. */
enum TransitionKind:
  case Disabled
  case Fade
  case TypedText
  case DirectionalSweep
  case OutlineThenContent
  case LineAndCharacterTandem

/** Direction used by ordered reveal transitions. */
enum TransitionDirection:
  case LeftToRight
  case RightToLeft
  case TopToBottom
  case BottomToTop
  case AnchorIn
  case AnchorOut

/** Timing values used by deterministic transition planning. */
final case class TransitionTiming(
    durationMs: Int,
    staggerMs: Int,
    delayMs: Int,
    speedScale: Double
)

object TransitionTiming:
  val immediate: TransitionTiming = TransitionTiming(durationMs = 0, staggerMs = 0, delayMs = 0, speedScale = 0.0)

/** Transition configuration independent of rendering or runtime ticking. */
final case class ElementTransitionSettings(
    enabled: Boolean,
    baseTiming: TransitionTiming,
    speedScale: Double,
    overrides: Map[TransitionScope, TransitionKind] = Map.empty
)

object ElementTransitionSettings:
  val disabled: ElementTransitionSettings =
    ElementTransitionSettings(enabled = false, baseTiming = TransitionTiming.immediate, speedScale = 0.0)

  val subtle: ElementTransitionSettings =
    ElementTransitionSettings(
      enabled = true,
      baseTiming = TransitionTiming(durationMs = 160, staggerMs = 12, delayMs = 0, speedScale = 1.0),
      speedScale = 1.0
    )

  val smooth: ElementTransitionSettings =
    ElementTransitionSettings(
      enabled = true,
      baseTiming = TransitionTiming(durationMs = 220, staggerMs = 16, delayMs = 0, speedScale = 1.0),
      speedScale = 1.0
    )

  val expressive: ElementTransitionSettings =
    ElementTransitionSettings(
      enabled = true,
      baseTiming = TransitionTiming(durationMs = 280, staggerMs = 22, delayMs = 20, speedScale = 1.0),
      speedScale = 1.0
    )
