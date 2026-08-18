package com.serenity.animation

import com.serenity.ui.layout.PanelPosition

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

/** Semantic request for a transition plan. */
final case class ElementTransitionRequest(
    scope: TransitionScope,
    placement: Option[PanelPosition] = None
)

/** Pure transition plan. Later renderer integration can lower this into existing animation primitives. */
final case class ElementTransitionPlan(
    scope: TransitionScope,
    kind: TransitionKind,
    direction: TransitionDirection,
    timing: TransitionTiming
)

object ElementTransitionPlanner:

  def plan(request: ElementTransitionRequest, settings: ElementTransitionSettings): ElementTransitionPlan =
    if !settings.enabled then
      ElementTransitionPlan(
        scope = request.scope,
        kind = TransitionKind.Disabled,
        direction = directionFor(request),
        timing = TransitionTiming.immediate
      )
    else
      ElementTransitionPlan(
        scope = request.scope,
        kind = settings.overrides.getOrElse(request.scope, defaultKind(request.scope)),
        direction = directionFor(request),
        timing = scaledTiming(settings.baseTiming, settings.speedScale)
      )

  private def defaultKind(scope: TransitionScope): TransitionKind =
    scope match
      case TransitionScope.PanelOpen =>
        TransitionKind.OutlineThenContent
      case TransitionScope.PanelClose =>
        TransitionKind.Fade
      case TransitionScope.Row =>
        TransitionKind.DirectionalSweep
      case TransitionScope.Glyph | TransitionScope.CommandRunner | TransitionScope.EditorInsertion =>
        TransitionKind.Fade
      case TransitionScope.SurfaceFrame | TransitionScope.SurfaceHeader =>
        TransitionKind.Fade

  private def directionFor(request: ElementTransitionRequest): TransitionDirection =
    request.placement match
      case Some(PanelPosition.Left)   => TransitionDirection.LeftToRight
      case Some(PanelPosition.Right)  => TransitionDirection.RightToLeft
      case Some(PanelPosition.Top)    => TransitionDirection.TopToBottom
      case Some(PanelPosition.Bottom) => TransitionDirection.BottomToTop
      case None =>
        request.scope match
          case TransitionScope.PanelClose => TransitionDirection.AnchorOut
          case _                          => TransitionDirection.AnchorIn

  private def scaledTiming(timing: TransitionTiming, speedScale: Double): TransitionTiming =
    val normalizedScale = speedScale.max(0.0)
    timing.copy(
      durationMs = math.round(timing.durationMs * normalizedScale).toInt,
      staggerMs = math.round(timing.staggerMs * normalizedScale).toInt,
      delayMs = math.round(timing.delayMs * normalizedScale).toInt,
      speedScale = normalizedScale
    )
