package com.serenity.animation

/** Character groups that can be lowered from a semantic transition plan into animation primitives. */
case class ElementTransitionCells(
    frame: Map[CharacterKey, CellAnimation] = Map.empty,
    content: Map[CharacterKey, CellAnimation] = Map.empty
):
  /** Combine all element cells when a transition kind does not need separate frame/content phases. */
  def all: Map[CharacterKey, CellAnimation] = frame ++ content

/** Lowers semantic element transition plans into existing character animation state. */
object ElementTransitionLowerer:

  /** Build active character animations for a semantic transition plan. */
  def lower(
    plan: ElementTransitionPlan,
    cells: ElementTransitionCells,
    tickRateMs: Int = 16
  ): AnimationState =
    plan.kind match
      case TransitionKind.Disabled =>
        AnimationState.empty
      case TransitionKind.OutlineThenContent =>
        lowerOutlineThenContent(plan, cells, tickRateMs)
      case TransitionKind.Fade =>
        AnimationState(lowerCells(plan, cells.all, tickRateMs, staggerFrames = 0, delayFrames = 0))
      case TransitionKind.TypedText | TransitionKind.DirectionalSweep | TransitionKind.LineAndCharacterTandem =>
        AnimationState(
          lowerCells(plan, cells.all, tickRateMs, staggerFrames = staggerFrames(plan, tickRateMs), delayFrames = 0)
        )

  private def lowerOutlineThenContent(
    plan: ElementTransitionPlan,
    cells: ElementTransitionCells,
    tickRateMs: Int
  ): AnimationState =
    val frameAnimations = lowerCells(plan, cells.frame, tickRateMs, staggerFrames = 0, delayFrames = 0)
    val contentAnimations = lowerCells(
      plan,
      cells.content,
      tickRateMs,
      staggerFrames = staggerFrames(plan, tickRateMs),
      delayFrames = durationFrames(plan, tickRateMs)
    )
    AnimationState(frameAnimations ++ contentAnimations)

  private def lowerCells(
    plan: ElementTransitionPlan,
    cells: Map[CharacterKey, CellAnimation],
    tickRateMs: Int,
    staggerFrames: Int,
    delayFrames: Int
  ): Map[CharacterKey, AnimatedCell] =
    val (flow, sweep) = flowFor(plan.direction)
    FlowAnimationBuilder.build(
      cells = cells,
      direction = flow,
      sweep = sweep,
      steps = durationFrames(plan, tickRateMs),
      staggerFrames = staggerFrames,
      delayFrames = delayFrames
    )

  private def flowFor(direction: TransitionDirection): (FlowDirection, SweepDirection) =
    direction match
      case TransitionDirection.LeftToRight | TransitionDirection.AnchorIn | TransitionDirection.AnchorOut =>
        FlowDirection.ByColumn -> SweepDirection.Forward
      case TransitionDirection.RightToLeft =>
        FlowDirection.ByColumn -> SweepDirection.Backward
      case TransitionDirection.TopToBottom =>
        FlowDirection.ByRow -> SweepDirection.Forward
      case TransitionDirection.BottomToTop =>
        FlowDirection.ByRow -> SweepDirection.Backward

  private def durationFrames(plan: ElementTransitionPlan, tickRateMs: Int): Int =
    frames(plan.timing.durationMs, tickRateMs)

  private def staggerFrames(plan: ElementTransitionPlan, tickRateMs: Int): Int =
    frames(plan.timing.staggerMs, tickRateMs)

  private def frames(milliseconds: Int, tickRateMs: Int): Int =
    if milliseconds <= 0 || tickRateMs <= 0 then 0
    else math.max(1, math.round(milliseconds.toDouble / tickRateMs.toDouble).toInt)
