package com.serenity.animation

/** Character groups that can be lowered from a semantic transition plan into animation primitives. */
final case class ElementTransitionCells(
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
        AnimationState(
          lowerCells(plan, cells.all, tickRateMs, staggerFrames = 0, delayFrames = delayFrames(plan, tickRateMs))
        )
      case TransitionKind.DirectionalSweep =>
        AnimationState(
          lowerCells(
            plan,
            cells.all,
            tickRateMs,
            staggerFrames = staggerFrames(plan, tickRateMs),
            delayFrames = delayFrames(plan, tickRateMs)
          )
        )
      case TransitionKind.TypedText =>
        AnimationState(lowerTypedText(plan, cells.all, tickRateMs))
      case TransitionKind.LineAndCharacterTandem =>
        AnimationState(lowerLineAndCharacterTandem(plan, cells.all, tickRateMs))

  private def lowerOutlineThenContent(
    plan: ElementTransitionPlan,
    cells: ElementTransitionCells,
    tickRateMs: Int
  ): AnimationState =
    val plannedDelayFrames = delayFrames(plan, tickRateMs)
    val frameAnimations =
      lowerCells(plan, cells.frame, tickRateMs, staggerFrames = 0, delayFrames = plannedDelayFrames)
    val contentAnimations = lowerCells(
      plan,
      cells.content,
      tickRateMs,
      staggerFrames = staggerFrames(plan, tickRateMs),
      delayFrames = plannedDelayFrames + durationFrames(plan, tickRateMs)
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

  private def lowerTypedText(
    plan: ElementTransitionPlan,
    cells: Map[CharacterKey, CellAnimation],
    tickRateMs: Int
  ): Map[CharacterKey, AnimatedCell] =
    val orderedKeys = typedOrdering(plan.direction, cells.keys.toList)
    val offsets     = orderedKeys.zipWithIndex.toMap
    lowerCellsWithOffsets(
      cells,
      steps = durationFrames(plan, tickRateMs),
      staggerFrames = staggerFrames(plan, tickRateMs),
      delayFrames = delayFrames(plan, tickRateMs),
      key => offsets.getOrElse(key, 0)
    )

  private def lowerLineAndCharacterTandem(
    plan: ElementTransitionPlan,
    cells: Map[CharacterKey, CellAnimation],
    tickRateMs: Int
  ): Map[CharacterKey, AnimatedCell] =
    lowerCellsWithOffsets(
      cells,
      steps = durationFrames(plan, tickRateMs),
      staggerFrames = staggerFrames(plan, tickRateMs),
      delayFrames = delayFrames(plan, tickRateMs),
      tandemOffsetFor(plan.direction, cells.keys)
    )

  private def lowerCellsWithOffsets(
    cells: Map[CharacterKey, CellAnimation],
    steps: Int,
    staggerFrames: Int,
    delayFrames: Int,
    offsetFor: CharacterKey => Int
  ): Map[CharacterKey, AnimatedCell] =
    cells.map { (key, cell) =>
      key -> AnimatedCell.parametricForeground(
        char = cell.char,
        startColor = cell.startColor,
        endColor = cell.endColor,
        steps = steps,
        delayFrames = delayFrames.max(0) + offsetFor(key).max(0) * staggerFrames.max(0)
      )
    }

  private def typedOrdering(direction: TransitionDirection, keys: List[CharacterKey]): List[CharacterKey] =
    direction match
      case TransitionDirection.RightToLeft =>
        keys.sortBy(key => (key.line, -key.column))
      case TransitionDirection.TopToBottom =>
        keys.sortBy(key => (key.column, key.line))
      case TransitionDirection.BottomToTop =>
        keys.sortBy(key => (key.column, -key.line))
      case TransitionDirection.LeftToRight | TransitionDirection.AnchorIn | TransitionDirection.AnchorOut =>
        keys.sortBy(key => (key.line, key.column))

  private def tandemOffsetFor(direction: TransitionDirection, keys: Iterable[CharacterKey]): CharacterKey => Int =
    if keys.isEmpty then _ => 0
    else
      val minCol = keys.map(_.column).min
      val maxCol = keys.map(_.column).max
      val minRow = keys.map(_.line).min
      val maxRow = keys.map(_.line).max

      direction match
        case TransitionDirection.RightToLeft =>
          key => (key.line - minRow) + (maxCol - key.column)
        case TransitionDirection.TopToBottom =>
          key => (key.column - minCol) + (key.line - minRow)
        case TransitionDirection.BottomToTop =>
          key => (key.column - minCol) + (maxRow - key.line)
        case TransitionDirection.LeftToRight | TransitionDirection.AnchorIn | TransitionDirection.AnchorOut =>
          key => (key.line - minRow) + (key.column - minCol)

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

  private def delayFrames(plan: ElementTransitionPlan, tickRateMs: Int): Int =
    frames(plan.timing.delayMs, tickRateMs)

  private def frames(milliseconds: Int, tickRateMs: Int): Int =
    if milliseconds <= 0 || tickRateMs <= 0 then 0
    else math.max(1, math.round(milliseconds.toDouble / tickRateMs.toDouble).toInt)
