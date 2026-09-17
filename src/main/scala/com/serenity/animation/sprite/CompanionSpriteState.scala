package com.serenity.animation.sprite

import scala.util.Random

/** The clips a companion sprite sheet may define. Only `Idle` is guaranteed to have real frames on the placeholder
  * sheet `CompanionSpriteAssets` ships today -- painting falls back to the `Idle` frames for any action clip a sheet
  * doesn't define, so choosing one of these before a matching clip exists is never a rendering error, just a
  * temporarily invisible choice.
  */
enum CompanionSpriteAction:
  case Idle, Walk, Shoot, Morph

/** Frame-cycling state for the companion sprite: which clip is playing, which frame of it is showing, and how long the
  * current clip has been running. Advanced one tick at a time by [[advance]].
  *
  * The transition policy between clips is deliberately explicit rather than "pick uniformly at random every tick",
  * which would look visually chaotic:
  *   1. Typing takes priority over everything else: [[observeTyping]] forces the `Walk` action for a dual-cadence
  *      activity window (mirroring the retired `com.serenity.animation.WindowSitter`'s typing reaction, issue #934 v2),
  *      stepped via [[SpriteFrameSelection]] per the configured [[CompanionSpriteConfig.typingCycle]]. While that
  *      window is open ([[isTypingActive]]), the idle-roll policy below does not run.
  *   2. Otherwise, only idling rolls for a new action, and only once at least
  *      [[CompanionSpriteState.MinIdleTicksBeforeAction]] ticks have passed in the current idle run.
  *   3. Every non-idle action always returns to `Idle` the moment it completes one full loop -- two actions never chain
  *      directly into one another.
  *   4. The action chosen never repeats the one just played (tracked in [[lastAction]]), so consecutive rolls read as
  *      varied rather than "the same trick over and over".
  *
  * Action selection is pseudo-random but never touches unseeded global randomness itself: every call site passes in a
  * `Random`, so a test (or a production caller that wants to replay a session) can make the whole trace deterministic
  * by fixing the seed.
  */
final case class CompanionSpriteState(
    action: CompanionSpriteAction = CompanionSpriteAction.Idle,
    frameIndex: Int = 0,
    ticksInAction: Int = 0,
    lastAction: Option[CompanionSpriteAction] = None,
    frameCounts: Map[CompanionSpriteAction, Int] = Map(CompanionSpriteAction.Idle -> 1),
    // Counts every call to `tick`, including throttled-away ones, so `tick`'s own half-rate throttle at `reducedRate`
    // has something to alternate on that survives independently of `ticksInAction` (which resets on every clip
    // change and would make the throttle's phase depend on the transition policy).
    renderTicks: Long = 0,
    // Typing-reactivity state, merged in from `WindowSitter` (issue #934 v2). `typingActiveTicks` counts down the
    // dual-cadence activity window opened by `observeTyping`; `typingCycle`/`typingAscending` are the traversal style
    // and ping-pong direction `advance` steps through via `SpriteFrameSelection` while that window is open.
    typingActiveTicks: Int = 0,
    typingCycle: SpriteFrameCycle = SpriteFrameCycle.default,
    typingAscending: Boolean = true,
    lastTypedAtNanos: Option[Long] = None
):

  def frameCount: Int = frameCounts.getOrElse(action, 1).max(1)

  /** Whether a typing-triggered `Walk` reaction is still playing out. */
  def isTypingActive: Boolean = typingActiveTicks > 0

  /** React to printable input, using the interval since the previous character as activity intensity -- the same
    * dual-cadence shape `WindowSitter.observeTyping` used before the merge (issue #934 v2).
    */
  def observeTyping(
    nowNanos: Long,
    config: CompanionSpriteConfig = CompanionSpriteConfig.default
  ): CompanionSpriteState =
    val normalized    = config.normalized
    val intervalNanos = lastTypedAtNanos.map(previous => (nowNanos - previous).max(0L))
    val ticks = intervalNanos.fold(normalized.typingActiveTicks) { interval =>
      if interval <= normalized.typingFastThresholdMs.toLong * 1_000_000L then normalized.typingFastActiveTicks
      else normalized.typingActiveTicks
    }
    val walkFrameCount = frameCounts.getOrElse(CompanionSpriteAction.Walk, 1)
    val next = SpriteFrameSelection.next(
      cycle = normalized.typingCycle,
      frameCount = walkFrameCount,
      currentIndex = if action == CompanionSpriteAction.Walk && isTypingActive then frameIndex else 0,
      ascending = if action == CompanionSpriteAction.Walk && isTypingActive then typingAscending else true
    )
    copy(
      action = CompanionSpriteAction.Walk,
      frameIndex = next.index,
      ticksInAction = 0,
      typingActiveTicks = ticks,
      typingCycle = normalized.typingCycle,
      typingAscending = next.ascending,
      lastTypedAtNanos = Some(nowNanos)
    )

  /** The render-loop entry point: `advance` at full rate, or throttled to every second call when `reducedRate` is set
    * -- the "Reduced" visual flair tier's lower tick rate (`VisualFlairLevel` itself lives in `com.serenity.config`, a
    * layer above this package, so the throttle is a plain boolean rather than a dependency on that type).
    */
  def tick(
    random: Random,
    reducedRate: Boolean,
    actionChance: Double = CompanionSpriteState.DefaultActionChance
  ): CompanionSpriteState =
    if !reducedRate then advance(random, actionChance).copy(renderTicks = renderTicks + 1)
    else
      val nextRenderTicks = renderTicks + 1
      if nextRenderTicks % 2 == 0 then advance(random, actionChance).copy(renderTicks = nextRenderTicks)
      else copy(renderTicks = nextRenderTicks)

  /** Advance one animation tick per the transition policy documented on this class. */
  def advance(
    random: Random,
    actionChance: Double = CompanionSpriteState.DefaultActionChance
  ): CompanionSpriteState =
    if isTypingActive then advanceTyping
    else
      val nextTicks       = ticksInAction + 1
      val framesPerAction = frameCount
      val nextFrame       = (frameIndex + 1) % framesPerAction.max(1)
      val shouldStartAction =
        action == CompanionSpriteAction.Idle &&
          nextTicks >= CompanionSpriteState.MinIdleTicksBeforeAction &&
          random.nextDouble() < actionChance
      if shouldStartAction then
        val chosen = CompanionSpriteState.pickNonIdleAction(random, excluding = lastAction)
        copy(action = chosen, frameIndex = 0, ticksInAction = 0, lastAction = Some(chosen))
      else if action != CompanionSpriteAction.Idle && nextFrame == 0 then
        copy(action = CompanionSpriteAction.Idle, frameIndex = 0, ticksInAction = 0)
      else copy(frameIndex = nextFrame, ticksInAction = nextTicks)

  /** Ends any in-progress typing-triggered `Walk` reaction immediately, returning to `Idle` -- used when motion is
    * disabled or all active motion is cancelled (mirrors what resetting `WindowSitter` to its default used to do). A
    * no-op when no typing reaction is playing, since the idle-roll trick animation it would otherwise clobber has
    * nothing to do with typing.
    */
  def resetTyping: CompanionSpriteState =
    if isTypingActive then
      copy(action = CompanionSpriteAction.Idle, frameIndex = 0, ticksInAction = 0, typingActiveTicks = 0)
    else this

  /** Steps (or ends) the typing-triggered `Walk` reaction, returning to the resting `Idle` frame once the activity
    * window completes -- mirrors `WindowSitter.advance`'s own countdown.
    */
  private def advanceTyping: CompanionSpriteState =
    if typingActiveTicks == 1 then
      copy(action = CompanionSpriteAction.Idle, frameIndex = 0, ticksInAction = 0, typingActiveTicks = 0)
    else
      val walkFrameCount = frameCounts.getOrElse(CompanionSpriteAction.Walk, 1)
      val next           = SpriteFrameSelection.next(typingCycle, walkFrameCount, frameIndex, typingAscending)
      copy(frameIndex = next.index, typingActiveTicks = typingActiveTicks - 1, typingAscending = next.ascending)

object CompanionSpriteState:

  val DefaultActionChance: Double   = 0.02
  val MinIdleTicksBeforeAction: Int = 20

  private val nonIdleActions =
    Vector(CompanionSpriteAction.Walk, CompanionSpriteAction.Shoot, CompanionSpriteAction.Morph)

  private def pickNonIdleAction(random: Random, excluding: Option[CompanionSpriteAction]): CompanionSpriteAction =
    val candidates = excluding.fold(nonIdleActions)(previous => nonIdleActions.filterNot(_ == previous))
    candidates(random.nextInt(candidates.length))

  def default(frameCounts: Map[CompanionSpriteAction, Int]): CompanionSpriteState =
    CompanionSpriteState(frameCounts = frameCounts)

  /** The state a fresh session starts with: every action's frame count seeded from the bundled sheet's real idle frame
    * count (issue #934 v2) -- before this merge, `Runtime`'s bare `CompanionSpriteState()` left every non-idle action's
    * frame count at the case class's own `1` default, so a random roll into `Walk`/`Shoot`/`Morph` completed its "loop"
    * in a single tick and never visibly animated.
    */
  val default: CompanionSpriteState =
    CompanionSpriteState(frameCounts =
      CompanionSpriteAction.values.map(_ -> CompanionSpriteAssets.IdleFrameCount).toMap
    )
