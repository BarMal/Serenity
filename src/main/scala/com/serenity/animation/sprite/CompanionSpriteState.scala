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
  * current clip has been running. Advanced one tick at a time by [[advance]], mirroring
  * `com.serenity.animation.WindowSitter`'s own tick-driven frame cycling.
  *
  * The transition policy between clips is deliberately explicit rather than "pick uniformly at random every tick",
  * which would look visually chaotic:
  *   1. Only idling rolls for a new action, and only once at least [[CompanionSpriteState.MinIdleTicksBeforeAction]]
  *      ticks have passed in the current idle run.
  *   2. Every non-idle action always returns to `Idle` the moment it completes one full loop -- two actions never chain
  *      directly into one another.
  *   3. The action chosen never repeats the one just played (tracked in [[lastAction]]), so consecutive rolls read as
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
    renderTicks: Long = 0
):

  def frameCount: Int = frameCounts.getOrElse(action, 1).max(1)

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
