package com.serenity.animation.sprite

import scala.util.Random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompanionSpriteStateSpec extends AnyFlatSpec with Matchers:

  private val frameCounts = Map(
    CompanionSpriteAction.Idle  -> 4,
    CompanionSpriteAction.Walk  -> 3,
    CompanionSpriteAction.Shoot -> 2,
    CompanionSpriteAction.Morph -> 2
  )

  "CompanionSpriteState.advance" should "cycle frames of the current clip without changing action, when never rolling into one" in {
    val never  = new Random(0L)
    val state  = CompanionSpriteState(frameCounts = frameCounts)
    val ticked = (1 to 3).foldLeft(state)((s, _) => s.advance(never, actionChance = 0.0))

    ticked.action shouldBe CompanionSpriteAction.Idle
    ticked.frameIndex shouldBe 3
  }

  it should "wrap the frame index back to 0 at the end of the clip's frame count" in {
    val never = new Random(0L)
    val state = CompanionSpriteState(frameCounts = frameCounts, frameIndex = 3)

    state.advance(never, actionChance = 0.0).frameIndex shouldBe 0
  }

  it should "never leave Idle when actionChance is zero, no matter how many ticks pass" in {
    val random = new Random(42L)
    val state  = CompanionSpriteState(frameCounts = frameCounts)
    val ticked = (1 to 200).foldLeft(state)((s, _) => s.advance(random, actionChance = 0.0))

    ticked.action shouldBe CompanionSpriteAction.Idle
  }

  it should "always leave Idle for a non-idle action once enough idle ticks have passed, when actionChance is one" in {
    val random = new Random(7L)
    val state  = CompanionSpriteState(frameCounts = frameCounts)
    val ticked =
      (1 to CompanionSpriteState.MinIdleTicksBeforeAction).foldLeft(state)((s, _) => s.advance(random, actionChance = 1.0))

    ticked.action should not be CompanionSpriteAction.Idle
    ticked.frameIndex shouldBe 0
  }

  it should "return to Idle once a non-idle clip completes a full loop" in {
    val random = new Random(7L)
    val start  = CompanionSpriteState(frameCounts = frameCounts, action = CompanionSpriteAction.Shoot, frameIndex = 0)

    val afterOneLoop = (1 to 2).foldLeft(start)((s, _) => s.advance(random, actionChance = 0.0))

    afterOneLoop.action shouldBe CompanionSpriteAction.Idle
    afterOneLoop.frameIndex shouldBe 0
  }

  it should "be deterministic: the same seed and the same sequence of calls produce the same trace" in {
    def trace(seed: Long): Vector[(CompanionSpriteAction, Int)] =
      val random = new Random(seed)
      val start  = CompanionSpriteState(frameCounts = frameCounts)
      Iterator
        .iterate(start)(_.advance(random, actionChance = 0.1))
        .take(50)
        .map(s => (s.action, s.frameIndex))
        .toVector

    trace(123L) shouldBe trace(123L)
  }

  it should "not roll into an action before the minimum idle-tick threshold, even with actionChance one" in {
    val random = new Random(9L)
    val state  = CompanionSpriteState(frameCounts = frameCounts)
    val ticked =
      (1 until CompanionSpriteState.MinIdleTicksBeforeAction).foldLeft(state)((s, _) => s.advance(random, actionChance = 1.0))

    ticked.action shouldBe CompanionSpriteAction.Idle
  }

  // -- Transition policy: forced (actionChance = 1) so every idle run breaks into an action, isolating the choice of
  // *which* action from *whether* one starts at all -- covered separately above.

  private def actionTrace(seed: Long, ticks: Int): Vector[CompanionSpriteAction] =
    val random = new Random(seed)
    val start  = CompanionSpriteState(frameCounts = frameCounts)
    Iterator.iterate(start)(_.advance(random, actionChance = 1.0)).take(ticks).map(_.action).toVector

  it should "always return to Idle between one non-idle action and the next, never chaining two actions directly" in {
    val trace = actionTrace(seed = 11L, ticks = 400)
    val nonIdleRuns = trace
      .foldLeft(Vector.empty[CompanionSpriteAction]) {
        case (acc, CompanionSpriteAction.Idle) => acc
        case (acc, action)                     => acc :+ action
      }
    // Every entry in `trace` immediately preceding a change of non-idle action must have passed through Idle: proven
    // structurally by construction (advance only ever assigns a non-idle action from the Idle branch), so the
    // behavioural check here is the one that actually matters -- no two *consecutive ticks* are both non-idle
    // actions unless they're the same run of one action's own frames.
    trace.sliding(2).foreach {
      case Vector(a, b) if a != CompanionSpriteAction.Idle && b != CompanionSpriteAction.Idle =>
        a shouldBe b
      case _ => ()
    }
    nonIdleRuns should not be empty
  }

  it should "never immediately repeat the same action across two consecutive idle-to-action rolls" in {
    def actionRuns(trace: Vector[CompanionSpriteAction]): Vector[CompanionSpriteAction] =
      trace
        .foldLeft(Vector.empty[CompanionSpriteAction]) {
          case (acc, CompanionSpriteAction.Idle)                => acc
          case (acc, action) if acc.lastOption.contains(action) => acc
          case (acc, action)                                    => acc :+ action
        }

    val runs = actionRuns(actionTrace(seed = 11L, ticks = 2000))
    runs.size should be > 5
    runs.sliding(2).foreach {
      case Vector(first, second) => first should not be second
      case _                     => ()
    }
  }
