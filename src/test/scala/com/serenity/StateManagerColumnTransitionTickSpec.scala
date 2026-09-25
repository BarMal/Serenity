package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{EasingCurve, TransitionDirection, Tween}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.ColumnTransitionState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Column-based document layout (issue #1338, Phase 1 animation): `animationTicker.advanceAnimationsOnTick` advances
  * `Runtime.columnTransitions` once per tick, mirroring `Runtime.themeTransition`'s tick-driven advance, and drops a
  * transition once it completes.
  */
class StateManagerColumnTransitionTickSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager.apply(logger).unsafeRunSync()

  private def seedTransition(sm: StateManager, bufferId: com.serenity.state.models.BufferId, steps: Int): Unit =
    sm.updateState { state =>
      state.copy(runtime =
        state.runtime.copy(columnTransitions =
          Map(
            bufferId -> ColumnTransitionState(
              tween = Tween(start = 0.0, end = 1.0, curve = EasingCurve.Linear, steps = steps),
              direction = TransitionDirection.RightToLeft,
              previousTopLine = 0,
              previousTopVisualLine = 0
            )
          )
        )
      )
    }.unsafeRunSync()

  private def firstBufferId(sm: StateManager): com.serenity.state.models.BufferId =
    val state = sm.getCurrentState.unsafeRunSync()
    if state.persisted.buffers.nonEmpty then state.persisted.buffers.head._1
    else
      val id = state.runtime.nextBufferId
      sm.updateState { s =>
        val buffer = com.serenity.state.models.Buffer.newEmpty(s.runtime.nextBufferId)
        s.copy(persisted = s.persisted.copy(buffers = s.persisted.buffers + (id -> buffer)))
      }.unsafeRunSync()
      id

  "advanceAnimationsOnTick" should "return true while a column transition is still in flight" in {
    val sm       = makeStateManager()
    val bufferId = firstBufferId(sm)
    seedTransition(sm, bufferId, steps = 4)

    val result = sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    result shouldBe true
    val advanced = sm.getCurrentState.unsafeRunSync().runtime.columnTransitions(bufferId)
    advanced.progress shouldBe 0.25
  }

  it should "drop the column transition once it completes, returning false" in {
    val sm       = makeStateManager()
    val bufferId = firstBufferId(sm)
    seedTransition(sm, bufferId, steps = 1)

    val result = sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    result shouldBe false
    sm.getCurrentState.unsafeRunSync().runtime.columnTransitions shouldBe empty
  }

  it should "leave other runtime animation state untouched while advancing a column transition" in {
    val sm       = makeStateManager()
    val bufferId = firstBufferId(sm)
    seedTransition(sm, bufferId, steps = 4)
    val before = sm.getCurrentState.unsafeRunSync()

    sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.runtime.themeTransition shouldBe before.runtime.themeTransition
    after.runtime.surfaceAnimations shouldBe before.runtime.surfaceAnimations
  }
