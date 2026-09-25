package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{EasingCurve, Tween}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.ui.layout.PixelPoint
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Caret-glide (issue #1085 phase 2): `animationTicker.advanceAnimationsOnTick` advances every buffer's cursors'
  * `Cursor.glide` once per tick, mirroring `Runtime.columnTransitions`'/`Runtime.panelGeometry`'s tick-driven advance
  * (`StateManagerColumnTransitionTickSpec`/`StateManagerPanelGeometryTickSpec`) -- except glide lives on `Cursor`
  * inside `Buffer.editing.cursors` rather than a top-level `Runtime` map, since it is per-cursor storage now (`#1577`).
  */
class StateManagerCursorGlideTickSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(1)

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager.apply(logger).unsafeRunSync()

  private def seedGlide(sm: StateManager, steps: Int, cursors: List[Cursor] = List.empty): Unit =
    sm.updateState { state =>
      val glide  = Tween(start = PixelPoint(0, 0), end = PixelPoint(20, 0), curve = EasingCurve.Linear, steps = steps)
      val cursor = Cursor(CursorPosition(0, 0), glide = Some(glide))
      val buffer =
        Buffer
          .fromString(bufferId, "hello world\nsecond line")
          .copy(editing = EditingState.fromCursors(cursor :: cursors))
      // Advances `nextBufferId` past the seeded buffer, as a real allocation would; the second line gives a
      // multi-cursor fixture's second cursor somewhere valid to sit.
      state.copy(
        persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)),
        runtime = state.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))
      )
    }.unsafeRunSync()

  "advanceAnimationsOnTick" should "return true while a cursor glide is still in flight" in {
    val sm = makeStateManager()
    seedGlide(sm, steps = 4)

    val result = sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    result shouldBe true
    val glide = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors.head.glide
    glide.getOrElse(fail("expected an in-flight glide")).currentValue shouldBe PixelPoint(5, 0)
  }

  it should "drop the glide once it completes, returning false" in {
    val sm = makeStateManager()
    seedGlide(sm, steps = 1)

    val result = sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    result shouldBe false
    sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors.head.glide shouldBe None
  }

  it should "advance each cursor's glide independently in a multi-cursor buffer" in {
    val sm = makeStateManager()
    val secondCursor =
      Cursor(CursorPosition(1, 0), glide = Some(Tween(PixelPoint(0, 0), PixelPoint(20, 0), EasingCurve.Linear, 1)))
    seedGlide(sm, steps = 4, cursors = List(secondCursor))

    sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    val cursors = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors.toList
    cursors(0).glide shouldBe defined
    cursors(0).glide.get.isComplete shouldBe false
    cursors(1).glide shouldBe None // completed after one step, dropped
  }

  it should "leave other runtime animation state untouched while advancing a cursor glide" in {
    val sm = makeStateManager()
    seedGlide(sm, steps = 4)
    val before = sm.getCurrentState.unsafeRunSync()

    sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.runtime.themeTransition shouldBe before.runtime.themeTransition
    after.runtime.surfaceAnimations shouldBe before.runtime.surfaceAnimations
    after.runtime.columnTransitions shouldBe before.runtime.columnTransitions
    after.runtime.panelGeometry shouldBe before.runtime.panelGeometry
  }
