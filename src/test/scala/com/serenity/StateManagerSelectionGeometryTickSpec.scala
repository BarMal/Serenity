package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{EasingCurve, Tween}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.ui.layout.LayoutRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Selection grow/settle (issue #1085 phase 3): `animationTicker.advanceAnimationsOnTick` advances every buffer's
  * cursors' `Cursor.selectionGeometry` once per tick, mirroring `StateManagerCursorGlideTickSpec`'s own precedent for
  * per-cursor tick-driven state.
  */
class StateManagerSelectionGeometryTickSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(1)

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager.apply(logger).unsafeRunSync()

  private def rect(width: Int): LayoutRect = LayoutRect(x = 0, y = 0, width = width, height = 1)

  private def seedGeometry(sm: StateManager, steps: Int, cursors: List[Cursor] = List.empty): Unit =
    sm.updateState { state =>
      val tween    = Tween(start = rect(0), end = rect(4), curve = EasingCurve.Linear, steps = steps)
      val geometry = SelectionGeometryState(List(SelectionLineGeometry(SelectionLineKey(0, 0), tween)))
      val cursor   = Cursor(CursorPosition(0, 0), Some(CursorPosition(0, 0)), selectionGeometry = Some(geometry))
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

  "advanceAnimationsOnTick" should "return true while a selection geometry is still in flight" in {
    val sm = makeStateManager()
    seedGeometry(sm, steps = 4)

    val result = sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    result shouldBe true
    val geometry =
      sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors.head.selectionGeometry
    geometry.getOrElse(fail("expected an in-flight geometry")).rectFor(0, 0) shouldBe Some(rect(1))
  }

  it should "drop the geometry once it completes, returning false" in {
    val sm = makeStateManager()
    seedGeometry(sm, steps = 1)

    val result = sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    result shouldBe false
    sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors.head.selectionGeometry shouldBe None
  }

  it should "advance each cursor's selection geometry independently in a multi-cursor buffer" in {
    val sm             = makeStateManager()
    val secondTween    = Tween(rect(0), rect(4), EasingCurve.Linear, 1)
    val secondGeometry = SelectionGeometryState(List(SelectionLineGeometry(SelectionLineKey(1, 0), secondTween)))
    val secondCursor =
      Cursor(CursorPosition(1, 0), Some(CursorPosition(1, 0)), selectionGeometry = Some(secondGeometry))
    seedGeometry(sm, steps = 4, cursors = List(secondCursor))

    sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    val cursors = sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursors.toList
    cursors(0).selectionGeometry shouldBe defined
    cursors(0).selectionGeometry.get.isComplete shouldBe false
    cursors(1).selectionGeometry shouldBe None // completed after one step, dropped
  }

  it should "leave other runtime animation state untouched while advancing a selection geometry" in {
    val sm = makeStateManager()
    seedGeometry(sm, steps = 4)
    val before = sm.getCurrentState.unsafeRunSync()

    sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.runtime.themeTransition shouldBe before.runtime.themeTransition
    after.runtime.surfaceAnimations shouldBe before.runtime.surfaceAnimations
    after.runtime.columnTransitions shouldBe before.runtime.columnTransitions
    after.runtime.panelGeometry shouldBe before.runtime.panelGeometry
  }
