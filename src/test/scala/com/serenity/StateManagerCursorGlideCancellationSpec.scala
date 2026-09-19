package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{EasingCurve, Tween}
import com.serenity.command.{Command, CommandCategory, CommandIntent, MotionIntent, SettingsIntent}
import com.serenity.config.MotionAccessibility
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.PixelPoint
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Caret-glide (issue #1085 phase 2): disabling the `Cursor` motion family (via motion accessibility, or the family's
  * own speed scale) cancels every in-flight `Cursor.glide`, the same way disabling `PanelGeometry`/`ColumnTransitions`
  * cancels their own in-flight state (`StateManagerConfigEffects.cancelMotionFamily`). Caught as a regression by
  * `CommandRunnerAnimationSpec`'s "cancel all active animation state" case, which exercises every family generically.
  */
class StateManagerCursorGlideCancellationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(1)

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager.apply(logger).unsafeRunSync()

  private def seedGlide(sm: StateManager): Unit =
    sm.updateState { state =>
      val glide  = Tween(start = PixelPoint(0, 0), end = PixelPoint(20, 0), curve = EasingCurve.Linear, steps = 4)
      val cursor = Cursor(CursorPosition(0, 0), glide = Some(glide))
      val buffer = Buffer.fromString(bufferId, "hello world").copy(editing = EditingState.fromCursors(List(cursor)))
      state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)))
    }.unsafeRunSync()

  "disabling motion accessibility" should "cancel every in-flight cursor glide" in {
    val sm = makeStateManager()
    seedGlide(sm)

    sm.commandExecutor
      .executeCommand(
        Command.typed(
          "disable-motion",
          "Disable motion",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionAccessibility(MotionAccessibility.Off))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    state.persisted.buffers(bufferId).editing.cursors.head.glide shouldBe None
    sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync() shouldBe false
  }

  it should "cancel an in-flight cursor glide when only the Cursor family's own speed scale is disabled" in {
    val sm = makeStateManager()
    seedGlide(sm)

    sm.commandExecutor
      .executeCommand(
        Command.typed(
          "disable-cursor-motion",
          "Disable cursor motion",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCursorTransitionSpeedScale(0.0))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    state.persisted.buffers(bufferId).editing.cursors.head.glide shouldBe None
  }
