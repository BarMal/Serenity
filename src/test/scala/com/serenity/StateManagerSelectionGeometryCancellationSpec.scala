package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{EasingCurve, Tween}
import com.serenity.command.{Command, CommandCategory, CommandIntent, MotionIntent, SettingsIntent}
import com.serenity.config.MotionAccessibility
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.LayoutRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Selection grow/settle (issue #1085 phase 3): disabling motion entirely (via motion accessibility) cancels every
  * in-flight `Cursor.selectionGeometry`, the same way it cancels `Cursor.glide`/`Runtime.panelGeometry`
  * (`StateManagerMotionCancellation.cancelActiveMotion`). Mirrors `StateManagerCursorGlideCancellationSpec`.
  */
class StateManagerSelectionGeometryCancellationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(1)

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager.apply(logger).unsafeRunSync()

  private def seedGeometry(sm: StateManager): Unit =
    sm.updateState { state =>
      val rect     = LayoutRect(x = 0, y = 0, width = 4, height = 1)
      val tween    = Tween(start = rect.copy(width = 0), end = rect, curve = EasingCurve.Linear, steps = 4)
      val geometry = SelectionGeometryState(List(SelectionLineGeometry(SelectionLineKey(0, 0), tween)))
      val cursor   = Cursor(CursorPosition(0, 0), Some(CursorPosition(0, 0)), selectionGeometry = Some(geometry))
      val buffer   = Buffer.fromString(bufferId, "hello world").copy(editing = EditingState.fromCursors(List(cursor)))
      // Advances `nextBufferId` past the seeded buffer, as a real allocation would: cancellation now commits through
      // validation, which rejects a state whose next buffer ID collides with a live one.
      state.copy(
        persisted = state.persisted.copy(buffers = state.persisted.buffers + (bufferId -> buffer)),
        runtime = state.runtime.copy(nextBufferId = BufferId(bufferId.value + 1))
      )
    }.unsafeRunSync()

  "disabling motion accessibility" should "cancel every in-flight selection geometry" in {
    val sm = makeStateManager()
    seedGeometry(sm)

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
    state.persisted.buffers(bufferId).editing.cursors.head.selectionGeometry shouldBe None
    sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync() shouldBe false
  }
