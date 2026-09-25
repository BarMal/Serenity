package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.StateManagerTestSupport
import com.serenity.config.{HotkeyAction, HotkeyConfig}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.state.models.{AppState, SurfaceContent}
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The command runner's double-tap window is a `LaneKey.Timer` job (#1697 Wave 3): lane work the effect barrier waits
  * for and quitting cancels, whose expiry comes back through the dispatcher and applies only while the binding it was
  * scheduled for is still the one pending.
  */
class TimerEffectLanesSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def keymapRecording(name: String): IO[StateManager] =
    for
      stateManager <- createStateManagerIO(name)
      _ <- stateManager.updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config = state.persisted.config.withHotkeyConfig(HotkeyConfig.forOs("Linux")))
        )
      )
      _ <- stateManager.applyEvent(ToggleCommandRunner)
      _ <- "keymap".toList.traverse_(char => stateManager.applyEvent(InsertChar(char)))
      _ <- stateManager.applyEvent(Enter)
      _ <- stateManager.applyEvent(Enter)
    yield stateManager

  private def record(stateManager: StateManager, char: Char, atMillis: Long): IO[Unit] =
    stateManager.applyEvent(RunnerRecordBinding(KeyStrokeInfo(InputKey.Character, Some(char), Set.empty), atMillis))

  private def pendingBinding(state: AppState): Option[Long] =
    state.commandRunnerSurface.flatMap(_.content match
      case SurfaceContent.CommandPalette(runner) =>
        runner.activeSettingsSurface.flatMap(_.current.recording).flatMap(_.pendingRecordedBinding).map(_._2)
      case _ => None)

  private def commandRunnerBindings(state: AppState): List[String] =
    state.persisted.config.inputConfig.hotkeyConfig.bindingsFor(HotkeyAction.ToggleCommandRunner).map(_.render)

  "The binding-recording expiry" should "assign the pending binding once the double-tap window has passed" in {
    val program =
      for
        stateManager <- keymapRecording("TimerLaneExpiryFires")
        _            <- record(stateManager, 'k', 1_000L)
        _            <- IO.sleep(150.millis)
        inWindow     <- stateManager.getCurrentState
        _            <- IO.sleep(100.millis)
        afterWindow  <- stateManager.getCurrentState
      yield (pendingBinding(inWindow), commandRunnerBindings(afterWindow), pendingBinding(afterWindow))

    val (pendingInWindow, bindingsAfter, pendingAfter) = runVirtual(program)
    pendingInWindow shouldBe Some(1_000L)
    bindingsAfter shouldBe List("k")
    pendingAfter shouldBe None
  }

  it should "be lane work the effect barrier waits for" in {
    val program =
      for
        stateManager <- keymapRecording("TimerLaneExpiryAwaited")
        _            <- record(stateManager, 'k', 1_000L)
        _            <- stateManager.runtimeLifecycle.awaitEffects
        settled      <- stateManager.getCurrentState
      yield commandRunnerBindings(settled)

    runVirtual(program) shouldBe List("k")
  }

  it should "not touch a recording that started after the one it was scheduled for" in {
    val program =
      for
        stateManager <- keymapRecording("TimerLaneExpiryStale")
        _            <- record(stateManager, 'k', 1_000L)
        _            <- IO.sleep(100.millis)
        _            <- stateManager.applyEvent(Escape)
        _            <- stateManager.applyEvent(Enter)
        _            <- record(stateManager, 'j', 2_000L)
        _            <- IO.sleep(150.millis)
        afterStale   <- stateManager.getCurrentState
        _            <- IO.sleep(100.millis)
        afterCurrent <- stateManager.getCurrentState
      yield (pendingBinding(afterStale), commandRunnerBindings(afterStale), commandRunnerBindings(afterCurrent))

    val (pendingAfterStale, bindingsAfterStale, bindingsAfterCurrent) = runVirtual(program)
    pendingAfterStale shouldBe Some(2_000L)
    bindingsAfterStale should not contain "k"
    bindingsAfterCurrent shouldBe List("j")
  }

  it should "be cancelled by quitting rather than firing into a shut-down editor" in {
    val program =
      for
        stateManager <- keymapRecording("TimerLaneExpiryQuit")
        before       <- stateManager.getCurrentState.map(commandRunnerBindings)
        _            <- record(stateManager, 'k', 1_000L)
        _            <- stateManager.runtimeLifecycle.forceQuit
        _            <- IO.sleep(1.second)
        after        <- stateManager.getCurrentState
      yield (before, commandRunnerBindings(after))

    val (before, after) = runVirtual(program)
    after shouldBe before
  }

  "EffectResult.CommandRunnerBindingExpired" should "leave the state untouched when nothing is pending for it" in {
    val state = AppState.initial
    EffectResult.applyIfCurrent(
      state,
      EffectResult.CommandRunnerBindingExpired(1_000L)
    ) should be theSameInstanceAs state
  }
