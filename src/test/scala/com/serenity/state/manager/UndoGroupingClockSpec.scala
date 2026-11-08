package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.std.Queue
import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade.createBuffer
import com.serenity.testkit.VirtualTime
import com.serenity.ui.layout.ViewportSize
import com.serenity.{TestTemp, setBufferForPane, setCursorPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** Undo grouping ends an edit run when two text-entry keys are more than [[com.serenity.state.models.EditClock.Pause]]
  * apart on the monotonic clock (#1930). Applying a batch and applying its events one at a time must leave the same
  * history when no pause falls between the keys, and one at a time leaves one more step when one does (#2484,
  * EditRunSettlingSpec). Time here is `TestControl`'s: it moves only when the spec advances it, so neither outcome
  * depends on how fast the machine happens to be.
  */
class UndoGroupingClockSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = NoOpFactory[IO]

  private val oneSlice: EventBatchSteps[Event] = EventBatchSteps((_, event) => event, _ => false, 1.hour)

  private val keys: List[Event] = "abcdefghijkl".toList.map(InsertChar(_))

  private def editor: IO[StateManager] =
    for
      stateManager <- StateManager(NoOpLogger[IO], sessionRootOverride = Some(TestTemp.directory("undo-clock")))
      bufferId     <- stateManager.createBuffer("lorem ipsum dolor sit amet", None)
      state        <- stateManager.getCurrentState
      paneId = state.persisted.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)
      _ <- stateManager.applyEvent(ResizeEvent(ViewportSize(110, 36)))
      _ <- stateManager.setCursorPosition(paneId, 0, 5)
    yield stateManager

  private def undoSteps(stateManager: StateManager): Int =
    stateManager.getModel.map(_.undo.undoStack.size).unsafeRunSync()

  /** An editor that applies each key the spec offers, as it is offered, on a clock that only the spec moves. */
  final private class Keyboard(val stateManager: StateManager, queue: Queue[IO, Event], control: TestControl[Nothing]):

    def press(key: Event): Unit =
      queue.offer(key).unsafeRunSync()
      control.tick.unsafeRunSync()

    def pause(gap: FiniteDuration): Unit = if gap > Duration.Zero then control.advanceAndTick(gap).unsafeRunSync()

  private def keyboard(): Keyboard =
    val ready = Deferred.unsafe[IO, StateManager]
    val queue = Queue.unbounded[IO, Event].unsafeRunSync()
    val program: IO[Nothing] =
      editor.flatMap(stateManager =>
        ready.complete(stateManager) >> queue.take.flatMap(stateManager.applyEvent).foreverM
      )
    val control = TestControl.execute(program).unsafeRunSync()
    control.tick.unsafeRunSync()
    Keyboard(ready.get.unsafeRunSync(), queue, control)

  private def batched: Int =
    VirtualTime.runVirtual(
      for
        stateManager <- editor
        _            <- stateManager.applyEventBatch(keys, oneSlice)
        steps        <- stateManager.getModel.map(_.undo.undoStack.size)
      yield steps
    )

  /** Presses the twelve keys, letting `gap` pass after the sixth, and gives the steps before and after the gap. */
  private def oneAtATime(gap: FiniteDuration): (Int, Int) =
    val board = keyboard()
    keys.take(6).foreach(board.press)
    val before = undoSteps(board.stateManager)
    board.pause(gap)
    keys.drop(6).foreach(board.press)
    (before, undoSteps(board.stateManager))

  "Twelve typed keys" should "be one undo step whether batched or pressed one at a time with no time between them" in {
    batched shouldBe 1
    oneAtATime(gap = Duration.Zero) shouldBe (1, 1)
  }

  "A gap of two seconds between two typed keys" should "end the run, leaving one more undo step than batching them does" in {
    val (before, after) = oneAtATime(gap = 2.seconds)
    before shouldBe 1
    after shouldBe batched + 1
  }

  "A gap of half a second between two typed keys" should "not end the run" in {
    oneAtATime(gap = 500.millis) shouldBe (1, batched)
  }
