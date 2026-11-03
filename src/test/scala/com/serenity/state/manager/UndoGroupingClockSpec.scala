package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade.createBuffer
import com.serenity.testkit.DeterministicClock
import com.serenity.ui.layout.ViewportSize
import com.serenity.{TestTemp, setBufferForPane, setCursorPosition}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** Undo grouping ends an edit run when two text-entry keys are more than [[com.serenity.state.models.EditClock.Pause]]
  * apart on the monotonic clock (#1930). Applying a batch and applying its events one at a time must leave the same
  * history when no pause falls between the keys, and one at a time leaves one more step when one does; the clock is
  * scripted, so neither outcome depends on how fast the machine happens to be (#2484, EditRunSettlingSpec).
  */
class UndoGroupingClockSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

  given Balance           = Balance.default
  given LoggerFactory[IO] = NoOpFactory[IO]

  private val clock = DeterministicClock()

  override protected def afterAll(): Unit =
    clock.shutdown()
    super.afterAll()

  private def run[A](program: IO[A]): A = program.unsafeRunSync()(using clock.runtime)

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

  private def undoSteps(stateManager: StateManager): IO[Int] = stateManager.getModel.map(_.undo.undoStack.size)

  private def oneAtATime(gapAfterSixth: Option[FiniteDuration]): Int =
    run(for
      stateManager <- editor
      _ <- keys.zipWithIndex.traverse_ { (key, index) =>
        stateManager.applyEvent(key) >> IO(if index == 5 then gapAfterSixth.foreach(clock.advance) else ())
      }
      steps <- undoSteps(stateManager)
    yield steps)

  private def batched: Int =
    run(for
      stateManager <- editor
      _            <- stateManager.applyEventBatch(keys, oneSlice)
      steps        <- undoSteps(stateManager)
    yield steps)

  "Twelve typed keys" should "be one undo step whether batched or applied one at a time, with no gap between them" in {
    val together = batched
    together shouldBe 1
    oneAtATime(gapAfterSixth = None) shouldBe together
  }

  "A gap of two seconds between two typed keys" should "leave one more undo step than batching them does" in {
    oneAtATime(gapAfterSixth = Some(2.seconds)) shouldBe batched + 1
  }

  "A gap of half a second between two typed keys" should "not end the run" in {
    oneAtATime(gapAfterSixth = Some(500.millis)) shouldBe batched
  }
