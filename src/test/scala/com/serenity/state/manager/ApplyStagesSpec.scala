package com.serenity.state.manager

import java.util.concurrent.ConcurrentLinkedQueue

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{Event, InsertChar, ResizeEvent, Undo}
import com.serenity.rope.Balance
import com.serenity.state.manager.ApplyStage.*
import com.serenity.state.manager.StateManagerTestFacade.{createBuffer, updateState}
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.ViewportSize
import com.serenity.{TestTemp, setBufferForPane, setCursorPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** A batch tells its stage clock where each input's time goes: reading the input as an event, stepping a typed key,
  * applying any other event, and committing a run of typed keys.
  */
class ApplyStagesSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = NoOpFactory[IO]

  final private class Recording extends ApplyStages:
    val seen = new ConcurrentLinkedQueue[ApplyStage]()

    def sync[A](stage: ApplyStage)(work: => A): A =
      seen.add(stage): Unit
      work

    def io[A](stage: ApplyStage)(work: IO[A]): IO[A] =
      IO(seen.add(stage)).void *> work

    def stages: List[ApplyStage] = seen.toArray.toList.map(_.asInstanceOf[ApplyStage])

  private def editor: IO[StateManager] =
    for
      stateManager <- StateManager(
        NoOpLogger[IO],
        sessionRootOverride = Some(TestTemp.directory("apply-stages")),
        dictionaryCache = SharedDictionary.default
      )
      _ <- stateManager.updateState(state => state.copy(persisted = state.persisted.copy(config = AppConfig.default)))
      bufferId <- stateManager.createBuffer("lorem ipsum dolor sit amet", None)
      state    <- stateManager.getCurrentState
      paneId = state.persisted.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)
      _ <- stateManager.applyEvent(ResizeEvent(ViewportSize(110, 36)))
      _ <- stateManager.setCursorPosition(paneId, 0, 5)
    yield stateManager

  private def stagesOf(events: List[Event]): List[ApplyStage] =
    val recording = new Recording
    val steps     = EventBatchSteps[Event]((_, event) => event, _ => false, 1.hour, recording)
    editor.flatMap(_.applyEventBatch(events, steps)).unsafeRunSync()
    recording.stages

  "A batch" should "read, step and commit a run of typed keys, in that order" in {
    stagesOf(List(InsertChar('a'), InsertChar('b'))) shouldBe
      List(Decode, TypedStep, Decode, TypedStep, TypedCommit)
  }

  it should "end the run before an event it applies on its own, then apply that event" in {
    stagesOf(List(InsertChar('a'), Undo)) shouldBe
      List(Decode, TypedStep, Decode, TypedStep, TypedCommit, Event)
  }

  it should "name no stage when it is not given a stage clock" in {
    val steps = EventBatchSteps[Event]((_, event) => event, _ => false, 1.hour)
    steps.stages shouldBe ApplyStages.none
  }
