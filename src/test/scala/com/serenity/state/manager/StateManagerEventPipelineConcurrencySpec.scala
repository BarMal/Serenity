package com.serenity.state.manager

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.parallel.*
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.state.undo.UndoState
import com.serenity.ui.presets.UiPresetStore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for #1570: `StateManagerEventPipeline.applyEvent` must not lose a concurrent writer's commit.
  *
  * `NewTab` is a convenient probe because each dispatch derives its new buffer's id from `state.runtime.nextBufferId`
  * -- a counter carried on the very snapshot `applyEvent` reads at the top of the call and commits back at the end.
  * Before the fix, every concurrent `applyEvent` call raced the same read-compute-`stateRef.set` shape as the animation
  * ticker fixed in #1564/#1571: whichever call's blind `stateRef.set` landed last discarded every other concurrent
  * call's fully-computed (and individually valid) new buffer, so firing N of them concurrently yielded far fewer than N
  * new buffers. Serializing the whole dispatch (this fix) makes every call observe the other calls' commits, so N
  * concurrent `NewTab` events always yield exactly N new buffers.
  */
class StateManagerEventPipelineConcurrencySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def newPipeline(
    sharedStateRef: Ref[IO, AppState],
    onEffect: (StateManagerOperationBoundary, AppEffect) => IO[Unit] = (_, _) => IO.unit
  ): IO[StateManagerEventPipeline] =
    for
      fiberRef               <- Ref.of[IO, Option[cats.effect.Fiber[IO, Throwable, Unit]]](None)
      cacheRef               <- Ref.of[IO, Option[MouseTargetCache]](None)
      sharedBufferAnimations <- Ref.of[IO, Map[BufferId, com.serenity.animation.AnimationState]](Map.empty)
      sharedUndoRef          <- Ref.of[IO, UndoState](UndoState())
      pipelineLogger = org.typelevel.log4cats.noop.NoOpLogger.impl[IO]
      operations <- StateManagerOperationBoundary.create(sharedStateRef, fiberRef, pipelineLogger)
      statePort = new EventStatePort:
        val stateRef                 = sharedStateRef
        val logger                   = pipelineLogger
        val documentAnalysisFiberRef = fiberRef
        val mouseTargetCacheRef      = cacheRef
        val bufferAnimationsRef      = sharedBufferAnimations
      effectPort = EventEffectPort(
        interpretEffect = effect => onEffect(operations, effect),
        interpretCommand = (_, _) => IO.unit
      )
      workflowPort = new EventWorkflowPort:
        def beginCloseAction(scope: CloseScope, state: AppState): IO[Unit]      = IO.unit
        def createBuffer(content: String, filePath: Option[Path]): IO[BufferId] = IO.pure(BufferId(0))
        def createPane(bufferId: Option[BufferId]): IO[PaneId]                  = IO.pure(PaneId(0))
      undoRecording = new UndoRecording(new UndoRecordingPort:
        val stateRef = sharedStateRef; val undoRef = sharedUndoRef
        export operations.validateAndUpdateState)
    yield new StateManagerEventPipeline(
      statePort,
      effectPort,
      workflowPort,
      UiPresetStore(Path.of("target", "state-manager-event-pipeline-concurrency-spec.json")),
      update =>
        sharedStateRef.modify(state =>
          val config = update(state.persisted.config)
          (state.copy(persisted = state.persisted.copy(config = config)), config)
        ),
      (_, _) => IO.unit,
      operations,
      undoRecording
    )

  "StateManagerEventPipeline.applyEvent" should "commit every concurrent dispatch instead of losing some to a racing writer (#1570)" in {
    val concurrency = 200

    val program =
      for
        stateRef <- Ref.of[IO, AppState](AppState.initial)
        pipeline <- newPipeline(stateRef)
        before   <- stateRef.get
        _        <- List.fill(concurrency)(pipeline.applyEvent(NewTab)).parSequence_
        after    <- stateRef.get
      yield after.persisted.buffers.size - before.persisted.buffers.size

    program.unsafeRunSync() shouldBe concurrency
  }

  /** The issue calls out that a naive `Semaphore[IO](1)` wrapped around all of `applyEvent` self-deadlocks:
    * `drainPendingOperations` recursively calls `applyEvent` again, on the same fiber, for operations enqueued while
    * interpreting an effect (`StateManagerOperationBoundary.enqueueEvent`). `FileSearch` is a convenient trigger
    * because `AppEventReducer` gives it a real effect (`AppEffect.Surface(SurfaceEffect.OpenFileSearch)`) without any
    * other setup; the effect handler below turns straight around and enqueues a `NewTab`, forcing exactly that
    * recursive replay. A correct fix must serialize concurrent *top-level* calls without blocking this same-fiber
    * recursive one, so this both completes (no self-deadlock) and actually replays the enqueued event.
    */
  it should "not self-deadlock when an interpreted effect enqueues a follow-up event for drainPendingOperations (#1570)" in {
    val program =
      for
        stateRef <- Ref.of[IO, AppState](AppState.initial)
        pipeline <- newPipeline(stateRef, onEffect = (operations, _) => operations.enqueueEvent(NewTab))
        before   <- stateRef.get
        _        <- pipeline.applyEvent(FileSearch)
        after    <- stateRef.get
      yield after.persisted.buffers.size - before.persisted.buffers.size

    program.timeout(10.seconds).unsafeRunSync() shouldBe 1
  }
