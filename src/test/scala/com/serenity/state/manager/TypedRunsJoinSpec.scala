package com.serenity.state.manager

import java.nio.file.Files
import java.util.concurrent.ConcurrentLinkedQueue

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Random

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{Event, InsertChar, Redo, ResizeEvent, Undo}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade.{createBuffer, updateState}
import com.serenity.state.models.*
import com.serenity.state.undo.HistoryEntry
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.ViewportSize
import com.serenity.{setBufferForPane, setCursorPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** A typed key that records an undo step still joins the run under way (#1985): the snapshot it records holds a
  * viewport waiting for the caret rather than the run's uncentred one, and undo or redo places it again on restore.
  */
class TypedRunsJoinSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = NoOpFactory[IO]

  private val words =
    "lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore".split(" ")

  private def proseDocument: String =
    val random = new Random(1985)
    Vector
      .fill(120)(Vector.fill(40 + random.nextInt(80))(words(random.nextInt(words.length))).mkString(" ") + ".")
      .mkString("\n\n")

  private val smartPunctuation = AppConfig.default.withWordWrap(true).withSmartPunctuation(true)

  private def editor: IO[(StateManager, BufferId)] = editor(smartPunctuation)

  private def editor(config: AppConfig): IO[(StateManager, BufferId)] =
    for
      stateManager <- StateManager(
        NoOpLogger[IO],
        sessionRootOverride = Some(Files.createTempDirectory("typed-run-join")),
        dictionaryCache = SharedDictionary.default
      )
      _        <- stateManager.updateState(state => state.copy(persisted = state.persisted.copy(config = config)))
      bufferId <- stateManager.createBuffer(proseDocument, None)
      state    <- stateManager.getCurrentState
      paneId = state.persisted.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)
      _ <- stateManager.applyEvent(ResizeEvent(ViewportSize(110, 36)))
      _ <- stateManager.setCursorPosition(paneId, 60, 30)
      _ <- stateManager.applyEvent(InsertChar('w'))
    yield (stateManager, bufferId)

  /** Each quote is substituted for a curly one, a step undo records on its own, in the middle of the run. */
  private val keys: List[Event] = "he said \"so long\" and 'left' at once".toList.map(InsertChar(_))

  private val oneSlice: EventBatchSteps[Event] = EventBatchSteps((_, event) => event, _ => false, 1.hour)

  final private case class Run(commits: Int, models: Vector[Model])

  private def typedAsOneBatch(stateManager: StateManager): IO[Run] =
    val accepted = new ConcurrentLinkedQueue[Model]()
    val commits  = new ConcurrentLinkedQueue[Unit]()
    val events   = stateManager.composition.events
    val recording = TypedRuns(
      (event, model, nowNanos) =>
        events.typedRunStep(event, model, nowNanos).map { typed =>
          accepted.add(typed); typed
        },
      (start, typed) => IO(commits.add(())) >> events.commitTypedRun(start, typed),
      TypedRuns.MaxKeys
    )
    events
      .dispatch(
        EventBatch.applying(keys, oneSlice, stateManager.getModel, events.applyEventOnDispatcher, recording).void
      )
      .map(_ => Run(commits.size, accepted.asScala.toVector))

  private def centred(state: AppState, bufferId: BufferId): Option[Boolean] =
    state.persisted.buffers.get(bufferId).flatMap { buffer =>
      buffer.editing.cursorPositions.headOption.map(cursor =>
        CursorViewport.adjustForCursor(buffer, state, cursor).copy(placement = ViewportPlacement.Placed) ==
          buffer.viewport
      )
    }

  "A typed run" should "take a key that records an undo step rather than settle before it" in {
    val run = (for
      (stateManager, _) <- editor
      run               <- typedAsOneBatch(stateManager)
    yield run).unsafeRunSync()

    run.models.size shouldBe keys.size
    run.commits shouldBe 1
  }

  it should "leave the snapshot such a key records holding a viewport that waits for the caret" in {
    val program = for
      (stateManager, bufferId) <- editor
      before                   <- stateManager.getModel
      run                      <- typedAsOneBatch(stateManager)
      model                    <- stateManager.getModel
    yield (before, run, model, bufferId)
    val (before, run, model, bufferId) = program.unsafeRunSync()

    val alreadyRecorded = before.undo.undoStack
    val recordedMidRun = run.models.lastOption.toVector
      .flatMap(_.undo.undoStack)
      .filterNot(alreadyRecorded.contains)
      .collect { case edit: HistoryEntry.BufferEdit => edit }
    recordedMidRun should not be empty
    recordedMidRun.map(_.snapshot.viewport.placement).distinct shouldBe Vector(ViewportPlacement.FollowCaret)
    model.app.persisted.buffers.get(bufferId).map(_.viewport.placement) shouldBe Some(ViewportPlacement.Placed)
  }

  it should "commit a centred, placed viewport once it settles" in {
    val program = for
      (stateManager, bufferId) <- editor
      _                        <- typedAsOneBatch(stateManager)
      state                    <- stateManager.getCurrentState
    yield centred(state, bufferId)

    program.unsafeRunSync() shouldBe Some(true)
  }

  "Undo and redo" should "commit a placed viewport centred on the restored cursor, whichever step was restored" in {
    val program = for
      (stateManager, bufferId) <- editor
      _                        <- typedAsOneBatch(stateManager)
      undone <- List.fill(8)(Undo).traverse { undo =>
        stateManager.applyEvent(undo) >> stateManager.getCurrentState.map(centred(_, bufferId))
      }
      redone <- List.fill(8)(Redo).traverse { redo =>
        stateManager.applyEvent(redo) >> stateManager.getCurrentState.map(centred(_, bufferId))
      }
      state <- stateManager.getCurrentState
    yield (undone ++ redone, state.persisted.buffers.values.map(_.viewport.placement).toSet)

    val (centredAfterEach, placements) = program.unsafeRunSync()
    centredAfterEach.distinct shouldBe List(Some(true))
    placements shouldBe Set(ViewportPlacement.Placed)
  }

  "Undo and redo in column mode" should "reach the viewports that dispatching the keys one by one does" in {
    val config = smartPunctuation.withColumnMode(true)
    def shown(stateManager: StateManager, bufferId: BufferId) =
      stateManager.getCurrentState.map(
        _.persisted.buffers
          .get(bufferId)
          .map(buffer => (buffer.document.content.toString, buffer.editing, buffer.viewport))
      )
    val program = for
      (batched, bufferId) <- editor(config)
      (perKey, _)         <- editor(config)
      _                   <- typedAsOneBatch(batched)
      _                   <- keys.traverse_(perKey.applyEvent)
      differ <- (List.fill(8)(Undo) ++ List.fill(8)(Redo)).zipWithIndex.traverse { (event, index) =>
        for
          _        <- batched.applyEvent(event)
          _        <- perKey.applyEvent(event)
          expected <- shown(perKey, bufferId)
          actual   <- shown(batched, bufferId)
        yield Option.when(actual != expected)(s"$event #$index")
      }
    yield differ.flatten
    program.unsafeRunSync() shouldBe empty
  }
