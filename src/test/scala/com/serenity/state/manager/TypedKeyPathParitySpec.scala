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
import com.serenity.keystroke.events.{Event, ExtendSelectionRight, InsertChar, OpenGotoLine, ResizeEvent}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade.{createBuffer, updateState}
import com.serenity.state.models.*
import com.serenity.state.undo.{HistoryEntry, UndoState}
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.ViewportSize
import com.serenity.{setBufferForPane, setCursorPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** A typed key reaches the state the same way whether it is dispatched on its own or folded into a batch's typed run
  * (#1985): both go through one step, and the run only defers centring and the commit to its end.
  */
class TypedKeyPathParitySpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = NoOpFactory[IO]

  private val words = "lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod".split(" ")

  private def document: String =
    val random = new Random(1879)
    Vector
      .fill(40)(Vector.fill(30 + random.nextInt(60))(words(random.nextInt(words.length))).mkString(" ") + ".")
      .mkString("\n\n")

  private val keys: List[Event] = "ab \"cd' -(e)f".toList.map(InsertChar(_))

  final private case class Scenario(name: String, config: AppConfig, setUp: (StateManager, BufferId) => IO[Unit])

  private val wrapped = AppConfig.default.withWordWrap(true)

  private def twoCursors(stateManager: StateManager, bufferId: BufferId): IO[Unit] =
    stateManager.updateState(state =>
      state.persisted.buffers.get(bufferId).fold(state) { buffer =>
        val cursors = List(CursorPosition(10, 5), CursorPosition(14, 7))
        state.copy(persisted =
          state.persisted
            .copy(buffers = state.persisted.buffers.updated(bufferId, buffer.copy(editing = EditingState(cursors))))
        )
      }
    )

  private val scenarios = List(
    Scenario("word wrap", wrapped, (_, _) => IO.unit),
    Scenario("no wrap", AppConfig.default.withWordWrap(false), (_, _) => IO.unit),
    Scenario("typewriter", wrapped.withTypewriterScrolling(true), (_, _) => IO.unit),
    Scenario("smart punctuation", wrapped.withSmartPunctuation(true), (_, _) => IO.unit),
    Scenario(
      "selection",
      wrapped,
      (stateManager, _) => List.fill(4)(ExtendSelectionRight).traverse_(stateManager.applyEvent)
    ),
    Scenario("two cursors", wrapped, twoCursors),
    Scenario("blocking modal", wrapped, (stateManager, _) => stateManager.applyEvent(OpenGotoLine))
  )

  private def editor(scenario: Scenario): IO[(StateManager, BufferId)] =
    for
      stateManager <- StateManager(
        NoOpLogger[IO],
        sessionRootOverride = Some(Files.createTempDirectory("parity")),
        dictionaryCache = SharedDictionary.default
      )
      _ <- stateManager.updateState(state => state.copy(persisted = state.persisted.copy(config = scenario.config)))
      bufferId <- stateManager.createBuffer(document, None)
      state    <- stateManager.getCurrentState
      paneId = state.persisted.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)
      _ <- stateManager.applyEvent(ResizeEvent(ViewportSize(100, 30)))
      _ <- stateManager.setCursorPosition(paneId, 12, 20)
      _ <- scenario.setUp(stateManager, bufferId)
    yield (stateManager, bufferId)

  /** Everything a key's dispatch commits but the clock-stamped runtime, with or without the buffers' viewports. */
  private def observed(model: Model, withViewports: Boolean): (Persisted, UndoState) =
    val persisted = model.app.persisted
    val buffers =
      if withViewports then persisted.buffers
      else persisted.buffers.view.mapValues(_.copy(viewport = Viewport.default)).toMap
    (persisted.copy(buffers = buffers), if withViewports then model.undo else withSnapshotsPlaced(model))

  /** A snapshot a key records mid-run holds a viewport still waiting for the caret, where dispatching the keys one by
    * one records the centred one. Both are compared as undo would restore them: placed on the snapshot's cursor.
    */
  private def withSnapshotsPlaced(model: Model): UndoState =
    def placedEdit(edit: HistoryEntry.BufferEdit): HistoryEntry.BufferEdit =
      edit
        .restore(model.app)
        .map((restored, _) => ViewportResolution.resolve(restored))
        .flatMap(_.persisted.buffers.get(edit.bufferId))
        .fold(edit)(buffer => edit.copy(snapshot = edit.snapshot.copy(viewport = buffer.viewport)))
    def placed(entry: HistoryEntry): HistoryEntry = entry match
      case edit: HistoryEntry.BufferEdit => placedEdit(edit)
      case other                         => other
    model.undo.copy(
      undoStack = model.undo.undoStack.map(placed),
      redoStack = model.undo.redoStack.map(placed),
      pendingGroup = model.undo.pendingGroup.map(placedEdit)
    )

  private val oneSlice: EventBatchSteps[Event] = EventBatchSteps((_, event) => event, _ => false, 1.hour)

  "A typed key" should "commit the same state dispatched alone as in a one-key batch" in
    scenarios.foreach { scenario =>
      val program = for
        (alone, _)   <- editor(scenario)
        (batched, _) <- editor(scenario)
        differ <- keys.zipWithIndex.traverse { (key, index) =>
          for
            _        <- alone.applyEvent(key)
            _        <- batched.applyEventBatch(List(key), oneSlice)
            expected <- alone.getModel
            actual   <- batched.getModel
          yield Option.when(observed(actual, true) != observed(expected, true))(s"key $index")
        }
      yield differ.flatten
      withClue(s"${scenario.name}: ")(program.unsafeRunSync() shouldBe empty)
    }

  /** Runs `keys` as one batch whose typed-run steps are recorded as the batch accepts them. */
  private def foldedSteps(stateManager: StateManager): IO[Vector[Model]] =
    val accepted = new ConcurrentLinkedQueue[Model]()
    val events   = stateManager.composition.events
    val recording = TypedRuns(
      (event, model, nowNanos) =>
        events.typedRunStep(event, model, nowNanos).map { typed =>
          accepted.add(typed); typed
        },
      events.commitTypedRun,
      TypedRuns.MaxKeys
    )
    events
      .dispatch(
        EventBatch.applying(keys, oneSlice, stateManager.getModel, events.applyEventOnDispatcher, recording).void
      )
      .map(_ => accepted.asScala.toVector)

  it should "leave every intermediate state of a typed run as dispatching its keys one by one, but for the viewport" in
    scenarios.foreach { scenario =>
      val program = for
        (alone, bufferId) <- editor(scenario)
        (batched, _)      <- editor(scenario)
        perKey            <- keys.traverse(key => alone.applyEvent(key) >> alone.getModel)
        steps             <- foldedSteps(batched)
      yield
        def text(model: Model) = model.app.persisted.buffers.get(bufferId).map(_.document.content.toString)
        steps.zipWithIndex.flatMap { (step, index) =>
          perKey.find(state => text(state) == text(step)) match
            case None                                                                 => Some(s"step $index: no match")
            case Some(expected) if observed(step, false) != observed(expected, false) => Some(s"step $index")
            case Some(_)                                                              => None
        }
      withClue(s"${scenario.name}: ")(program.unsafeRunSync() shouldBe empty)
    }
