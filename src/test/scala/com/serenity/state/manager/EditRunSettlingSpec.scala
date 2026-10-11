package com.serenity.state.manager

import java.nio.file.Path
import java.util.concurrent.ConcurrentLinkedQueue

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Random

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.StateManagerTestFacade.{createBuffer, updateState}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, LspQueueEffect}
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.ViewportSize
import com.serenity.{TestTemp, setBufferForPane, setCursorPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** A batch's deletions, line breaks and one-character caret steps settle once per run, like its typed characters do
  * (#1879): centring, the language-server sync, the preview bump and the lane resubmissions all follow the run's final
  * state, which must be the state applying the same events one at a time reaches.
  */
class EditRunSettlingSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = NoOpFactory[IO]

  private val words =
    "lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore".split(" ")

  private def proseDocument: String =
    val random = new Random(1879)
    Vector
      .fill(120)(Vector.fill(40 + random.nextInt(80))(words(random.nextInt(words.length))).mkString(" ") + ".")
      .mkString("\n\n")

  private def editor(config: AppConfig): IO[(StateManager, BufferId)] =
    for
      stateManager <- StateManager(
        NoOpLogger[IO],
        sessionRootOverride = Some(TestTemp.directory("edit-run")),
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

  private val oneSlice: EventBatchSteps[Event] = EventBatchSteps((_, event) => event, _ => false, 1.hour)

  private def typed(count: Int, seed: Long): List[Event] =
    val random = new Random(seed)
    List.fill(count)(InsertChar(if random.nextInt(6) == 0 then ' ' else ('a' + random.nextInt(26)).toChar))

  final private case class Run(commits: Int, folded: Int)

  /** Counts the runs a batch settles and the events they absorbed. Each settle is one centring, one language-server
    * sync, one preview bump and one lane resubmission, since `commitTypedRun` is where a run does all four.
    */
  private def settledRuns(stateManager: StateManager, events: List[Event]): IO[Run] =
    val folded   = new ConcurrentLinkedQueue[Event]()
    val commits  = new ConcurrentLinkedQueue[Unit]()
    val pipeline = stateManager.composition.events
    val recording = TypedRuns(
      (event, model, nowNanos) =>
        pipeline.typedRunStep(event, model, nowNanos).map { step =>
          folded.add(event); step
        },
      (start, step) => IO(commits.add(())) >> pipeline.commitTypedRun(start, step),
      TypedRuns.MaxKeys
    )
    pipeline
      .dispatch(
        EventBatch.applying(events, oneSlice, stateManager.getModel, pipeline.applyEventOnDispatcher, recording).void
      )
      .map(_ => Run(commits.size, folded.asScala.size))

  private def shown(stateManager: StateManager, bufferId: BufferId) =
    stateManager.getCurrentState.map(
      _.persisted.buffers
        .get(bufferId)
        .map(buffer => (buffer.document.content.toString, buffer.editing, buffer.viewport))
    )

  private def undoDepth(stateManager: StateManager): IO[Int] =
    stateManager.getModel.map(_.undo.undoStack.size)

  private def matchesPerEvent(config: AppConfig, slices: List[List[Event]], after: List[Event] = Nil): Unit =
    val program = for
      (batched, bufferId) <- editor(config)
      (perEvent, _)       <- editor(config)
      differ <- slices.zipWithIndex.traverse { (slice, index) =>
        for
          _             <- batched.applyEventBatch(slice, oneSlice)
          _             <- slice.traverse_(perEvent.applyEvent)
          expected      <- shown(perEvent, bufferId)
          actual        <- shown(batched, bufferId)
          batchedDepth  <- undoDepth(batched)
          perEventDepth <- undoDepth(perEvent)
        yield Option.when(actual != expected || batchedDepth != perEventDepth)(s"slice $index (${slice.size} events)")
      }
      afterDiffer <- after.zipWithIndex.traverse { (event, index) =>
        for
          _        <- batched.applyEvent(event)
          _        <- perEvent.applyEvent(event)
          expected <- shown(perEvent, bufferId)
          actual   <- shown(batched, bufferId)
        yield Option.when(actual != expected)(s"$event #$index after the slices")
      }
    yield (differ ++ afterDiffer).flatten
    program.unsafeRunSync() shouldBe empty

  private val wrapped    = AppConfig.default.withWordWrap(true)
  private val typewriter = wrapped.withSurfaceConfig(wrapped.surfaceConfig.copy(typewriterScrollingEnabled = true))
  private val unwrapped  = AppConfig.default.withWordWrap(false)

  private val heldBackspace: List[Event] = List.fill[Event](60)(DeleteBackward)

  private val editing: List[Event] =
    typed(12, 1L) ++ List[Event](Enter) ++ typed(8, 2L) ++ List.fill[Event](5)(DeleteBackward) ++ List[Event](
      NewLine,
      DeleteForward
    ) ++
      List.fill[Event](4)(MoveLeft) ++ typed(3, 3L) ++ List.fill[Event](2)(MoveRight) ++ List[Event](
        DeleteWordBackward,
        DeleteToLineEnd
      )

  "A batch of deletions" should "settle as one run" in {
    val run = (for
      (stateManager, _) <- editor(wrapped)
      run               <- settledRuns(stateManager, heldBackspace)
    yield run).unsafeRunSync()
    run shouldBe Run(commits = 1, folded = heldBackspace.size)
  }

  "A batch mixing typed keys, deletions, line breaks and caret steps" should "settle as one run" in {
    val run = (for
      (stateManager, _) <- editor(wrapped)
      run               <- settledRuns(stateManager, editing)
    yield run).unsafeRunSync()
    run shouldBe Run(commits = 1, folded = editing.size)
  }

  it should "settle at a viewport-reading event and again after it, so that event sees the cursor centred" in {
    val events = typed(10, 4L) ++ List[Event](PageDown) ++ List.fill[Event](10)(DeleteBackward) ++ List[Event](
      MoveToEnd
    ) ++ typed(5, 5L)
    val run = (for
      (stateManager, _) <- editor(wrapped)
      run               <- settledRuns(stateManager, events)
    yield run).unsafeRunSync()
    run shouldBe Run(commits = 3, folded = events.size - 2)
  }

  it should "leave every slice as applying its events one at a time would, undo steps included" in
    List(wrapped, typewriter, unwrapped).foreach(
      matchesPerEvent(
        _,
        List(
          heldBackspace,
          editing,
          typed(30, 6L) ++ List.fill[Event](30)(DeleteBackward) ++ List[Event](Enter, Enter)
        ),
        after = List.fill[Event](8)(Undo) ++ List.fill[Event](5)(Redo) ++ List.fill[Event](3)(Undo)
      )
    )

  it should "give viewport-reading events the same results as unbatched dispatch" in {
    val reading: List[Event] =
      typed(15, 7L) ++ List[Event](PageUp) ++ List.fill[Event](6)(DeleteBackward) ++ List[Event](
        PageDown,
        MoveToStart
      ) ++
        List.fill[Event](3)(MoveRight) ++ List[Event](MoveToEnd, MoveUp, MoveUp, MoveDown) ++ typed(4, 8L) ++
        List[Event](ColumnLeft, ColumnRight, ExtendSelectionPageDown) ++ List.fill[Event](4)(DeleteBackward)
    List(wrapped, typewriter).foreach(matchesPerEvent(_, List(reading), after = List.fill[Event](6)(Undo)))
  }

  "The language-server sync after a run" should "send one change carrying the run's final text" in {
    val path     = Path.of("/workspace/Foo.scala")
    val bufferId = BufferId(0)
    def stateWith(content: String): AppState =
      val document = Document(Rope(content), filePath = Some(path), language = Some(LanguageId.Scala))
      AppState.initial.copy(persisted =
        AppState.initial.persisted.copy(buffers = Map(bufferId -> Buffer(bufferId, document)))
      )
    val before = stateWith("val answer = 41\nval other = 1\n")
    val after  = stateWith("val answer = 42\nval other = 1\nval added = 2\n")
    val effects = (for
      sent <- Ref.of[IO, List[AppEffect]](Nil)
      port = LspDocumentSyncPort(
        currentState = IO.pure(after),
        interpretEffect = effect => sent.update(_ :+ effect),
        candidateLspBufferIds = (_, _) => Set(bufferId)
      )
      _       <- new LspDocumentSync(port).enqueueChangedLspDocuments(before)
      effects <- sent.get
    yield effects).unsafeRunSync()
    effects.collect {
      case AppEffect.LspQueue(LspQueueEffect.DocumentChanged(_, _, text, _)) =>
        text.collect()
    } shouldBe List("val answer = 42\nval other = 1\nval added = 2\n")
  }
