package com.serenity

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.{AppRuntime, AppRuntimeRenderLoops}
import com.serenity.config.AppConfig
import com.serenity.config.AppConfigOps.*
import com.serenity.diagnostics.FrameTimings
import com.serenity.input.{InputRouter, PendingInput, SystemClipboard}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.{AppState, Damage, Focus}
import com.serenity.testkit.SharedDictionary
import fs2.{Chunk, Stream}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** The input loop applies whatever queued up while the previous batch ran in one dispatch, and diffs it once. */
class AppRuntimeInputBatchSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val logger = LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeInputBatchSpec"))

  private def editorWithEmptyBuffer(config: AppConfig = AppConfig.default): IO[StateManager] =
    for
      stateManager <- StateManager(logger, initialConfig = config, dictionaryCache = SharedDictionary.cacheFor(config))
      bufferId     <- stateManager.createNewEmptyBuffer
      state        <- stateManager.getCurrentState
      _            <- stateManager.setBufferForPane(state.persisted.layout.editorPanes.keys.head, bufferId)
    yield stateManager

  private def focusedText(state: AppState): Option[String] =
    state.persisted.focus match
      case Focus.EditorPane(paneId) =>
        state.persisted.layout.editorPanes
          .get(paneId)
          .flatMap(_.bufferId)
          .flatMap(state.persisted.buffers.get)
          .map(_.document.content.toString)
      case _ => None

  private def runBatches(
    stateManager: StateManager,
    batches: List[List[PendingInput]],
    emitDamage: Damage => IO[Unit] = _ => IO.unit,
    clipboard: SystemClipboard[IO] = SystemClipboard[IO](readText = IO.pure(None), writeText = _ => IO.unit),
    logEvent: (Event, Focus) => IO[Unit] = (_, _) => IO.unit,
    frameTimings: FrameTimings = FrameTimings()
  ): IO[Unit] =
    for
      router          <- InputRouter.create[IO, Event](new TextEntryTranslator(AppConfig.default))
      cursorVisible   <- Ref.of[IO, Boolean](true)
      translatorCache <- Ref.of[IO, Option[AppRuntimeRenderLoops.FocusedTranslatorCacheEntry]](None)
      context = AppRuntimeRenderLoops.InputBatchContext(
        stateManager,
        router,
        clipboard,
        IO.unit,
        cursorVisible,
        emitDamage,
        translatorCache,
        frameTimings,
        logEvent = logEvent
      )
      _ <- Stream
        .emits(batches.map(Chunk.from))
        .through(AppRuntimeRenderLoops.inputBatchPhase(context))
        .compile
        .drain
    yield ()

  private def key(char: Char): PendingInput =
    PendingInput.Keystroke(KeyStrokeInfo(InputKey.Character, Some(char), Set.empty))

  "AppRuntime's input loop" should "apply every input of a batch, in order, translating keystrokes as it goes" in {
    val program = for
      stateManager <- editorWithEmptyBuffer()
      applied      <- Ref.of[IO, Vector[Event]](Vector.empty)
      _ <- runBatches(
        stateManager,
        List(List(key('a'), PendingInput.Ready(InsertChar('b')), PendingInput.Ready(MoveLeft), key('c'))),
        logEvent = (event, _) => applied.update(_ :+ event)
      )
      state  <- stateManager.getCurrentState
      events <- applied.get
    yield (focusedText(state), events)

    program.unsafeRunSync() shouldBe (Some("acb"), Vector(InsertChar('a'), InsertChar('b'), MoveLeft, InsertChar('c')))
  }

  it should "report a batch's damage once, keeping its commits from the commit observer" in {
    val program = for
      stateManager <- editorWithEmptyBuffer()
      observed     <- Ref.of[IO, Vector[Damage]](Vector.empty)
      emitted      <- Ref.of[IO, Vector[Damage]](Vector.empty)
      _ <- stateManager.runtimeLifecycle.observeCommits(
        AppRuntime.wakeRenderLoopOnCommit(damage => observed.update(_ :+ damage))
      )
      _ <- runBatches(
        stateManager,
        List(List(key('a'))),
        emitDamage = damage => emitted.update(_ :+ damage)
      )
      fromObserver <- observed.get
      fromInput    <- emitted.get
    yield (fromObserver, fromInput)

    val (fromObserver, fromInput) = program.unsafeRunSync()
    fromObserver shouldBe empty
    fromInput should have size 1
    fromInput.headOption should not be Some(Damage.Nothing)
  }

  it should "let the commit observer see background commits again once a batch is applied" in {
    val program = for
      stateManager <- editorWithEmptyBuffer()
      observed     <- Ref.of[IO, Int](0)
      _            <- stateManager.runtimeLifecycle.observeCommits((_, _) => observed.update(_ + 1))
      _            <- runBatches(stateManager, List(List(key('a'))))
      duringInput  <- observed.get
      _ <- stateManager.updateStateValidated(state =>
        state.copy(runtime = state.runtime.copy(chapterGhostsVisible = !state.runtime.chapterGhostsVisible))
      )
      afterward <- observed.get
    yield (duringInput, afterward)

    program.unsafeRunSync() shouldBe (0, 1)
  }

  it should "apply a clipboard event on its own, with its clipboard hooks, between the inputs around it" in {
    val clipboard = SystemClipboard[IO](readText = IO.pure(Some("X")), writeText = _ => IO.unit)
    val program = for
      stateManager <- editorWithEmptyBuffer()
      _ <- runBatches(stateManager, List(List(key('a'), PendingInput.Ready(Paste), key('b'))), clipboard = clipboard)
      state <- stateManager.getCurrentState
    yield focusedText(state)

    program.unsafeRunSync() shouldBe Some("aXb")
  }

  it should "translate a keystroke against the focus the events ahead of it in the batch left" in {
    val program = for
      stateManager <- editorWithEmptyBuffer()
      applied      <- Ref.of[IO, Vector[Event]](Vector.empty)
      _ <- runBatches(
        stateManager,
        List(List(PendingInput.Ready(OpenFind), key('x'))),
        logEvent = (event, _) => applied.update(_ :+ event)
      )
      events <- applied.get
    yield events

    val events = program.unsafeRunSync()
    events.headOption shouldBe Some(OpenFind)
    events.lift(1) shouldBe Some(ModalInsertChar('x'))
  }

  it should "split a batch that outlasts half a frame, publishing each slice's damage before applying the next" in {
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(config = AppState.initial.persisted.config.withRenderFpsTarget(com.serenity.config.RenderFpsTarget.Fps60))
    )
    val program = for
      timeline <- Ref.of[IO, Vector[String]](Vector.empty)
      stateManager = new com.serenity.state.manager.StateEngine:
        def getCurrentState: IO[AppState] = IO.pure(state)
        def getModel: IO[com.serenity.state.manager.Model] =
          IO.pure(com.serenity.state.manager.Model(state, com.serenity.state.undo.UndoState()))
        def updateStateValidated(update: AppState => AppState): IO[Unit] = IO.unit
        // Each event takes 4ms, so half of a 60fps frame (8.3ms) fits three of them.
        def applyEvent(event: Event): IO[Unit] =
          IO.sleep(4.millis) >> timeline.update(_ :+ event.toString)
      router          <- InputRouter.create[IO, Event](new TextEntryTranslator(AppConfig.default))
      cursorVisible   <- Ref.of[IO, Boolean](true)
      translatorCache <- Ref.of[IO, Option[AppRuntimeRenderLoops.FocusedTranslatorCacheEntry]](None)
      context = AppRuntimeRenderLoops.InputBatchContext(
        stateManager,
        router,
        SystemClipboard[IO](readText = IO.pure(None), writeText = _ => IO.unit),
        IO.unit,
        cursorVisible,
        _ => timeline.update(_ :+ "damage"),
        translatorCache,
        com.serenity.diagnostics.FrameTimings()
      )
      _ <- Stream
        .emit(Chunk.from("abcdef".toList.map(char => PendingInput.Ready(InsertChar(char)))))
        .through(AppRuntimeRenderLoops.inputBatchPhase(context))
        .compile
        .drain
      result <- timeline.get
    yield result

    com.serenity.testkit.VirtualTime.runVirtual(program) shouldBe Vector(
      "InsertChar(a)",
      "InsertChar(b)",
      "InsertChar(c)",
      "damage",
      "InsertChar(d)",
      "InsertChar(e)",
      "InsertChar(f)",
      "damage"
    )
  }

  private def traceArrivals(timings: FrameTimings, keys: Int): Unit =
    (1 to keys).foreach { _ =>
      timings.keyLatency.keyReceived(System.currentTimeMillis())
      timings.keyLatency.keyEnqueued()
    }
    timings.keyLatency.keysDequeued(keys)

  it should "stamp every keystroke a batch applies, and its damage, onto the latency trace" in {
    val clipboard = SystemClipboard[IO](readText = IO.pure(Some("X")), writeText = _ => IO.unit)
    val timings   = FrameTimings()
    timings.keyLatency.setEnabled(true)
    traceArrivals(timings, keys = 3)

    val program = for
      stateManager <- editorWithEmptyBuffer()
      _ <- runBatches(
        stateManager,
        List(List(key('a'), PendingInput.Ready(Paste), key('b')), List(PendingInput.Ready(MoveLeft), key('c'))),
        clipboard = clipboard,
        frameTimings = timings
      )
    yield ()
    program.unsafeRunSync()

    timings.keyLatency.pendingKeys.map(key => (key.appliedAt.isDefined, key.damagedAt.isDefined)) shouldBe
      Vector.fill(3)((true, true))
  }

  it should "leave the latency trace untouched while it is off" in {
    val timings = FrameTimings()
    traceArrivals(timings, keys = 1)

    val program = for
      stateManager <- editorWithEmptyBuffer()
      _            <- runBatches(stateManager, List(List(key('a'))), frameTimings = timings)
      state        <- stateManager.getCurrentState
    yield focusedText(state)

    program.unsafeRunSync() shouldBe Some("a")
    timings.keyLatency.pendingKeys shouldBe empty
  }
