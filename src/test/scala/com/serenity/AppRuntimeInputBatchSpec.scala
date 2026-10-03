package com.serenity

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.{AppRuntime, AppRuntimeRenderLoops}
import com.serenity.config.AppConfig
import com.serenity.input.{InputRouter, PendingInput, SystemClipboard}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.{AppState, Damage, Focus}
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
      stateManager <- StateManager(logger, initialConfig = config)
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
    logEvent: (Event, Focus) => IO[Unit] = (_, _) => IO.unit
  ): IO[Unit] =
    for
      router          <- InputRouter.create[IO, Event](new TextEntryTranslator(AppConfig.default))
      cursorVisible   <- Ref.of[IO, Boolean](true)
      breathIndex     <- Ref.of[IO, Int](0)
      translatorCache <- Ref.of[IO, Option[AppRuntimeRenderLoops.FocusedTranslatorCacheEntry]](None)
      context = AppRuntimeRenderLoops.InputBatchContext(
        stateManager,
        router,
        clipboard,
        IO.unit,
        cursorVisible,
        breathIndex,
        emitDamage,
        translatorCache,
        com.serenity.diagnostics.FrameTimings(),
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
    val enabledSprite =
      AppConfig.default.withCompanionSpriteConfig(AppConfig.default.companionSpriteConfig.copy(enabled = true))
    val program = for
      stateManager <- editorWithEmptyBuffer(enabledSprite)
      observed     <- Ref.of[IO, Vector[Damage]](Vector.empty)
      emitted      <- Ref.of[IO, Vector[Damage]](Vector.empty)
      _ <- stateManager.runtimeLifecycle.observeCommits(
        AppRuntime.wakeRenderLoopOnCommit(damage => observed.update(_ :+ damage))
      )
      _ <- runBatches(
        stateManager,
        List(List(key('a'), key('b'))),
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

  it should "still wake the companion sprite for typing applied in a batch" in {
    val enabledSprite =
      AppConfig.default.withCompanionSpriteConfig(AppConfig.default.companionSpriteConfig.copy(enabled = true))
    val program = for
      stateManager <- editorWithEmptyBuffer(enabledSprite)
      _            <- runBatches(stateManager, List(List(key('a'), key('b'))))
      state        <- stateManager.getCurrentState
    yield state.runtime.companionSprite.isTypingActive

    program.unsafeRunSync() shouldBe true
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
