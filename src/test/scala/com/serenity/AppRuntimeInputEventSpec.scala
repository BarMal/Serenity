package com.serenity

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.app.AppRuntime
import com.serenity.config.*
import com.serenity.input.{InputHandler, InputRouter, SystemClipboard}
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.{TextEntryTranslator, Translator}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, BufferId, Damage}
import com.serenity.ui.layout.ViewportSize
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

class AppRuntimeInputEventSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def waitForStartupSelection(
    stateManager: StateManager,
    expected: Int,
    attempts: Int
  ): IO[Boolean] =
    stateManager.getCurrentState.flatMap { state =>
      val selection = state.startPageSurface.flatMap {
        _.content match
          case com.serenity.state.models.SurfaceContent.StartPage(page) => Some(page.selectedIndex)
          case _                                                        => None
      }
      if selection.contains(expected) then IO.pure(true)
      else if attempts <= 0 then IO.pure(false)
      else IO.sleep(25.millis) >> waitForStartupSelection(stateManager, expected, attempts - 1)
    }

  "AppRuntime" should "reset the cursor activity phase to visible after user input" in {
    val result = (for
      cursorVisible <- Ref.of[IO, Boolean](false)
      breathIndex   <- Ref.of[IO, Int](17)
      _             <- AppRuntime.resetCursorActivity(cursorVisible, breathIndex)
      visible       <- cursorVisible.get
      breathe       <- breathIndex.get
    yield (visible, breathe)).unsafeRunSync()

    result shouldBe (true, 0)
  }

  it should "refresh the focused translator after every input event" in {
    val program = for
      refreshes     <- Ref.of[IO, Int](0)
      resizeChecks  <- Ref.of[IO, Int](0)
      cursorVisible <- Ref.of[IO, Boolean](true)
      breathIndex   <- Ref.of[IO, Int](0)
      router = new InputRouter[IO, Event]:
        private val initialTranslator = new TextEntryTranslator(AppConfig.default)

        def eventStream(infoStream: Stream[IO, KeyStrokeInfo]): Stream[IO, Event] = Stream.empty
        def setActiveTranslator(translator: Translator[Event]): IO[Unit]          = refreshes.update(_ + 1)
        def getActiveTranslator: IO[Translator[Event]]                            = IO.pure(initialTranslator)
      stateManager = new com.serenity.state.manager.StateEngine:
        def getCurrentState: IO[AppState]                                                 = IO.pure(AppState.initial)
        def getBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]] = IO.pure(Map.empty)
        def updateState(update: AppState => AppState): IO[Unit]                           = IO.unit
        def updateBufferAnimations(
          update: Map[BufferId, com.serenity.animation.AnimationState] => Map[
            BufferId,
            com.serenity.animation.AnimationState
          ]
        ): IO[Unit] = IO.unit
        def applyEvent(event: Event): IO[Unit] = IO.unit
      clipboard = SystemClipboard[IO](readText = IO.pure(None), writeText = _ => IO.unit)
      _ <- AppRuntime
        .inputEventPhase(
          stateManager,
          router,
          clipboard,
          resizeChecks.update(_ + 1),
          cursorVisible,
          breathIndex,
          (_: Damage) => IO.unit
        )(
          Stream.emits(List(InsertChar('a'), DeleteBackward, MoveLeft, InsertChar('b')))
        )
        .compile
        .drain
      refreshCount <- refreshes.get
      resizeCount  <- resizeChecks.get
    yield (refreshCount, resizeCount)

    program.unsafeRunSync() shouldBe (4, 0)
  }

  it should "check for a resize before applying pointer input" in {
    val program = for
      resizeChecks  <- Ref.of[IO, Int](0)
      cursorVisible <- Ref.of[IO, Boolean](true)
      breathIndex   <- Ref.of[IO, Int](0)
      router = new InputRouter[IO, Event]:
        private val initialTranslator = new TextEntryTranslator(AppConfig.default)

        def eventStream(infoStream: Stream[IO, KeyStrokeInfo]): Stream[IO, Event] = Stream.empty
        def setActiveTranslator(translator: Translator[Event]): IO[Unit]          = IO.unit
        def getActiveTranslator: IO[Translator[Event]]                            = IO.pure(initialTranslator)
      stateManager = new com.serenity.state.manager.StateEngine:
        def getCurrentState: IO[AppState]                                                 = IO.pure(AppState.initial)
        def getBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]] = IO.pure(Map.empty)
        def updateState(update: AppState => AppState): IO[Unit]                           = IO.unit
        def updateBufferAnimations(
          update: Map[BufferId, com.serenity.animation.AnimationState] => Map[
            BufferId,
            com.serenity.animation.AnimationState
          ]
        ): IO[Unit] = IO.unit
        def applyEvent(event: Event): IO[Unit] = IO.unit
      clipboard = SystemClipboard[IO](readText = IO.pure(None), writeText = _ => IO.unit)
      _ <- AppRuntime
        .inputEventPhase(
          stateManager,
          router,
          clipboard,
          resizeChecks.update(_ + 1),
          cursorVisible,
          breathIndex,
          (_: Damage) => IO.unit
        )(Stream.emit(MousePress(0, 0)))
        .compile
        .drain
      count <- resizeChecks.get
    yield count

    program.unsafeRunSync() shouldBe 1
  }

  it should "process landing-page input while the initial frame is rendering" in {
    given Logger[IO] = LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeStartupInputSpec"))

    val program = for
      initialRenderStarted <- Deferred[IO, Unit]
      allowInitialRender   <- Deferred[IO, Unit]
      closeRequested       <- Deferred[IO, Unit]
      stateManager <- StateManager.apply(
        LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeStartupInputSpec")),
        policy = SessionManager.SessionPolicy(saveOnAppClose = false)
      )
      inputHandler = new InputHandler[IO]:
        override def keyStrokeInfoStream: Stream[IO, KeyStrokeInfo] = Stream.never
        override def eventStream: Stream[IO, Event]                 = Stream.emit(MoveDown) ++ Stream.never
        override def shutdown: IO[Unit]                             = IO.unit
      fiber <- AppRuntime
        .run(
          initialViewportSize = ViewportSize(120, 40),
          makeInputHandler = _ => IO.pure(inputHandler),
          checkResize = IO.pure(None),
          renderFull = (
            _: AppState,
            _: Boolean,
            _: Option[Color],
            _: Damage,
            _: Map[BufferId, com.serenity.animation.AnimationState]
          ) => initialRenderStarted.complete(()).flatMap(_ => allowInitialRender.get),
          renderCursorOnly = (
            _: AppState,
            _: Boolean,
            _: Option[Color],
            _: Damage,
            _: Map[BufferId, com.serenity.animation.AnimationState]
          ) => IO.unit,
          appConfig = AppConfig.default,
          makeStateManager = Some(_ => IO.pure(stateManager)),
          awaitExternalQuit = closeRequested.get,
          registerResizeCallback = _ => ()
        )
        .start
      _                           <- initialRenderStarted.get
      selectedDuringInitialRender <- waitForStartupSelection(stateManager, expected = 1, attempts = 20)
      _                           <- allowInitialRender.complete(())
      _                           <- closeRequested.complete(())
      _                           <- fiber.joinWithNever
    yield selectedDuringInitialRender

    program.unsafeRunTimed(10.seconds) shouldBe Some(true)
  }

  it should "cancel the input stream when the initial render fails" in {
    given Logger[IO] = LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeStartupFailureSpec"))

    val program = for
      inputStarted   <- Deferred[IO, Unit]
      inputCancelled <- Deferred[IO, Unit]
      stateManager <- StateManager.apply(
        LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeStartupFailureSpec")),
        policy = SessionManager.SessionPolicy(saveOnAppClose = false)
      )
      inputHandler = new InputHandler[IO]:
        override def keyStrokeInfoStream: Stream[IO, KeyStrokeInfo] = Stream.never
        override def eventStream: Stream[IO, Event] =
          (Stream.eval(inputStarted.complete(()).map(_ => ())).drain ++ Stream
            .repeatEval(IO.never[Event]))
            .onFinalize(inputCancelled.complete(()).map(_ => ()))
        override def shutdown: IO[Unit] = IO.unit
      result <- AppRuntime
        .run(
          initialViewportSize = ViewportSize(120, 40),
          makeInputHandler = _ => IO.pure(inputHandler),
          checkResize = IO.pure(None),
          renderFull = (
            _: AppState,
            _: Boolean,
            _: Option[Color],
            _: Damage,
            _: Map[BufferId, com.serenity.animation.AnimationState]
          ) => inputStarted.get >> IO.raiseError(RuntimeException("initial render failed")),
          renderCursorOnly = (
            _: AppState,
            _: Boolean,
            _: Option[Color],
            _: Damage,
            _: Map[BufferId, com.serenity.animation.AnimationState]
          ) => IO.unit,
          appConfig = AppConfig.default,
          makeStateManager = Some(_ => IO.pure(stateManager)),
          registerResizeCallback = _ => ()
        )
        .attempt
      cancelled <- IO.race(inputCancelled.get, IO.sleep(5.seconds)).map(_.isLeft)
    yield (result, cancelled)

    val (result, cancelled) = program.unsafeRunTimed(10.seconds).getOrElse(fail("Runtime did not finish"))
    result.swap.toOption.map(_.getMessage) should contain("initial render failed")
    cancelled shouldBe true
  }

  it should "refresh the focused translator after a modal request" in {
    val program = for
      refreshes     <- Ref.of[IO, Int](0)
      cursorVisible <- Ref.of[IO, Boolean](true)
      breathIndex   <- Ref.of[IO, Int](0)
      router = new InputRouter[IO, Event]:
        private val initialTranslator = new TextEntryTranslator(AppConfig.default)

        def eventStream(infoStream: Stream[IO, KeyStrokeInfo]): Stream[IO, Event] = Stream.empty
        def setActiveTranslator(translator: Translator[Event]): IO[Unit]          = refreshes.update(_ + 1)
        def getActiveTranslator: IO[Translator[Event]]                            = IO.pure(initialTranslator)
      stateManager = new com.serenity.state.manager.StateEngine:
        def getCurrentState: IO[AppState]                                                 = IO.pure(AppState.initial)
        def getBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]] = IO.pure(Map.empty)
        def updateState(update: AppState => AppState): IO[Unit]                           = IO.unit
        def updateBufferAnimations(
          update: Map[BufferId, com.serenity.animation.AnimationState] => Map[
            BufferId,
            com.serenity.animation.AnimationState
          ]
        ): IO[Unit] = IO.unit
        def applyEvent(event: Event): IO[Unit] = IO.unit
      clipboard = SystemClipboard[IO](readText = IO.pure(None), writeText = _ => IO.unit)
      _ <- AppRuntime
        .inputEventPhase(stateManager, router, clipboard, IO.unit, cursorVisible, breathIndex, (_: Damage) => IO.unit)(
          Stream.emit(OpenFind)
        )
        .compile
        .drain
      count <- refreshes.get
    yield count

    program.unsafeRunSync() shouldBe 1
  }
