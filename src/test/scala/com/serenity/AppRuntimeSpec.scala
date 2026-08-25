package com.serenity

import java.awt.Color
import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.semigroup.*
import com.serenity.animation.{WindowSitter, WindowSitterConfig}
import com.serenity.app.AppRuntime
import com.serenity.config.*
import com.serenity.input.{InputHandler, InputRouter, SystemClipboard}
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.{TextEntryTranslator, Translator}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, BufferId, CursorPosition, Damage}
import com.serenity.ui.layout.ViewportSize
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

class AppRuntimeSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  "AppRuntime" should "keep fast rendering active while the window sitter is ticking" in {
    val state = AppState.initial.copy(windowSitter = WindowSitter.default.observeTyping(1_000_000_000L))

    AppRuntime.hasActiveAnimations(state) shouldBe true
  }

  it should "initialize the window sitter from the configured startup frames" in {
    val config = AppConfig.default.withWindowSitterConfig(
      WindowSitterConfig(frames = Vector("rest", "active"), activeTicks = 3)
    )

    AppState.initial(config).windowSitter shouldBe WindowSitter.fromConfig(config.windowSitterConfig)
  }

  it should "wake and settle the window sitter through a real typing and tick sequence" in {
    val logger       = LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeSpec"))
    val stateManager = StateManager(logger, initialConfig = AppConfig.default).unsafeRunSync()

    AppRuntime
      .observeWindowSitterTyping(InsertChar('a'), stateManager)
      .unsafeRunSync()

    val awakened = stateManager.getCurrentState.unsafeRunSync().windowSitter
    awakened.isActive shouldBe true
    awakened.glyph should not be WindowSitter.default.glyph

    Iterator
      .continually(stateManager.advanceAnimationsOnTick().unsafeRunSync())
      .takeWhile(identity)
      .toList

    stateManager.getCurrentState.unsafeRunSync().windowSitter.isActive shouldBe false
  }

  private class SilentInputHandler extends InputHandler[IO]:
    override def keyStrokeInfoStream: Stream[IO, KeyStrokeInfo] = Stream.never
    override def eventStream: Stream[IO, Event]                 = Stream.never
    override def shutdown: IO[Unit]                             = IO.unit

  final private case class LogEntry(level: String, message: String, error: Option[Throwable])

  private class RecordingLogger(ref: Ref[IO, Vector[LogEntry]]) extends Logger[IO]:
    private def record(level: String, message: String, error: Option[Throwable]): IO[Unit] =
      ref.update(_ :+ LogEntry(level, message, error))

    override def error(t: Throwable)(message: => String): IO[Unit] = record("error", message, Some(t))
    override def warn(t: Throwable)(message: => String): IO[Unit]  = record("warn", message, Some(t))
    override def info(t: Throwable)(message: => String): IO[Unit]  = record("info", message, Some(t))
    override def debug(t: Throwable)(message: => String): IO[Unit] = record("debug", message, Some(t))
    override def trace(t: Throwable)(message: => String): IO[Unit] = record("trace", message, Some(t))
    override def error(message: => String): IO[Unit]               = record("error", message, None)
    override def warn(message: => String): IO[Unit]                = record("warn", message, None)
    override def info(message: => String): IO[Unit]                = record("info", message, None)
    override def debug(message: => String): IO[Unit]               = record("debug", message, None)
    override def trace(message: => String): IO[Unit]               = record("trace", message, None)

  private def awaitLogEntry(
    logs: Ref[IO, Vector[LogEntry]],
    matches: LogEntry => Boolean,
    attempts: Int = 40
  ): IO[Option[LogEntry]] =
    def loop(remaining: Int): IO[Option[LogEntry]] =
      logs.get.flatMap { entries =>
        entries.find(matches) match
          case found @ Some(_) => IO.pure(found)
          case None if remaining <= 0 =>
            IO.pure(None)
          case None =>
            IO.sleep(25.millis) >> loop(remaining - 1)
      }
    loop(attempts)

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

  "AppRuntime" should "derive render frame intervals from the configured FPS target" in {
    AppRuntime.fastFrameInterval(RenderFpsTarget.Fps30).toNanos shouldBe 33333333L
    AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60).toNanos shouldBe 16666666L
    AppRuntime.fastFrameInterval(RenderFpsTarget.Fps90).toNanos shouldBe 11111111L
    AppRuntime.fastFrameInterval(RenderFpsTarget.Fps120).toNanos shouldBe 8333333L
    AppRuntime.fastFrameInterval(RenderFpsTarget.Uncapped).toNanos shouldBe 3333333L
  }

  it should "pace full renders at the configured frame interval when no animations are active" in {
    val frameInterval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60)

    AppRuntime.fastFrameDelay(frameInterval) shouldBe frameInterval
  }

  it should "render the first fast frame without waiting for the configured frame interval" in {
    val frameInterval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps30)

    AppRuntime.fastFrameDelay(frameInterval, isInitialFrame = true) shouldBe Duration.Zero
    AppRuntime.fastFrameDelay(frameInterval, isInitialFrame = false) shouldBe frameInterval
  }

  it should "render an input-requested first fast frame immediately, pace its follow-up, and defer its animation tick" in {
    val frameInterval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps30)
    val state = AppState.initial.copy(
      config = AppState.initial.config.copy(renderFpsTarget = RenderFpsTarget.Fps30),
      surfaceAnimations = Map(
        com.serenity.state.models.SurfaceId("fast-render-regression") -> com.serenity.state.models
          .SurfaceAnimationState()
      )
    )

    val program = for
      fastModeSignal       <- fs2.concurrent.SignallingRef.of[IO, Boolean](false)
      pendingDamage        <- Ref.of[IO, Damage](Damage.Nothing)
      animationTickCadence <- Ref.of[IO, AppRuntime.AnimationTickCadence](AppRuntime.AnimationTickCadence.empty)
      animationTicks       <- Ref.of[IO, Int](0)
      rendered             <- Ref.of[IO, Vector[Int]](Vector.empty)
      requestedDelays      <- Ref.of[IO, Vector[FiniteDuration]](Vector.empty)
      cursorVisible        <- Ref.of[IO, Boolean](true)
      breathIndex          <- Ref.of[IO, Int](0)
      stateManager = new com.serenity.state.manager.StateReader
        with com.serenity.state.manager.StateUpdater
        with com.serenity.state.manager.EventApplier
        with com.serenity.state.manager.AnimationTicker:
        def getCurrentState: IO[AppState]                       = IO.pure(state)
        def updateState(update: AppState => AppState): IO[Unit] = IO.unit
        def applyEvent(event: Event): IO[Unit]                  = IO.unit
        def advanceAnimationFrames(): IO[Unit]                  = IO.unit
        def advanceAnimationsOnTick(): IO[Boolean]              = animationTicks.updateAndGet(_ + 1).as(true)
      inputRouter = new InputRouter[IO, Event]:
        private val translator = new TextEntryTranslator(AppConfig.default)

        def eventStream(infoStream: Stream[IO, KeyStrokeInfo]): Stream[IO, Event] = Stream.empty
        def setActiveTranslator(translator: Translator[Event]): IO[Unit]          = IO.unit
        def getActiveTranslator: IO[Translator[Event]]                            = IO.pure(translator)
      clipboard = new SystemClipboard[IO]:
        def readText: IO[Option[String]]      = IO.pure(None)
        def writeText(text: String): IO[Unit] = IO.unit
      emitDamage: (Damage => IO[Unit]) = damage => pendingDamage.update(_ |+| damage) >> fastModeSignal.set(true)
      given Logger[IO]                 = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      _ <- AppRuntime
        .inputEventPhase(
          stateManager,
          inputRouter,
          clipboard,
          IO.unit,
          cursorVisible,
          breathIndex,
          emitDamage
        )(Stream.emit(InsertChar('a')))
        .compile
        .drain
      _ <- AppRuntime
        .fastRenderPhase(
          stateManager,
          fastModeSignal,
          pendingDamage,
          animationTickCadence,
          IO.pure(Some(state)),
          IO.unit,
          (_: AppState, _: Boolean, _: Option[Color]) =>
            animationTicks.get.flatMap(tickCount => rendered.update(_ :+ tickCount)),
          (_: AppState, _: Boolean, _: Option[Color]) =>
            IO.raiseError(new AssertionError("expected full render while a surface animation is active")),
          delay => requestedDelays.update(_ :+ delay)
        )
        .take(2)
        .compile
        .drain
      frames <- rendered.get
      delays <- requestedDelays.get
    yield
      frames should have size 2
      delays shouldBe Vector(Duration.Zero, frameInterval)
      frames shouldBe Vector(0, 1)

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "route fast-mode frames through the cursor-only render path when only the window sitter is active" in {
    val state = AppState.initial.copy(
      config = AppState.initial.config.copy(renderFpsTarget = RenderFpsTarget.Fps30),
      windowSitter = WindowSitter.default.observeTyping(1_000_000_000L)
    )
    state.windowSitter.isActive shouldBe true
    AppRuntime.needsFullContentRender(state) shouldBe false

    val program = for
      fastModeSignal       <- fs2.concurrent.SignallingRef.of[IO, Boolean](false)
      pendingDamage        <- Ref.of[IO, Damage](Damage.Nothing)
      animationTickCadence <- Ref.of[IO, AppRuntime.AnimationTickCadence](AppRuntime.AnimationTickCadence.empty)
      cursorOnlyFrames     <- Ref.of[IO, Int](0)
      stateManager = new com.serenity.state.manager.StateReader
        with com.serenity.state.manager.StateUpdater
        with com.serenity.state.manager.EventApplier
        with com.serenity.state.manager.AnimationTicker:
        def getCurrentState: IO[AppState]                       = IO.pure(state)
        def updateState(update: AppState => AppState): IO[Unit] = IO.unit
        def applyEvent(event: Event): IO[Unit]                  = IO.unit
        def advanceAnimationFrames(): IO[Unit]                  = IO.unit
        def advanceAnimationsOnTick(): IO[Boolean]              = IO.pure(true)
      given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      _ <- AppRuntime
        .fastRenderPhase(
          stateManager,
          fastModeSignal,
          pendingDamage,
          animationTickCadence,
          IO.pure(Some(state)),
          IO.unit,
          (_: AppState, _: Boolean, _: Option[Color]) =>
            IO.raiseError(new AssertionError("expected cursor-only render while only the window sitter is active")),
          (_: AppState, _: Boolean, _: Option[Color]) => cursorOnlyFrames.update(_ + 1),
          _ => IO.unit
        )
        .take(2)
        .compile
        .drain
      frames <- cursorOnlyFrames.get
    yield frames shouldBe 2

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "advance animations once per frame at whatever render FPS is configured, not a fixed 60Hz rate" in
    List(RenderFpsTarget.Fps30, RenderFpsTarget.Fps60, RenderFpsTarget.Fps90, RenderFpsTarget.Fps120).foreach {
      target =>
        val interval       = AppRuntime.fastFrameInterval(target)
        val (after, ticks) = AppRuntime.AnimationTickCadence.empty.advance(interval)

        withClue(s"target=$target ") {
          ticks shouldBe 1
          after.remainderNanos shouldBe 0L
        }
    }

  it should "derive cursor idle cadence from the cursor motion speed scale" in {
    AppRuntime.cursorIdleInterval(AppConfig.default) shouldBe Some(500.millis)
    AppRuntime.cursorIdleInterval(AppConfig.default.withElementTransitionSpeedScale(2.0)) shouldBe Some(1000.millis)
    AppRuntime.cursorIdleInterval(
      AppConfig.default
        .withElementTransitionSpeedScale(2.0)
        .withCursorTransitionSpeedScale(Some(0.5))
    ) shouldBe Some(250.millis)
    AppRuntime.cursorIdleInterval(AppConfig.default.withCursorTransitionSpeedScale(Some(0.0))) shouldBe None
    AppRuntime.cursorIdleInterval(
      AppConfig.default.withMotionAccessibility(MotionAccessibility.Off)
    ) shouldBe None
  }

  it should "reset the cursor activity phase to visible after user input" in {
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
      stateManager = new com.serenity.state.manager.StateReader
        with com.serenity.state.manager.StateUpdater
        with com.serenity.state.manager.EventApplier:
        def getCurrentState: IO[AppState]                       = IO.pure(AppState.initial)
        def updateState(update: AppState => AppState): IO[Unit] = IO.unit
        def applyEvent(event: Event): IO[Unit]                  = IO.unit
      clipboard = new SystemClipboard[IO]:
        def readText: IO[Option[String]]      = IO.pure(None)
        def writeText(text: String): IO[Unit] = IO.unit
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
      stateManager = new com.serenity.state.manager.StateReader
        with com.serenity.state.manager.StateUpdater
        with com.serenity.state.manager.EventApplier:
        def getCurrentState: IO[AppState]                       = IO.pure(AppState.initial)
        def updateState(update: AppState => AppState): IO[Unit] = IO.unit
        def applyEvent(event: Event): IO[Unit]                  = IO.unit
      clipboard = new SystemClipboard[IO]:
        def readText: IO[Option[String]]      = IO.pure(None)
        def writeText(text: String): IO[Unit] = IO.unit
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
          makeInputHandler = _ => inputHandler,
          checkResize = IO.pure(None),
          renderFull = (_: AppState, _: Boolean, _: Option[Color]) =>
            initialRenderStarted.complete(()).flatMap(_ => allowInitialRender.get),
          renderCursorOnly = (_: AppState, _: Boolean, _: Option[Color]) => IO.unit,
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
          makeInputHandler = _ => inputHandler,
          checkResize = IO.pure(None),
          renderFull = (_: AppState, _: Boolean, _: Option[Color]) =>
            inputStarted.get >> IO.raiseError(RuntimeException("initial render failed")),
          renderCursorOnly = (_: AppState, _: Boolean, _: Option[Color]) => IO.unit,
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
      stateManager = new com.serenity.state.manager.StateReader
        with com.serenity.state.manager.StateUpdater
        with com.serenity.state.manager.EventApplier:
        def getCurrentState: IO[AppState]                       = IO.pure(AppState.initial)
        def updateState(update: AppState => AppState): IO[Unit] = IO.unit
        def applyEvent(event: Event): IO[Unit]                  = IO.unit
      clipboard = new SystemClipboard[IO]:
        def readText: IO[Option[String]]      = IO.pure(None)
        def writeText(text: String): IO[Unit] = IO.unit
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

  it should "keep fast rendering active when fresh damage arrived during finalization" in {
    AppRuntime
      .shouldClearFastMode(stillActive = false, pendingDamage = Damage.Nothing)
      .shouldBe(true)
    AppRuntime
      .shouldClearFastMode(stillActive = true, pendingDamage = Damage.Nothing)
      .shouldBe(false)
    AppRuntime
      .shouldClearFastMode(stillActive = false, pendingDamage = Damage.Everything)
      .shouldBe(false)
  }

  it should "force quit when the external close signal wins runtime coordination" in {
    val program = for
      forced <- Ref.of[IO, Boolean](false)
      _      <- AppRuntime.coordinateExternalQuit(IO.unit, forced.set(true), IO.never)
      result <- forced.get
    yield result shouldBe true

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "terminate the app loop when the external close signal fires" in {
    given org.typelevel.log4cats.Logger[IO] =
      LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeSpec"))

    val program = AppRuntime.run(
      initialViewportSize = ViewportSize(120, 40),
      makeInputHandler = _ => new SilentInputHandler,
      checkResize = IO.pure(None),
      renderFull = (_: AppState, _: Boolean, _: Option[Color]) => IO.unit,
      renderCursorOnly = (_: AppState, _: Boolean, _: Option[Color]) => IO.unit,
      appConfig = AppConfig.default,
      awaitExternalQuit = IO.unit,
      registerResizeCallback = _ => ()
    )

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "shut the input handler down when the external close signal fires" in {
    given org.typelevel.log4cats.Logger[IO] =
      LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeShutdownSpec"))

    val shutdownObserved = Ref.of[IO, Boolean](false).unsafeRunSync()

    class TrackingInputHandler extends InputHandler[IO]:
      override def keyStrokeInfoStream: Stream[IO, KeyStrokeInfo] = Stream.never
      override def eventStream: Stream[IO, Event]                 = Stream.never
      override def shutdown: IO[Unit]                             = shutdownObserved.set(true)

    val program = AppRuntime.run(
      initialViewportSize = ViewportSize(120, 40),
      makeInputHandler = _ => new TrackingInputHandler,
      checkResize = IO.pure(None),
      renderFull = (_: AppState, _: Boolean, _: Option[Color]) => IO.unit,
      renderCursorOnly = (_: AppState, _: Boolean, _: Option[Color]) => IO.unit,
      appConfig = AppConfig.default,
      awaitExternalQuit = IO.unit,
      registerResizeCallback = _ => ()
    )

    program.unsafeRunTimed(10.seconds) shouldBe defined
    shutdownObserved.get.unsafeRunSync() shouldBe true
  }

  it should "terminate after external close even when app-close session persistence fails" in {
    given org.typelevel.log4cats.Logger[IO] =
      LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeExternalClosePersistenceSpec"))

    val program = AppRuntime.run(
      initialViewportSize = ViewportSize(120, 40),
      makeInputHandler = _ => new SilentInputHandler,
      checkResize = IO.pure(None),
      renderFull = (_: AppState, _: Boolean, _: Option[Color]) => IO.unit,
      renderCursorOnly = (_: AppState, _: Boolean, _: Option[Color]) => IO.unit,
      appConfig = AppConfig.default,
      makeStateManager = Some(logger =>
        IO.blocking(java.nio.file.Files.createTempFile("serenity-session-root", ".tmp")).flatMap { fileRoot =>
          StateManager.apply(
            logger,
            policy = SessionManager.SessionPolicy(saveOnAppClose = true),
            sessionRootOverride = Some(fileRoot)
          )
        }
      ),
      awaitExternalQuit = IO.unit,
      registerResizeCallback = _ => ()
    )

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "force a safe quit when a runtime fiber fails" in {
    given org.typelevel.log4cats.Logger[IO] =
      LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeFiberFailureSpec"))

    val sessionRoot = Files.createTempDirectory("serenity-runtime-fiber-failure")

    val program = AppRuntime.run(
      initialViewportSize = ViewportSize(120, 40),
      makeInputHandler = _ => new SilentInputHandler,
      checkResize = IO.raiseError(new RuntimeException("resize check failed")),
      renderFull = (_: AppState, _: Boolean, _: Option[Color]) => IO.unit,
      renderCursorOnly = (_: AppState, _: Boolean, _: Option[Color]) => IO.unit,
      appConfig = AppConfig.default,
      makeStateManager = Some(logger =>
        StateManager.apply(
          logger,
          policy = SessionManager.SessionPolicy(saveOnAppClose = true),
          sessionRootOverride = Some(sessionRoot)
        )
      ),
      awaitExternalQuit = IO.never,
      registerResizeCallback = _ => ()
    )

    program.unsafeRunTimed(10.seconds) shouldBe defined
    Files.exists(sessionRoot.resolve("session-index.json")) shouldBe true
  }

  it should "describe active document state for crash diagnostics" in {
    val state    = AppState.initial.copy(viewportSize = Some(ViewportSize(120, 40)))
    val bufferId = BufferId(0)
    val buffer = state
      .buffers(bufferId)
      .copy(
        language = Some(LanguageId.JsonLang),
        cursors = List(CursorPosition(2, 4))
      )
    val described = AppRuntime.describeStateForDiagnostics(
      state.copy(buffers = state.buffers.updated(bufferId, buffer))
    )

    described should include("focus=EditorPane(PaneId(0))")
    described should include("viewport=120x40")
    described should include("buffers=1")
    described should include("panes=1")
    described should include("activePane=PaneId(0)")
    described should include("activeBuffer=BufferId(0)")
    described should include("chars=0")
    described should include("lines=1")
    described should include("language=json")
    described should include("cursor=2:4")
  }

  it should "wrap runtime loop failures with current state diagnostics" in {
    given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
    val state        = AppState.initial.copy(viewportSize = Some(ViewportSize(120, 40)))

    val result = AppRuntime
      .withRuntimeDiagnostics(
        loopName = "render loop",
        phase = "fast.full-render",
        stateForDiagnostics = IO.pure(Some(state))
      )(IO.raiseError(RuntimeException("render failed")))
      .attempt
      .unsafeRunSync()

    val failure = result.swap.toOption.getOrElse(fail("Expected runtime failure"))
    val runtimeFailure = failure match
      case wrapped: AppRuntime.RuntimeFailure => wrapped
      case other                              => fail(s"Expected RuntimeFailure, got $other")

    runtimeFailure.loopName shouldBe "render loop"
    runtimeFailure.phase shouldBe "fast.full-render"
    runtimeFailure.diagnostics should include("viewport=120x40")
    runtimeFailure.diagnostics should include("buffers=1")
    runtimeFailure.cause.getMessage shouldBe "render failed"
  }

  it should "preserve existing runtime failures without rewrapping them" in {
    given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
    val existing = AppRuntime.RuntimeFailure(
      loopName = "render loop",
      phase = "idle.state",
      diagnostics = "viewport=120x40",
      cause = RuntimeException("already wrapped")
    )

    val result = AppRuntime
      .withRuntimeDiagnostics(
        loopName = "render loop",
        phase = "ignored",
        stateForDiagnostics = IO.pure(Some(AppState.initial))
      )(IO.raiseError(existing))
      .attempt
      .unsafeRunSync()

    result.shouldBe(Left(existing))
  }

  it should "log and force a safe shutdown when a supervised runtime loop fails" in {
    val program = for
      logs      <- Ref.of[IO, Vector[LogEntry]](Vector.empty)
      forceQuit <- Ref.of[IO, Boolean](false)
      given Logger[IO] = new RecordingLogger(logs)
      _ <- AppRuntime.superviseLoop("render loop", forceQuit.set(true))(
        IO.raiseError(
          AppRuntime.RuntimeFailure(
            loopName = "render loop",
            phase = "idle.cursor-render",
            diagnostics = "viewport=120x40",
            cause = RuntimeException("boom")
          )
        )
      )
      entries <- logs.get
      forced  <- forceQuit.get
    yield
      forced shouldBe true
      val failure = entries.find(_.message.contains("[RUNTIME] render loop failed"))
      failure.map(_.message) shouldBe defined
      failure.get.message should include("phase=idle.cursor-render")
      failure.get.message should include("viewport=120x40")
      failure.flatMap(_.error).map(_.getMessage) should contain("boom")

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "toggle blink cursor visibility for idle frames" in {
    val program = for
      cursorVisible <- Ref.of[IO, Boolean](true)
      breathIndex   <- Ref.of[IO, Int](0)
      first         <- AppRuntime.computeIdleCursorFrame(AppState.initial, cursorVisible, breathIndex)
      second        <- AppRuntime.computeIdleCursorFrame(AppState.initial, cursorVisible, breathIndex)
    yield
      first shouldBe ((false, None))
      second shouldBe ((true, None))

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "derive breathing cursor colours for idle frames" in {
    val state             = AppState.initial.copy(config = AppState.initial.config.withCursorMode(CursorMode.Breathe))
    val expectedBaseColor = state.config.cursorColors.activeOr(state.theme.cursor)
    val expectedAlpha     = ((math.sin(math.Pi / 24) + 1.0) / 2.0 * 255).toInt

    val program = for
      cursorVisible <- Ref.of[IO, Boolean](true)
      breathIndex   <- Ref.of[IO, Int](0)
      frame         <- AppRuntime.computeIdleCursorFrame(state, cursorVisible, breathIndex)
      nextIndex     <- breathIndex.get
    yield
      frame._1 shouldBe true
      nextIndex shouldBe 1
      val cursor = frame._2.getOrElse(fail("Expected breathing cursor colour"))
      cursor.getRed shouldBe expectedBaseColor.getRed
      cursor.getGreen shouldBe expectedBaseColor.getGreen
      cursor.getBlue shouldBe expectedBaseColor.getBlue
      cursor.getAlpha shouldBe expectedAlpha

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "request a full render after recovering an idle cursor render failure" in {
    val program = for
      logs          <- Ref.of[IO, Vector[LogEntry]](Vector.empty)
      requestedFast <- Ref.of[IO, Boolean](false)
      given Logger[IO] = new RecordingLogger(logs)
      _ <- AppRuntime.recoverIdleCursorRenderFailure(
        AppRuntime.RuntimeFailure(
          loopName = "render loop",
          phase = "idle.cursor-render",
          diagnostics = "viewport=120x40",
          cause = RuntimeException("boom")
        ),
        requestedFast.set(true)
      )
      entries   <- logs.get
      requested <- requestedFast.get
    yield
      requested shouldBe true
      val failure = entries.find(_.message.contains("[RUNTIME] idle cursor render failed"))
      failure.map(_.message) shouldBe defined
      failure.get.message should include("phase=idle.cursor-render")
      failure.get.message should include("viewport=120x40")
      failure.flatMap(_.error).map(_.getMessage) should contain("boom")

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "skip idle cursor rendering when the cursor idle interval is disabled" in {
    val state = AppState.initial.copy(
      config = AppState.initial.config.withCursorTransitionSpeedScale(Some(0.0))
    )

    val program = for
      cursorVisible <- Ref.of[IO, Boolean](true)
      breathIndex   <- Ref.of[IO, Int](0)
      renderCalls   <- Ref.of[IO, Int](0)
      given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      _ <- AppRuntime.runIdleRenderStep(
        currentStateForDiagnostics = IO.pure(Some(state)),
        loadState = IO.pure(state),
        checkResizeAndHandle = IO.unit,
        cursorVisible = cursorVisible,
        breathIndex = breathIndex,
        renderCursorOnly = (_: AppState, _: Boolean, _: Option[Color]) => renderCalls.update(_ + 1),
        requestFastRender = IO.unit
      )
      calls <- renderCalls.get
    yield calls shouldBe 0

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "render one idle blink frame when the cursor idle interval is enabled" in {
    val state = AppState.initial

    val program = for
      cursorVisible <- Ref.of[IO, Boolean](true)
      breathIndex   <- Ref.of[IO, Int](0)
      rendered      <- Ref.of[IO, Vector[(Boolean, Option[Color])]](Vector.empty)
      given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      _ <- AppRuntime.runIdleRenderStep(
        currentStateForDiagnostics = IO.pure(Some(state)),
        loadState = IO.pure(state),
        checkResizeAndHandle = IO.unit,
        cursorVisible = cursorVisible,
        breathIndex = breathIndex,
        renderCursorOnly =
          (_: AppState, visible: Boolean, cursor: Option[Color]) => rendered.update(_ :+ (visible -> cursor)),
        requestFastRender = IO.unit
      )
      frames <- rendered.get
    yield frames shouldBe Vector(false -> None)

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "recover idle cursor render failures with phase and state diagnostics" in {
    val program = for
      logs <- Ref.of[IO, Vector[LogEntry]](Vector.empty)
      given Logger[IO] = new RecordingLogger(logs)
      result <- IO.race(
        AppRuntime
          .run(
            initialViewportSize = ViewportSize(120, 40),
            makeInputHandler = _ => new SilentInputHandler,
            checkResize = IO.pure(None),
            renderFull = (_: AppState, _: Boolean, _: Option[Color]) => IO.unit,
            renderCursorOnly =
              (_: AppState, _: Boolean, _: Option[Color]) => IO.raiseError(RuntimeException("idle render failed")),
            appConfig = AppConfig.default,
            makeStateManager = Some(logger =>
              StateManager.apply(
                logger,
                policy = SessionManager.SessionPolicy(saveOnAppClose = false)
              )
            ),
            awaitExternalQuit = IO.never,
            registerResizeCallback = _ => ()
          )
          .as("completed"),
        IO.sleep(1500.millis).as("still-running")
      )
      entries <- logs.get
    yield
      result shouldBe Right("still-running")
      entries.exists(_.message.contains("[RUNTIME] render loop failed")) shouldBe false
      val failure = entries.find(_.message.contains("[RUNTIME] idle cursor render failed"))
      failure.map(_.message) shouldBe defined
      failure.get.message should include("phase=idle.cursor-render")
      failure.get.message should include("viewport=120x40")
      failure.get.message should include("buffers=0")
      failure.get.message should include("surfaces=1")
      failure.get.message should include("activeBuffer=none")
      failure.flatMap(_.error).map(_.getMessage) should contain("idle render failed")

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "log resize callback failures from the runtime bridge" in {
    val program = Dispatcher.parallel[IO].use { dispatcher =>
      for
        logs <- Ref.of[IO, Vector[LogEntry]](Vector.empty)
        given Logger[IO] = new RecordingLogger(logs)
        callback = AppRuntime.resizeCallbackBridge(
          IO.raiseError(new RuntimeException("resize signal failed")),
          dispatcher
        )
        _ <- IO(callback())
        failure <- awaitLogEntry(
          logs,
          _.message.contains("[RUNTIME] resize callback failed")
        )
      yield
        failure.map(_.message) shouldBe defined
        failure.flatMap(_.error).map(_.getMessage) should contain("resize signal failed")
    }

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }
