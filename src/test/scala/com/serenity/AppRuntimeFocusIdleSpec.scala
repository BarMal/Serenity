package com.serenity

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.app.AppRuntime
import com.serenity.config.*
import com.serenity.input.InputHandler
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, BufferId, Damage}
import com.serenity.testkit.VirtualTime.runVirtual
import com.serenity.ui.layout.ViewportSize
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

class AppRuntimeFocusIdleSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

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

  "AppRuntime" should "park the cursor steady and request a fast render when focus is lost" in {
    val program = for
      windowFocused       <- fs2.concurrent.SignallingRef.of[IO, Boolean](true)
      cursorVisible       <- Ref.of[IO, Boolean](false)
      breathIndex         <- Ref.of[IO, Int](7)
      fastRenderRequested <- Ref.of[IO, Boolean](false)
      _ <- AppRuntime.onWindowFocusChanged(
        focused = false,
        windowFocused = windowFocused,
        cursorVisible = cursorVisible,
        breathIndex = breathIndex,
        requestFastRender = fastRenderRequested.set(true)
      )
      focusedAfter <- windowFocused.get
      visible      <- cursorVisible.get
      breathe      <- breathIndex.get
      requested    <- fastRenderRequested.get
    yield
      focusedAfter shouldBe false
      visible shouldBe true
      breathe shouldBe 0
      requested shouldBe true

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "mark the window focused again without forcing a fast render when focus is regained" in {
    val program = for
      windowFocused       <- fs2.concurrent.SignallingRef.of[IO, Boolean](false)
      cursorVisible       <- Ref.of[IO, Boolean](true)
      breathIndex         <- Ref.of[IO, Int](0)
      fastRenderRequested <- Ref.of[IO, Boolean](false)
      _ <- AppRuntime.onWindowFocusChanged(
        focused = true,
        windowFocused = windowFocused,
        cursorVisible = cursorVisible,
        breathIndex = breathIndex,
        requestFastRender = fastRenderRequested.set(true)
      )
      focusedAfter <- windowFocused.get
      requested    <- fastRenderRequested.get
    yield
      focusedAfter shouldBe true
      requested shouldBe false

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "block the idle tick while unfocused and unblock once focus returns" in {
    val program = for
      windowFocused <- fs2.concurrent.SignallingRef.of[IO, Boolean](false)
      earlyRace <- IO.race(
        AppRuntime.awaitFocusedIdleTick(IO.pure(AppState.initial), windowFocused),
        IO.sleep(150.millis)
      )
      _ <- windowFocused.set(true)
      lateRace <- IO.race(
        AppRuntime.awaitFocusedIdleTick(IO.pure(AppState.initial), windowFocused),
        IO.sleep(2.seconds)
      )
    yield
      earlyRace shouldBe Right(())
      lateRace.isLeft shouldBe true

    runVirtual(program)
  }

  it should "sleep for the cursor idle interval, not block, while the window is focused" in {
    val fastConfig = AppConfig.default.withElementTransitionSpeedScale(0.02)
    val state      = AppState.initial(fastConfig)

    val program = for
      windowFocused <- fs2.concurrent.SignallingRef.of[IO, Boolean](true)
      result <- IO.race(
        AppRuntime.awaitFocusedIdleTick(IO.pure(state), windowFocused),
        IO.sleep(500.millis)
      )
    yield result shouldBe Left(())

    runVirtual(program)
  }

  it should "never wake the idle tick in TUI blink mode -- the caret is delegated to the terminal, zero wakeups" in {
    val tuiBlinkState =
      AppState.initial.copy(runtime = AppState.initial.runtime.copy(isTuiMode = true))

    val program = for
      windowFocused <- fs2.concurrent.SignallingRef.of[IO, Boolean](true)
      // If awaitFocusedIdleTick ever produced a finite sleep here, the race would resolve Left before the 2s
      // sleep on the right; genuinely sleeping forever is the only way this resolves Right.
      result <- IO.race(
        AppRuntime.awaitFocusedIdleTick(IO.pure(tuiBlinkState), windowFocused),
        IO.sleep(2.seconds)
      )
    yield result shouldBe Right(())

    runVirtual(program)
  }

  it should "still wake the idle tick in TUI breathe mode -- breathe stays the documented app-painted exception" in {
    val fastConfig = AppConfig.default.withElementTransitionSpeedScale(0.02).withCursorMode(CursorMode.Breathe)
    val tuiBreatheState =
      AppState.initial(fastConfig).copy(runtime = AppState.initial(fastConfig).runtime.copy(isTuiMode = true))

    val program = for
      windowFocused <- fs2.concurrent.SignallingRef.of[IO, Boolean](true)
      result <- IO.race(
        AppRuntime.awaitFocusedIdleTick(IO.pure(tuiBreatheState), windowFocused),
        IO.sleep(500.millis)
      )
    yield result shouldBe Left(())

    runVirtual(program)
  }

  it should "skip idle cursor rendering entirely while unfocused, then resume once focus returns" in {
    val fastConfig = AppConfig.default.withElementTransitionSpeedScale(0.02)
    val state      = AppState.initial(fastConfig)

    val program = for
      fastModeSignal     <- fs2.concurrent.SignallingRef.of[IO, Boolean](false)
      windowFocused      <- fs2.concurrent.SignallingRef.of[IO, Boolean](false)
      pendingPaintDamage <- Ref.of[IO, Damage](Damage.Nothing)
      cursorVisible      <- Ref.of[IO, Boolean](true)
      breathIndex        <- Ref.of[IO, Int](0)
      renderCalls        <- Ref.of[IO, Int](0)
      given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      fiber <- AppRuntime
        .idleRenderPhase(
          loadState = IO.pure(state),
          loadBufferAnimations = IO.pure(Map.empty),
          fastModeSignal = fastModeSignal,
          windowFocused = windowFocused,
          pendingPaintDamage = pendingPaintDamage,
          currentStateForDiagnostics = IO.pure(Some(state)),
          checkResizeAndHandle = IO.unit,
          cursorVisible = cursorVisible,
          breathIndex = breathIndex,
          renderCursorOnly = (
            _: AppState,
            _: Boolean,
            _: Option[Color],
            _: Damage,
            _: Map[BufferId, com.serenity.animation.AnimationState]
          ) => renderCalls.update(_ + 1),
          requestFastRender = IO.unit
        )
        .compile
        .drain
        .start
      _                   <- IO.sleep(150.millis)
      callsWhileUnfocused <- renderCalls.get
      _                   <- windowFocused.set(true)
      _                   <- IO.sleep(150.millis)
      callsAfterFocus     <- renderCalls.get
      _                   <- fiber.cancel
    yield
      callsWhileUnfocused shouldBe 0
      callsAfterFocus should be > 0

    runVirtual(program)
  }

  it should "never render an idle cursor frame in TUI blink mode -- zero idle wakeups, terminal owns the caret" in {
    val fastConfig = AppConfig.default.withElementTransitionSpeedScale(0.02)
    val state = AppState.initial(fastConfig).copy(runtime = AppState.initial(fastConfig).runtime.copy(isTuiMode = true))

    val program = for
      fastModeSignal     <- fs2.concurrent.SignallingRef.of[IO, Boolean](false)
      windowFocused      <- fs2.concurrent.SignallingRef.of[IO, Boolean](true)
      pendingPaintDamage <- Ref.of[IO, Damage](Damage.Nothing)
      cursorVisible      <- Ref.of[IO, Boolean](true)
      breathIndex        <- Ref.of[IO, Int](0)
      renderCalls        <- Ref.of[IO, Int](0)
      given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      fiber <- AppRuntime
        .idleRenderPhase(
          loadState = IO.pure(state),
          loadBufferAnimations = IO.pure(Map.empty),
          fastModeSignal = fastModeSignal,
          windowFocused = windowFocused,
          pendingPaintDamage = pendingPaintDamage,
          currentStateForDiagnostics = IO.pure(Some(state)),
          checkResizeAndHandle = IO.unit,
          cursorVisible = cursorVisible,
          breathIndex = breathIndex,
          renderCursorOnly = (
            _: AppState,
            _: Boolean,
            _: Option[Color],
            _: Damage,
            _: Map[BufferId, com.serenity.animation.AnimationState]
          ) => renderCalls.update(_ + 1),
          requestFastRender = IO.unit
        )
        .compile
        .drain
        .start
      _     <- IO.sleep(300.millis) // several multiples of what would have been a 500ms*0.02=10ms idle cadence
      calls <- renderCalls.get
      _     <- fiber.cancel
    yield calls shouldBe 0

    runVirtual(program)
  }

  it should "keep rendering idle breathe frames in TUI mode -- breathe is app-painted, not delegated to the terminal" in {
    val fastConfig = AppConfig.default.withElementTransitionSpeedScale(0.02).withCursorMode(CursorMode.Breathe)
    val state = AppState.initial(fastConfig).copy(runtime = AppState.initial(fastConfig).runtime.copy(isTuiMode = true))

    val program = for
      fastModeSignal     <- fs2.concurrent.SignallingRef.of[IO, Boolean](false)
      windowFocused      <- fs2.concurrent.SignallingRef.of[IO, Boolean](true)
      pendingPaintDamage <- Ref.of[IO, Damage](Damage.Nothing)
      cursorVisible      <- Ref.of[IO, Boolean](true)
      breathIndex        <- Ref.of[IO, Int](0)
      renderCalls        <- Ref.of[IO, Int](0)
      given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      fiber <- AppRuntime
        .idleRenderPhase(
          loadState = IO.pure(state),
          loadBufferAnimations = IO.pure(Map.empty),
          fastModeSignal = fastModeSignal,
          windowFocused = windowFocused,
          pendingPaintDamage = pendingPaintDamage,
          currentStateForDiagnostics = IO.pure(Some(state)),
          checkResizeAndHandle = IO.unit,
          cursorVisible = cursorVisible,
          breathIndex = breathIndex,
          renderCursorOnly = (
            _: AppState,
            _: Boolean,
            _: Option[Color],
            _: Damage,
            _: Map[BufferId, com.serenity.animation.AnimationState]
          ) => renderCalls.update(_ + 1),
          requestFastRender = IO.unit
        )
        .compile
        .drain
        .start
      _     <- IO.sleep(150.millis)
      calls <- renderCalls.get
      _     <- fiber.cancel
    yield calls should be > 0

    runVirtual(program)
  }

  it should "pause idle cursor rendering while unfocused and resume once focus returns, via registerFocusCallback wiring" in {
    given Logger[IO] = LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeFocusWiringSpec"))

    val fastConfig          = AppConfig.default.withElementTransitionSpeedScale(0.02)
    val focusCallbackHolder = new java.util.concurrent.atomic.AtomicReference[Option[Boolean => Unit]](None)

    val program = for
      stateManager <- StateManager.apply(
        LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeFocusWiringSpec")),
        policy = SessionManager.SessionPolicy(saveOnAppClose = false),
        initialConfig = fastConfig
      )
      idleRenderCalls <- Ref.of[IO, Int](0)
      closeRequested  <- Deferred[IO, Unit]
      fiber <- AppRuntime
        .run(
          initialViewportSize = ViewportSize(120, 40),
          makeInputHandler = _ => IO.pure(new SilentInputHandler),
          checkResize = IO.pure(None),
          renderFull = (
            _: AppState,
            _: Boolean,
            _: Option[Color],
            _: Damage,
            _: Map[BufferId, com.serenity.animation.AnimationState]
          ) => IO.unit,
          renderCursorOnly = (
            _: AppState,
            _: Boolean,
            _: Option[Color],
            _: Damage,
            _: Map[BufferId, com.serenity.animation.AnimationState]
          ) => idleRenderCalls.update(_ + 1),
          appConfig = fastConfig,
          makeStateManager = Some(_ => IO.pure(stateManager)),
          awaitExternalQuit = closeRequested.get,
          registerResizeCallback = _ => (),
          registerFocusCallback = cb => focusCallbackHolder.set(Some(cb))
        )
        .start
      _                   <- IO.sleep(200.millis)
      callsBeforeBlur     <- idleRenderCalls.get
      _                   <- IO(focusCallbackHolder.get().foreach(_.apply(false)))
      _                   <- IO.sleep(100.millis)
      _                   <- idleRenderCalls.set(0)
      _                   <- IO.sleep(300.millis)
      callsWhileUnfocused <- idleRenderCalls.get
      _                   <- IO(focusCallbackHolder.get().foreach(_.apply(true)))
      _                   <- IO.sleep(300.millis)
      callsAfterRefocus   <- idleRenderCalls.get
      _                   <- closeRequested.complete(())
      _                   <- fiber.joinWithNever
    yield
      callsBeforeBlur should be > 0
      callsWhileUnfocused shouldBe 0
      callsAfterRefocus should be > 0

    runVirtual(program)
  }

  it should "recover idle cursor render failures with phase and state diagnostics" in {
    val program = for
      logs <- Ref.of[IO, Vector[LogEntry]](Vector.empty)
      given Logger[IO] = new RecordingLogger(logs)
      result <- IO.race(
        AppRuntime
          .run(
            initialViewportSize = ViewportSize(120, 40),
            makeInputHandler = _ => IO.pure(new SilentInputHandler),
            checkResize = IO.pure(None),
            renderFull = (
              _: AppState,
              _: Boolean,
              _: Option[Color],
              _: Damage,
              _: Map[BufferId, com.serenity.animation.AnimationState]
            ) => IO.unit,
            renderCursorOnly = (
              _: AppState,
              _: Boolean,
              _: Option[Color],
              _: Damage,
              _: Map[BufferId, com.serenity.animation.AnimationState]
            ) => IO.raiseError(RuntimeException("idle render failed")),
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

    runVirtual(program)
  }
