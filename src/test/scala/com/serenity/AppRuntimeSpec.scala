package com.serenity

import java.awt.Color
import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.AppRuntime
import com.serenity.config.{AppConfig, RenderFpsTarget}
import com.serenity.input.InputHandler
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, BufferId, CursorPosition}
import com.serenity.ui.layout.ViewportSize
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

class AppRuntimeSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private class SilentInputHandler extends InputHandler[IO]:
    override def keyStrokeInfoStream: Stream[IO, KeyStrokeInfo] = Stream.never
    override def eventStream: Stream[IO, Event]                 = Stream.never
    override def shutdown: IO[Unit]                             = IO.unit

  private case class LogEntry(level: String, message: String, error: Option[Throwable])

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

  "AppRuntime" should "derive render frame intervals from the configured FPS target" in {
    AppRuntime.fastFrameInterval(RenderFpsTarget.Fps30).toNanos shouldBe 33333333L
    AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60).toNanos shouldBe 16666666L
    AppRuntime.fastFrameInterval(RenderFpsTarget.Fps90).toNanos shouldBe 11111111L
    AppRuntime.fastFrameInterval(RenderFpsTarget.Fps120).toNanos shouldBe 8333333L
    AppRuntime.fastFrameInterval(RenderFpsTarget.Uncapped).toNanos shouldBe 3333333L
  }

  it should "advance animations at a stable 60 FPS cadence across render targets" in {
    val sixtyFpsCadence = AppRuntime.AnimationTickCadence.empty
    val (after60, ticks60) =
      sixtyFpsCadence.advance(AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60))

    ticks60 shouldBe 1
    after60.remainderNanos shouldBe 0L

    val (afterFirst120, first120Ticks) =
      AppRuntime.AnimationTickCadence.empty.advance(AppRuntime.fastFrameInterval(RenderFpsTarget.Fps120))
    val (afterSecond120, second120Ticks) =
      afterFirst120.advance(AppRuntime.fastFrameInterval(RenderFpsTarget.Fps120))

    first120Ticks shouldBe 0
    second120Ticks shouldBe 1
    afterSecond120.remainderNanos shouldBe 0L

    val (_, ticks30) =
      AppRuntime.AnimationTickCadence.empty.advance(AppRuntime.fastFrameInterval(RenderFpsTarget.Fps30))

    ticks30 shouldBe 2
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

  it should "keep fast rendering active when a newer render request arrives during finalization" in {
    AppRuntime
      .shouldClearFastMode(stillActive = false, phaseStartRenderRequest = 1L, currentRenderRequest = 1L)
      .shouldBe(true)
    AppRuntime
      .shouldClearFastMode(stillActive = true, phaseStartRenderRequest = 1L, currentRenderRequest = 1L)
      .shouldBe(false)
    AppRuntime
      .shouldClearFastMode(stillActive = false, phaseStartRenderRequest = 1L, currentRenderRequest = 2L)
      .shouldBe(false)
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
    val state = AppState.initial.copy(viewportSize = Some(ViewportSize(120, 40)))

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
