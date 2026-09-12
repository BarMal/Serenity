package com.serenity

import java.awt.Color
import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.AppRuntime
import com.serenity.config.*
import com.serenity.input.InputHandler
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*
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

class AppRuntimeLifecycleSpec extends AnyFlatSpec with Matchers:

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

  "AppRuntime" should "keep fast rendering active when fresh damage arrived during finalization" in {
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
      ) => IO.unit,
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
      makeInputHandler = _ => IO.pure(new TrackingInputHandler),
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
      ) => IO.unit,
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
      ) => IO.unit,
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
      makeInputHandler = _ => IO.pure(new SilentInputHandler),
      checkResize = IO.raiseError(new RuntimeException("resize check failed")),
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
      ) => IO.unit,
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
    val state =
      AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(120, 40))))
    val bufferId = BufferId(0)
    val buffer = state.persisted
      .buffers(bufferId)
      .copy(
        document = state.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.JsonLang)),
        editing = state.persisted.buffers(bufferId).editing.copy(cursors = List(CursorPosition(2, 4)))
      )
    val described = AppRuntime.describeStateForDiagnostics(
      state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, buffer)))
    )

    described should include("focus=EditorPane(0)")
    described should include("viewport=120x40")
    described should include("buffers=1")
    described should include("panes=1")
    described should include("activePane=0")
    described should include("activeBuffer=0")
    described should include("chars=0")
    described should include("lines=1")
    described should include("language=json")
    described should include("cursor=2:4")
  }

  it should "wrap runtime loop failures with current state diagnostics" in {
    given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
    val state =
      AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(120, 40))))

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
