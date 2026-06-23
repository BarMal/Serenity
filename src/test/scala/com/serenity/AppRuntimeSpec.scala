package com.serenity

import java.awt.Color
import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.AppRuntime
import com.serenity.config.AppConfig
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

  "AppRuntime" should "terminate the app loop when the external close signal fires" in {
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
      checkResize = IO.pure(None),
      renderFull = (_: AppState, _: Boolean, _: Option[Color]) => IO.unit,
      renderCursorOnly =
        (_: AppState, _: Boolean, _: Option[Color]) => IO.raiseError(new RuntimeException("cursor render failed")),
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

  it should "log idle render failures with phase and state diagnostics" in {
    val program = for
      logs <- Ref.of[IO, Vector[LogEntry]](Vector.empty)
      given Logger[IO] = new RecordingLogger(logs)
      _ <- AppRuntime.run(
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
      entries <- logs.get
    yield
      val failure = entries.find(_.message.contains("[RUNTIME] render loop failed"))
      failure.map(_.message) shouldBe defined
      failure.get.message should include("phase=idle.cursor-render")
      failure.get.message should include("viewport=120x40")
      failure.get.message should include("buffers=0")
      failure.get.message should include("surfaces=1")
      failure.get.message should include("activeBuffer=none")
      failure.flatMap(_.error).map(_.getMessage) should contain("idle render failed")

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }
