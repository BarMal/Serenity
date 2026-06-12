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
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.StateManager
import com.serenity.state.models.AppState
import com.serenity.ui.layout.ViewportSize
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class AppRuntimeSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private class SilentInputHandler extends InputHandler[IO]:
    override def keyStrokeInfoStream: Stream[IO, KeyStrokeInfo] = Stream.never
    override def eventStream: Stream[IO, Event]                 = Stream.never
    override def shutdown: IO[Unit]                             = IO.unit

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
