package com.serenity

import scala.concurrent.duration.*

import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.AppRuntime
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.{StateManager, StateUpdater}
import com.serenity.state.models.{AppState, BufferId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory}

class AppRuntimeCallbackBridgeSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

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

  "AppRuntime" should "log resize callback failures from the runtime bridge" in {
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

  it should "no-op safely when a resize callback fires after the dispatcher has shut down" in {
    // WINCH can arrive during shutdown, after the runtime's Dispatcher is closed. unsafeRunAndForget then throws
    // "Dispatcher already closed" synchronously onto the AWT/pump thread -- before the effect's own error handler runs
    // -- and CrashReporter logs it. The bridge must absorb that late callback rather than raise.
    val program = for
      logs <- Ref.of[IO, Vector[LogEntry]](Vector.empty)
      given Logger[IO] = new RecordingLogger(logs)
      closedDispatcher <- Dispatcher.parallel[IO].use(IO.pure)
      signalled        <- Ref.of[IO, Boolean](false)
      callback = AppRuntime.resizeCallbackBridge(signalled.set(true), closedDispatcher)
      thrown  <- IO(callback()).attempt
      entries <- logs.get
      ran     <- signalled.get
    yield
      thrown shouldBe Right(())
      ran shouldBe false
      entries.exists(_.message.contains("[RUNTIME] resize callback failed")) shouldBe false

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "no-op safely when a focus callback fires after the dispatcher has shut down" in {
    val program = for
      logs <- Ref.of[IO, Vector[LogEntry]](Vector.empty)
      given Logger[IO] = new RecordingLogger(logs)
      closedDispatcher <- Dispatcher.parallel[IO].use(IO.pure)
      windowFocused    <- fs2.concurrent.SignallingRef.of[IO, Boolean](true)
      cursorVisible    <- Ref.of[IO, Boolean](true)
      breathIndex      <- Ref.of[IO, Int](0)
      requested        <- Ref.of[IO, Boolean](false)
      callback = AppRuntime.focusCallbackBridge(
        windowFocused,
        cursorVisible,
        breathIndex,
        requested.set(true),
        closedDispatcher
      )
      thrown  <- IO(callback(false)).attempt
      entries <- logs.get
      ran     <- requested.get
    yield
      thrown shouldBe Right(())
      ran shouldBe false
      entries.exists(_.message.contains("[RUNTIME] focus callback failed")) shouldBe false

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  "closeMarkdownPreviewWindowInState" should "clear the preview window's buffer without touching anything else" in {
    val bufferId = BufferId(7)
    val state = AppState.initial.copy(runtime =
      AppState.initial.runtime.copy(isTuiMode = true, markdownPreviewWindowBuffer = Some(bufferId))
    )

    val cleared = AppRuntime.closeMarkdownPreviewWindowInState(state)

    cleared.runtime.markdownPreviewWindowBuffer shouldBe None
    cleared.runtime.isTuiMode shouldBe true
    cleared.persisted shouldBe state.persisted
  }

  it should "be idempotent when the window is already closed" in {
    val state = AppState.initial

    AppRuntime.closeMarkdownPreviewWindowInState(state) shouldBe state
  }

  "markdownPreviewCloseCallbackBridge" should "sync the window-closed toggle back into application state" in {
    val program = Dispatcher.parallel[IO].use { dispatcher =>
      for
        logs <- Ref.of[IO, Vector[LogEntry]](Vector.empty)
        given Logger[IO] = new RecordingLogger(logs)
        stateManager <- StateManager.apply(
          summon[Logger[IO]],
          policy = SessionManager.SessionPolicy(saveOnAppClose = false)
        )
        bufferId = BufferId(3)
        _ <- stateManager
          .updateState(s => s.copy(runtime = s.runtime.copy(markdownPreviewWindowBuffer = Some(bufferId))))
        callback = AppRuntime.markdownPreviewCloseCallbackBridge(stateManager, dispatcher)
        _ <- IO(callback())
        cleared <-
          def loop(remaining: Int): IO[Boolean] =
            stateManager.getCurrentState.flatMap { state =>
              if state.runtime.markdownPreviewWindowBuffer.isEmpty then IO.pure(true)
              else if remaining <= 0 then IO.pure(false)
              else IO.sleep(25.millis) >> loop(remaining - 1)
            }
          loop(40)
      yield cleared shouldBe true
    }

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "log failures from the runtime bridge" in {
    val program = Dispatcher.parallel[IO].use { dispatcher =>
      for
        logs <- Ref.of[IO, Vector[LogEntry]](Vector.empty)
        given Logger[IO] = new RecordingLogger(logs)
        failingStateManager = new StateUpdater:
          def updateState(update: AppState => AppState): IO[Unit] =
            IO.raiseError(new RuntimeException("markdown preview close signal failed"))
          def updateBufferAnimations(
            update: Map[BufferId, com.serenity.animation.AnimationState] => Map[
              BufferId,
              com.serenity.animation.AnimationState
            ]
          ): IO[Unit] = IO.unit
        callback = AppRuntime.markdownPreviewCloseCallbackBridge(failingStateManager, dispatcher)
        _ <- IO(callback())
        failure <- awaitLogEntry(
          logs,
          _.message.contains("[RUNTIME] markdown preview close callback failed")
        )
      yield
        failure.map(_.message) shouldBe defined
        failure.flatMap(_.error).map(_.getMessage) should contain("markdown preview close signal failed")
    }

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }
