package com.serenity

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.testkit.TestControl
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.{AppRuntime, AppRuntimeRenderLoops}
import com.serenity.config.AppConfig
import com.serenity.frontend.GuiFrontend
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId, Damage}
import com.serenity.testkit.VirtualTime.runVirtual
import fs2.Stream
import fs2.concurrent.SignallingRef
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger

class AppRuntimeIdleCursorRenderSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

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

  "AppRuntime" should "toggle blink cursor visibility for idle frames" in {
    val program = for
      cursorVisible <- Ref.of[IO, Boolean](true)
      first         <- AppRuntimeRenderLoops.computeIdleCursorFrame(cursorVisible)
      second        <- AppRuntimeRenderLoops.computeIdleCursorFrame(cursorVisible)
    yield
      first shouldBe ((false, None))
      second shouldBe ((true, None))

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "request a full render after recovering an idle cursor render failure" in {
    val program = for
      logs          <- Ref.of[IO, Vector[LogEntry]](Vector.empty)
      requestedFast <- Ref.of[IO, Boolean](false)
      given Logger[IO] = new RecordingLogger(logs)
      _ <- AppRuntimeRenderLoops.recoverIdleCursorRenderFailure(
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
    val state = AppState.initial

    val program = for
      cursorVisible      <- Ref.of[IO, Boolean](true)
      pendingPaintDamage <- Ref.of[IO, Damage](Damage.Nothing)
      renderCalls        <- Ref.of[IO, Int](0)
      given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      _ <- AppRuntimeRenderLoops.runIdleRenderStep(
        currentStateForDiagnostics = IO.pure(Some(state)),
        loadModel = IO.pure(com.serenity.state.manager.Model(state, com.serenity.state.undo.UndoState())),
        pendingPaintDamage = pendingPaintDamage,
        checkResizeAndHandle = IO.unit,
        cursorVisible = cursorVisible,
        renderCursorOnly = (
          _: AppState,
          _: Boolean,
          _: Option[Color],
          _: Damage,
          _: com.serenity.state.manager.RenderCaches
        ) => renderCalls.update(_ + 1),
        requestFastRender = IO.unit,
        cursorIdleInterval = _ => None,
        renderCaches = com.serenity.state.manager.RenderCaches.create()
      )
      calls <- renderCalls.get
    yield calls shouldBe 0

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "leave content damage pending for the fast phase when an idle cursor frame renders" in {
    val state  = AppState.initial
    val damage = Damage.BufferRows(BufferId(1), Set(3))

    val program = for
      cursorVisible      <- Ref.of[IO, Boolean](true)
      pendingPaintDamage <- Ref.of[IO, Damage](damage)
      given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      _ <- AppRuntimeRenderLoops.runIdleRenderStep(
        currentStateForDiagnostics = IO.pure(Some(state)),
        loadModel = IO.pure(com.serenity.state.manager.Model(state, com.serenity.state.undo.UndoState())),
        pendingPaintDamage = pendingPaintDamage,
        checkResizeAndHandle = IO.unit,
        cursorVisible = cursorVisible,
        renderCursorOnly = (
          _: AppState,
          _: Boolean,
          _: Option[Color],
          _: Damage,
          _: com.serenity.state.manager.RenderCaches
        ) => IO.unit,
        requestFastRender = IO.unit,
        cursorIdleInterval = GuiFrontend.cursorIdleInterval,
        renderCaches = com.serenity.state.manager.RenderCaches.create()
      )
      remaining <- pendingPaintDamage.get
    yield remaining shouldBe damage

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  it should "render one idle blink frame when the cursor idle interval is enabled" in {
    val state = AppState.initial

    val program = for
      cursorVisible      <- Ref.of[IO, Boolean](true)
      pendingPaintDamage <- Ref.of[IO, Damage](Damage.Nothing)
      rendered           <- Ref.of[IO, Vector[(Boolean, Option[Color])]](Vector.empty)
      given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      _ <- AppRuntimeRenderLoops.runIdleRenderStep(
        currentStateForDiagnostics = IO.pure(Some(state)),
        loadModel = IO.pure(com.serenity.state.manager.Model(state, com.serenity.state.undo.UndoState())),
        pendingPaintDamage = pendingPaintDamage,
        checkResizeAndHandle = IO.unit,
        cursorVisible = cursorVisible,
        renderCursorOnly = (
          _: AppState,
          visible: Boolean,
          cursor: Option[Color],
          _: Damage,
          _: com.serenity.state.manager.RenderCaches
        ) => rendered.update(_ :+ (visible -> cursor)),
        requestFastRender = IO.unit,
        cursorIdleInterval = GuiFrontend.cursorIdleInterval,
        renderCaches = com.serenity.state.manager.RenderCaches.create()
      )
      frames <- rendered.get
    yield frames shouldBe Vector(false -> None)

    program.unsafeRunTimed(10.seconds) shouldBe defined
  }

  /** The idle phase as `AppRuntime.run` wires it, recording the visibility of every caret frame it paints. */
  private def recordingIdlePhase(
    config: AppConfig,
    fastModeSignal: SignallingRef[IO, Boolean],
    cursorVisible: Ref[IO, Boolean],
    painted: Ref[IO, Vector[Boolean]]
  ): Stream[IO, Unit] =
    val state = AppState.initial(config)
    given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
    Stream
      .eval(SignallingRef.of[IO, Boolean](true))
      .flatMap(windowFocused =>
        AppRuntimeRenderLoops.idleRenderPhase(
          loadModel = IO.pure(com.serenity.state.manager.Model(state, com.serenity.state.undo.UndoState())),
          fastModeSignal = fastModeSignal,
          windowFocused = windowFocused,
          pendingPaintDamage = Ref.unsafe[IO, Damage](Damage.Nothing),
          currentStateForDiagnostics = IO.pure(Some(state)),
          checkResizeAndHandle = IO.unit,
          cursorVisible = cursorVisible,
          renderCursorOnly = (
            _: AppState,
            visible: Boolean,
            _: Option[Color],
            _: Damage,
            _: com.serenity.state.manager.RenderCaches
          ) => painted.update(_ :+ visible),
          requestFastRender = IO.unit,
          cursorIdleInterval = GuiFrontend.cursorIdleInterval,
          renderCaches = com.serenity.state.manager.RenderCaches.create()
        )
      )

  private val twoSecondBlinkTimeout = AppConfig.default.withCursorBlinkTimeoutMillis(2000L)

  "The idle render phase" should "hold the caret solid once the blink timeout passes without input" in {
    val program = for
      fastModeSignal <- SignallingRef.of[IO, Boolean](false)
      cursorVisible  <- Ref.of[IO, Boolean](true)
      painted        <- Ref.of[IO, Vector[Boolean]](Vector.empty)
      idlePhase = recordingIdlePhase(twoSecondBlinkTimeout, fastModeSignal, cursorVisible, painted)
      fiber   <- idlePhase.compile.drain.start
      _       <- IO.sleep(30.seconds)
      frames  <- painted.get
      visible <- cursorVisible.get
      _       <- fiber.cancel
    yield
      // Blinks at 0.5s, 1s and 1.5s; at 2s the timeout has passed and the hidden caret is painted solid.
      frames shouldBe Vector(false, true, false, true)
      visible shouldBe true

    runVirtual(program)
  }

  it should "schedule no further wakeups once the caret holds solid" in {
    val program = for
      fastModeSignal <- SignallingRef.of[IO, Boolean](false)
      cursorVisible  <- Ref.of[IO, Boolean](true)
      painted        <- Ref.of[IO, Vector[Boolean]](Vector.empty)
      _              <- recordingIdlePhase(twoSecondBlinkTimeout, fastModeSignal, cursorVisible, painted).compile.drain
    yield ()

    val parked = (for
      control  <- TestControl.execute(program)
      _        <- control.tickFor(1.minute)
      finished <- control.results
      idle     <- control.isDeadlocked
    yield (finished, idle)).unsafeRunSync()

    parked shouldBe ((None, true))
  }

  it should "blink again, for a fresh timeout, after input wakes the render loop" in {
    val program = for
      fastModeSignal <- SignallingRef.of[IO, Boolean](false)
      cursorVisible  <- Ref.of[IO, Boolean](true)
      painted        <- Ref.of[IO, Vector[Boolean]](Vector.empty)
      idlePhase = recordingIdlePhase(twoSecondBlinkTimeout, fastModeSignal, cursorVisible, painted)
      fiber <- AppRuntimeRenderLoops
        .renderLoop(idlePhase, Stream.exec(fastModeSignal.set(false)))
        .compile
        .drain
        .start
      _           <- IO.sleep(30.seconds)
      beforeInput <- painted.getAndSet(Vector.empty)
      // What a real input batch does: AppRuntime.resetCursorActivity, then emitDamage raising fast mode.
      _          <- AppRuntime.resetCursorActivity(cursorVisible) >> fastModeSignal.set(true)
      _          <- IO.sleep(30.seconds)
      afterInput <- painted.get
      _          <- fiber.cancel
    yield
      beforeInput shouldBe Vector(false, true, false, true)
      afterInput shouldBe Vector(false, true, false, true)

    runVirtual(program)
  }

  it should "keep blinking for as long as the window is focused when the blink timeout is 0" in {
    val neverTimingOut = AppConfig.default.withCursorBlinkTimeoutMillis(0L)

    val program = for
      fastModeSignal <- SignallingRef.of[IO, Boolean](false)
      cursorVisible  <- Ref.of[IO, Boolean](true)
      painted        <- Ref.of[IO, Vector[Boolean]](Vector.empty)
      idlePhase = recordingIdlePhase(neverTimingOut, fastModeSignal, cursorVisible, painted)
      fiber  <- idlePhase.compile.drain.start
      _      <- IO.sleep(59900.millis)
      frames <- painted.get
      _      <- fiber.cancel
    yield frames should have size 119

    runVirtual(program)
  }
