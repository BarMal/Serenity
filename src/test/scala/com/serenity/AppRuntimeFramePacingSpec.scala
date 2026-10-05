package com.serenity

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.{AppRuntime, AppRuntimeRenderLoops}
import com.serenity.config.*
import com.serenity.config.AppConfigOps.*
import com.serenity.diagnostics.{KeyLatencyTrace, LatencyStage}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.color.RenderColor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger

class AppRuntimeFramePacingSpec extends AnyFlatSpec with Matchers:

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

  "AppRuntime" should "derive render frame intervals from the configured FPS target" in {
    AppRuntime.fastFrameInterval(RenderFpsTarget.Fps30).toNanos shouldBe 33333333L
    AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60).toNanos shouldBe 16666666L
    AppRuntime.fastFrameInterval(RenderFpsTarget.Fps90).toNanos shouldBe 11111111L
    AppRuntime.fastFrameInterval(RenderFpsTarget.Fps120).toNanos shouldBe 8333333L
    AppRuntime.fastFrameInterval(RenderFpsTarget.Uncapped).toNanos shouldBe 3333333L
  }

  it should "start the next frame one frame interval after the previous frame started" in {
    val frameInterval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60)

    AppRuntime.fastFrameDelay(frameInterval, Some(1.second), 1.second) shouldBe frameInterval
    AppRuntime.fastFrameDelay(frameInterval, Some(1.second), 1.second + 10.millis) shouldBe frameInterval - 10.millis
  }

  it should "render the first fast frame, and any frame whose deadline has passed, without waiting" in {
    val frameInterval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps30)

    AppRuntime.fastFrameDelay(frameInterval, None, 1.second) shouldBe Duration.Zero
    AppRuntime.fastFrameDelay(frameInterval, Some(1.second), 1.second + frameInterval) shouldBe Duration.Zero
    AppRuntime.fastFrameDelay(frameInterval, Some(1.second), 2.seconds) shouldBe Duration.Zero
  }

  it should "space frame starts by the frame interval however long each render takes" in {
    val interval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60)
    val pacing   = runPacedFrames(renderTime = 10.millis, lastFrameStart = None, startAt = Duration.Zero, frames = 4)

    pacing.frameStarts shouldBe Vector(Duration.Zero, interval, interval * 2, interval * 3)
    pacing.delays shouldBe Vector(Duration.Zero, interval - 10.millis, interval - 10.millis, interval - 10.millis)
  }

  it should "start the next frame at once when the render overran the frame interval" in {
    val pacing = runPacedFrames(renderTime = 25.millis, lastFrameStart = None, startAt = Duration.Zero, frames = 3)

    pacing.frameStarts shouldBe Vector(Duration.Zero, 25.millis, 50.millis)
    pacing.delays shouldBe Vector(Duration.Zero, Duration.Zero, Duration.Zero)
  }

  it should "render input arriving after the previous frame's deadline at once" in {
    val interval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60)
    val pacing =
      runPacedFrames(
        renderTime = 1.milli,
        lastFrameStart = Some(1.second),
        startAt = 1.second + interval * 3,
        frames = 1
      )

    pacing.frameStarts shouldBe Vector(1.second + interval * 3)
    pacing.delays shouldBe Vector(Duration.Zero)
  }

  it should "hold input arriving before the previous frame's deadline until that deadline" in {
    val interval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60)
    val pacing =
      runPacedFrames(renderTime = 1.milli, lastFrameStart = Some(1.second), startAt = 1.second + 5.millis, frames = 1)

    pacing.frameStarts shouldBe Vector(1.second + interval)
    pacing.delays shouldBe Vector(interval - 5.millis)
  }

  it should "stamp a frame's wake, pacing deadline and model read onto the latency trace" in {
    val interval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60)
    val nanos    = new java.util.concurrent.atomic.AtomicLong(1.second.toNanos)
    val trace    = KeyLatencyTrace(() => nanos.get())
    trace.setEnabled(true)
    trace.keyReceived(System.currentTimeMillis())
    trace.keyEnqueued()
    trace.keysDequeued(1)
    trace.dispatchStarted()
    trace.keysApplied(1)
    trace.damageEmitted()

    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(config = AppState.initial.persisted.config.withRenderFpsTarget(RenderFpsTarget.Fps60))
    )
    val program = for
      lastStart          <- Ref.of[IO, Option[FiniteDuration]](Some(1.second - 5.millis))
      fastModeSignal     <- fs2.concurrent.SignallingRef.of[IO, Boolean](true)
      pendingDamage      <- Ref.of[IO, Damage](Damage.Nothing)
      pendingPaintDamage <- Ref.of[IO, Damage](Damage.Nothing)
      stateManager = new com.serenity.state.manager.StateEngine:
        def getCurrentState: IO[AppState] = IO.pure(state)
        def getModel: IO[com.serenity.state.manager.Model] =
          IO.pure(com.serenity.state.manager.Model(state, com.serenity.state.undo.UndoState()))
        def updateStateValidated(update: AppState => AppState): IO[Unit]     = IO.unit
        def applyEvent(event: com.serenity.keystroke.events.Event): IO[Unit] = IO.unit
      given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      _ <- AppRuntimeRenderLoops
        .fastRenderPhase(
          stateManager,
          fastModeSignal,
          pendingDamage,
          pendingPaintDamage,
          IO.pure(Some(state)),
          IO.unit,
          (
            _: AppState,
            _: Boolean,
            _: Option[RenderColor],
            _: Damage,
            _: com.serenity.state.manager.RenderCaches
          ) => IO(nanos.addAndGet(3.millis.toNanos)) >> IO(trace.framePublished()),
          com.serenity.state.manager.RenderCaches.create(),
          delay => IO(nanos.addAndGet(delay.toNanos)).void,
          frameClock = IO(nanos.get().nanos),
          lastFrameStart = lastStart,
          keyLatency = trace
        )
        .compile
        .drain
    yield ()
    program.unsafeRunTimed(10.seconds).getOrElse(fail("the fast render phase did not finish"))
    trace.paintStarted()
    trace.paintFinished()

    val painted = trace.drain().keys
    painted.map(_.stage(LatencyStage.FrameWait)) shouldBe Vector(0L)
    painted.map(_.stage(LatencyStage.PacingWait)) shouldBe Vector((interval - 5.millis).toNanos)
    painted.map(_.stage(LatencyStage.Render)) shouldBe Vector(3.millis.toNanos)
  }

  final private case class PacedFrames(frameStarts: Vector[FiniteDuration], delays: Vector[FiniteDuration])

  /** Runs `frames` successive fast phases (one frame each) against a hand-advanced clock: sleeping and rendering are
    * the only things that move it, and the phases share one `lastFrameStart` as the runtime's do.
    */
  private def runPacedFrames(
    renderTime: FiniteDuration,
    lastFrameStart: Option[FiniteDuration],
    startAt: FiniteDuration,
    frames: Int
  ): PacedFrames =
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted
        .copy(config = AppState.initial.persisted.config.withRenderFpsTarget(RenderFpsTarget.Fps60))
    )
    val program = for
      clock              <- Ref.of[IO, FiniteDuration](startAt)
      lastStart          <- Ref.of[IO, Option[FiniteDuration]](lastFrameStart)
      fastModeSignal     <- fs2.concurrent.SignallingRef.of[IO, Boolean](true)
      pendingDamage      <- Ref.of[IO, Damage](Damage.Nothing)
      pendingPaintDamage <- Ref.of[IO, Damage](Damage.Nothing)
      renders            <- Ref.of[IO, Vector[FiniteDuration]](Vector.empty)
      delays             <- Ref.of[IO, Vector[FiniteDuration]](Vector.empty)
      stateManager = new com.serenity.state.manager.StateEngine:
        def getCurrentState: IO[AppState] = IO.pure(state)
        def getModel: IO[com.serenity.state.manager.Model] =
          IO.pure(com.serenity.state.manager.Model(state, com.serenity.state.undo.UndoState()))
        def updateStateValidated(update: AppState => AppState): IO[Unit]     = IO.unit
        def applyEvent(event: com.serenity.keystroke.events.Event): IO[Unit] = IO.unit
      given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      phase = AppRuntimeRenderLoops.fastRenderPhase(
        stateManager,
        fastModeSignal,
        pendingDamage,
        pendingPaintDamage,
        IO.pure(Some(state)),
        IO.unit,
        (
          _: AppState,
          _: Boolean,
          _: Option[RenderColor],
          _: Damage,
          _: com.serenity.state.manager.RenderCaches
        ) => clock.get.flatMap(now => renders.update(_ :+ now) >> clock.update(_ + renderTime)),
        com.serenity.state.manager.RenderCaches.create(),
        delay => delays.update(_ :+ delay) >> clock.update(_ + delay),
        frameClock = clock.get,
        lastFrameStart = lastStart
      )
      _         <- phase.compile.drain.replicateA_(frames)
      rendered  <- renders.get
      requested <- delays.get
    yield PacedFrames(rendered, requested)

    program.unsafeRunTimed(10.seconds).getOrElse(fail("the fast render phase did not finish"))
