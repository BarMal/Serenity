package com.serenity

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.semigroup.*
import com.serenity.app.{AppRuntime, AppRuntimeRenderLoops}
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.input.{InputRouter, SystemClipboard}
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.{TextEntryTranslator, Translator}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId, Damage}
import com.serenity.testkit.ActiveAnimationFixtures
import fs2.Stream
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

  it should "render an input-requested first fast frame immediately, pace its follow-up, and defer its animation tick" in {
    val frameInterval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps30)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppState.initial.persisted.config.withRenderFpsTarget(RenderFpsTarget.Fps30)
      )
    )

    val program = for
      fastModeSignal       <- fs2.concurrent.SignallingRef.of[IO, Boolean](false)
      pendingDamage        <- Ref.of[IO, Damage](Damage.Nothing)
      pendingPaintDamage   <- Ref.of[IO, Damage](Damage.Nothing)
      animationTickCadence <- Ref.of[IO, AppRuntime.AnimationTickCadence](AppRuntime.AnimationTickCadence.empty)
      animationTicks       <- Ref.of[IO, Int](0)
      rendered             <- Ref.of[IO, Vector[Int]](Vector.empty)
      requestedDelays      <- Ref.of[IO, Vector[FiniteDuration]](Vector.empty)
      cursorVisible        <- Ref.of[IO, Boolean](true)
      breathIndex          <- Ref.of[IO, Int](0)
      stateManager = new com.serenity.state.manager.StateEngine:
        def getCurrentState: IO[AppState] = IO.pure(state)
        def getModel: IO[com.serenity.state.manager.Model] =
          IO.pure(
            com.serenity.state.manager
              .Model(state, com.serenity.state.undo.UndoState(), ActiveAnimationFixtures.bufferAnimations(state))
          )
        def getBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]] = IO.pure(Map.empty)
        def updateState(update: AppState => AppState): IO[Unit]                           = IO.unit
        def updateStateValidated(update: AppState => AppState): IO[Unit]                  = IO.unit
        def updateBufferAnimations(
          update: Map[BufferId, com.serenity.animation.AnimationState] => Map[
            BufferId,
            com.serenity.animation.AnimationState
          ]
        ): IO[Unit] = IO.unit
        def applyEvent(event: Event): IO[Unit] = IO.unit
        def advanceAnimationFrames(): IO[Unit] = IO.unit
      animationTicker = com.serenity.state.manager.AnimationTicker(
        advanceAnimationsOnTick = animationTicks.updateAndGet(_ + 1).as(true)
      )
      inputRouter = new InputRouter[IO, Event]:
        private val translator = new TextEntryTranslator(AppConfig.default)

        def eventStream(infoStream: Stream[IO, KeyStrokeInfo]): Stream[IO, Event] = Stream.empty
        def setActiveTranslator(translator: Translator[Event]): IO[Unit]          = IO.unit
        def getActiveTranslator: IO[Translator[Event]]                            = IO.pure(translator)
      clipboard                        = SystemClipboard[IO](readText = IO.pure(None), writeText = _ => IO.unit)
      emitDamage: (Damage => IO[Unit]) = damage => pendingDamage.update(_ |+| damage) >> fastModeSignal.set(true)
      given Logger[IO]                 = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      _ <- AppRuntimeRenderLoops
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
      _ <- AppRuntimeRenderLoops
        .fastRenderPhase(
          stateManager,
          animationTicker,
          fastModeSignal,
          pendingDamage,
          pendingPaintDamage,
          animationTickCadence,
          IO.pure(Some(state)),
          IO.unit,
          (
            _: AppState,
            _: Boolean,
            _: Option[Color],
            _: Damage,
            _: Map[BufferId, com.serenity.animation.AnimationState],
            _: com.serenity.state.manager.RenderCaches
          ) => animationTicks.get.flatMap(tickCount => rendered.update(_ :+ tickCount)),
          com.serenity.state.manager.RenderCaches.create(),
          delay => requestedDelays.update(_ :+ delay),
          frameClock = IO.pure(Duration.Zero)
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

  it should "advance animations once per frame at whatever render FPS is configured, not a fixed 60Hz rate" in
    List(RenderFpsTarget.Fps30, RenderFpsTarget.Fps60, RenderFpsTarget.Fps90, RenderFpsTarget.Fps120).foreach {
      target =>
        val interval       = AppRuntime.fastFrameInterval(target)
        val (after, ticks) = AppRuntime.AnimationTickCadence.empty.advance(interval, interval)

        withClue(s"target=$target ") {
          ticks shouldBe 1
          after.remainderNanos shouldBe 0L
        }
    }

  it should "advance animations by the time elapsed since the previous frame, not one tick per frame" in {
    val interval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60)

    val (afterLateFrame, lateTicks) = AppRuntime.AnimationTickCadence.empty.advance(interval * 5 / 2, interval)
    lateTicks shouldBe 2
    afterLateFrame.remainderNanos shouldBe interval.toNanos / 2

    val (_, nextTicks) = afterLateFrame.advance(interval / 2, interval)
    nextTicks shouldBe 1
  }

  it should "cap the animation catch-up after a stall" in {
    val interval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60)

    val (_, ticks) = AppRuntime.AnimationTickCadence.empty.advance(10.seconds, interval)

    ticks shouldBe AppRuntime.AnimationTickCadence.MaxTicksPerFrame.toInt
  }

  it should "space frame starts by the frame interval however long each render takes" in {
    val interval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60)
    val pacing   = runPacedFrames(renderTime = 10.millis, lastFrameStart = None, startAt = Duration.Zero, frames = 4)

    pacing.frameStarts shouldBe Vector(Duration.Zero, interval, interval * 2, interval * 3)
    pacing.delays shouldBe Vector(Duration.Zero, interval - 10.millis, interval - 10.millis, interval - 10.millis)
    pacing.ticksPerFrame shouldBe Vector(0, 1, 1, 1)
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

  final private case class PacedFrames(
      frameStarts: Vector[FiniteDuration],
      delays: Vector[FiniteDuration],
      ticksPerFrame: Vector[Int]
  )

  /** Drives the fast phase against a hand-advanced clock: sleeping and rendering are the only things that move it. */
  private def runPacedFrames(
    renderTime: FiniteDuration,
    lastFrameStart: Option[FiniteDuration],
    startAt: FiniteDuration,
    frames: Int
  ): PacedFrames =
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppState.initial.persisted.config.withRenderFpsTarget(RenderFpsTarget.Fps60)
      )
    )
    val program = for
      clock                <- Ref.of[IO, FiniteDuration](startAt)
      lastStart            <- Ref.of[IO, Option[FiniteDuration]](lastFrameStart)
      fastModeSignal       <- fs2.concurrent.SignallingRef.of[IO, Boolean](true)
      pendingDamage        <- Ref.of[IO, Damage](Damage.Nothing)
      pendingPaintDamage   <- Ref.of[IO, Damage](Damage.Nothing)
      animationTickCadence <- Ref.of[IO, AppRuntime.AnimationTickCadence](AppRuntime.AnimationTickCadence.empty)
      animationTicks       <- Ref.of[IO, Int](0)
      renders              <- Ref.of[IO, Vector[(FiniteDuration, Int)]](Vector.empty)
      delays               <- Ref.of[IO, Vector[FiniteDuration]](Vector.empty)
      stateManager = new com.serenity.state.manager.StateEngine:
        def getCurrentState: IO[AppState] = IO.pure(state)
        def getModel: IO[com.serenity.state.manager.Model] =
          IO.pure(
            com.serenity.state.manager
              .Model(state, com.serenity.state.undo.UndoState(), ActiveAnimationFixtures.bufferAnimations(state))
          )
        def updateStateValidated(update: AppState => AppState): IO[Unit] = IO.unit
        def applyEvent(event: Event): IO[Unit]                           = IO.unit
      animationTicker = com.serenity.state.manager.AnimationTicker(
        advanceAnimationsOnTick = animationTicks.update(_ + 1).as(true)
      )
      given Logger[IO] = new RecordingLogger(Ref.unsafe[IO, Vector[LogEntry]](Vector.empty))
      _ <- AppRuntimeRenderLoops
        .fastRenderPhase(
          stateManager,
          animationTicker,
          fastModeSignal,
          pendingDamage,
          pendingPaintDamage,
          animationTickCadence,
          IO.pure(Some(state)),
          IO.unit,
          (
            _: AppState,
            _: Boolean,
            _: Option[Color],
            _: Damage,
            _: Map[BufferId, com.serenity.animation.AnimationState],
            _: com.serenity.state.manager.RenderCaches
          ) =>
            for
              now   <- clock.get
              ticks <- animationTicks.getAndSet(0)
              _     <- renders.update(_ :+ (now -> ticks))
              _     <- clock.update(_ + renderTime)
            yield (),
          com.serenity.state.manager.RenderCaches.create(),
          delay => delays.update(_ :+ delay) >> clock.update(_ + delay),
          frameClock = clock.get,
          lastFrameStart = lastStart
        )
        .take(frames.toLong)
        .compile
        .drain
      rendered  <- renders.get
      requested <- delays.get
    yield PacedFrames(rendered.map(_._1), requested, rendered.map(_._2))

    program.unsafeRunTimed(10.seconds).getOrElse(fail("the fast render phase did not finish"))
