package com.serenity

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.semigroup.*
import com.serenity.app.AppRuntime
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.input.{InputRouter, SystemClipboard}
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.{TextEntryTranslator, Translator}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId, Damage}
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
      persisted = AppState.initial.persisted.copy(
        config = AppState.initial.persisted.config.withRenderFpsTarget(RenderFpsTarget.Fps30)
      ),
      runtime = AppState.initial.runtime.copy(
        surfaceAnimations = Map(
          com.serenity.state.models.SurfaceId("fast-render-regression") -> com.serenity.state.models
            .SurfaceAnimationState()
        )
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
        def getCurrentState: IO[AppState]                                                 = IO.pure(state)
        def getBufferAnimations: IO[Map[BufferId, com.serenity.animation.AnimationState]] = IO.pure(Map.empty)
        def updateState(update: AppState => AppState): IO[Unit]                           = IO.unit
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
            _: Map[BufferId, com.serenity.animation.AnimationState]
          ) => animationTicks.get.flatMap(tickCount => rendered.update(_ :+ tickCount)),
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
