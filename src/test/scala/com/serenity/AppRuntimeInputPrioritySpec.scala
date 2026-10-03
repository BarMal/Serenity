package com.serenity

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.{AppRuntime, AppRuntimeRenderLoops}
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.RenderFpsTarget
import com.serenity.keystroke.events.Event
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId, Damage, SurfaceAnimationState, SurfaceId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerName}

/** A fast frame with fresh input to show skips its animation tick, so the input paints without waiting behind it. */
class AppRuntimeInputPrioritySpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = Slf4jFactory.create[IO].getLogger(using LoggerName("AppRuntimeInputPrioritySpec"))

  private val interval = AppRuntime.fastFrameInterval(RenderFpsTarget.Fps60)

  "AnimationTickCadence" should "bank a frame's ticks while input is pending and run them on the next frame" in {
    val (yielded, ticksWhileInput) = AppRuntime.AnimationTickCadence.empty.advanceYieldingTo(true, interval, interval)
    val (_, ticksAfter)            = yielded.advanceYieldingTo(false, interval, interval)

    ticksWhileInput shouldBe 0
    ticksAfter shouldBe 2
  }

  it should "never yield two frames running, so held-down keys cannot stall animations" in {
    val (yielded, _)           = AppRuntime.AnimationTickCadence.empty.advanceYieldingTo(true, interval, interval)
    val (again, ticksOnSecond) = yielded.advanceYieldingTo(true, interval, interval)
    val (_, ticksOnThird)      = again.advanceYieldingTo(true, interval, interval)

    ticksOnSecond shouldBe 2
    ticksOnThird shouldBe 0
  }

  it should "still cap the catch-up after a stall that followed a yielded frame" in {
    val (yielded, _) = AppRuntime.AnimationTickCadence.empty.advanceYieldingTo(true, 10.seconds, interval)
    val (_, ticks)   = yielded.advance(10.seconds, interval)

    ticks shouldBe AppRuntime.AnimationTickCadence.MaxTicksPerFrame.toInt
  }

  private val animatingState = AppState.initial.copy(
    persisted = AppState.initial.persisted.copy(
      config = AppState.initial.persisted.config.withRenderFpsTarget(RenderFpsTarget.Fps60)
    ),
    runtime = AppState.initial.runtime.copy(
      motion = AppState.initial.runtime.motion
        .copy(surfaceAnimations = Map(SurfaceId("input-priority") -> SurfaceAnimationState()))
    )
  )

  /** Runs up to `frames` fast frames and returns the ticks each one ran. The model reports a full-content animation
    * only until the first frame has rendered; from then on only the ticker knows animations are still running.
    */
  private def ticksPerFrame(inputPendingPerFrame: Vector[Boolean], frames: Int): Vector[Int] =
    val settledState = animatingState.copy(runtime = AppState.initial.runtime)
    val program = for
      clock          <- Ref.of[IO, FiniteDuration](Duration.Zero)
      animationTicks <- Ref.of[IO, Int](0)
      rendered       <- Ref.of[IO, Vector[Int]](Vector.empty)
      inputPending   <- Ref.of[IO, Vector[Boolean]](inputPendingPerFrame)
      fastModeSignal <- fs2.concurrent.SignallingRef.of[IO, Boolean](true)
      stateManager = new com.serenity.state.manager.StateEngine:
        def getCurrentState: IO[AppState] = getModel.map(_.app)
        def getModel: IO[com.serenity.state.manager.Model] =
          rendered.get.map(done =>
            com.serenity.state.manager
              .Model(
                if done.isEmpty then animatingState else settledState,
                com.serenity.state.undo.UndoState(),
                Map.empty
              )
          )
        def updateStateValidated(update: AppState => AppState): IO[Unit] = IO.unit
        def applyEvent(event: Event): IO[Unit]                           = IO.unit
      _ <- AppRuntimeRenderLoops
        .fastRenderPhase(
          stateManager,
          com.serenity.state.manager.AnimationTicker(animationTicks.update(_ + 1).as(true)),
          fastModeSignal,
          Ref.unsafe[IO, Damage](Damage.Nothing),
          Ref.unsafe[IO, Damage](Damage.Nothing),
          Ref.unsafe[IO, AppRuntime.AnimationTickCadence](AppRuntime.AnimationTickCadence.empty),
          IO.pure(Some(animatingState)),
          IO.unit,
          (
            _: AppState,
            _: Boolean,
            _: Option[Color],
            _: Damage,
            _: Map[BufferId, com.serenity.animation.AnimationState],
            _: com.serenity.state.manager.RenderCaches
          ) => animationTicks.getAndSet(0).flatMap(ticks => rendered.update(_ :+ ticks)),
          com.serenity.state.manager.RenderCaches.create(),
          delay => clock.update(_ + delay),
          frameClock = clock.get,
          takeInputPending = inputPending.modify(pending => (pending.drop(1), pending.headOption.getOrElse(false)))
        )
        .take(frames.toLong)
        .compile
        .drain
      result <- rendered.get
    yield result

    program.unsafeRunTimed(10.seconds).getOrElse(fail("the fast render phase did not finish"))

  "The fast render phase" should "skip the animation tick of a frame showing fresh input, then catch it up" in {
    ticksPerFrame(Vector(true, false, false), frames = 4) shouldBe Vector(0, 0, 2, 1)
  }

  it should "keep running past a frame that skipped its tick, leaving the next tick to decide" in {
    ticksPerFrame(Vector(true, false, false), frames = 4) should have size 4
  }
