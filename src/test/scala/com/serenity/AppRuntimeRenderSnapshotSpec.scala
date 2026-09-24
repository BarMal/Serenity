package com.serenity

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.animation.AnimationState
import com.serenity.app.{AppRuntime, AppRuntimeRenderLoops}
import com.serenity.keystroke.events.Event
import com.serenity.rope.Balance
import com.serenity.state.manager.{AnimationTicker, Model, StateEngine}
import com.serenity.state.models.{AppState, BufferId, Damage, SurfaceAnimationState, SurfaceId}
import com.serenity.state.undo.UndoState
import fs2.concurrent.SignallingRef
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** The renderer paints app state and buffer animations from one model snapshot (#1697 F3). The fake engine here commits
  * a new version between every two reads -- the worst case for a reader that takes app state and animations in separate
  * reads -- and tags both halves with the version they belong to.
  */
class AppRuntimeRenderSnapshotSpec extends AnyFlatSpec with Matchers:

  given Balance    = Balance.default
  given Logger[IO] = NoOpLogger.impl[IO]

  private val animating: AppState = AppState.initial.copy(runtime =
    AppState.initial.runtime.copy(surfaceAnimations = Map(SurfaceId("render-snapshot") -> SurfaceAnimationState()))
  )

  private def modelAt(version: Int): Model =
    Model(
      animating.copy(runtime = animating.runtime.copy(availableThemeNames = List(version.toString))),
      UndoState(),
      Map(BufferId(version) -> AnimationState.empty)
    )

  private def versionOf(state: AppState): List[String] = state.runtime.availableThemeNames

  private def versionOf(animations: Map[BufferId, AnimationState]): List[String] =
    animations.keys.map(_.value.toString).toList

  private def committingBetweenReads(version: Ref[IO, Int]): StateEngine =
    new StateEngine:
      private def next: IO[Model]                                      = version.updateAndGet(_ + 1).map(modelAt)
      def getModel: IO[Model]                                          = next
      def getCurrentState: IO[AppState]                                = next.map(_.app)
      def getBufferAnimations: IO[Map[BufferId, AnimationState]]       = next.map(_.bufferAnimations)
      def updateState(update: AppState => AppState): IO[Unit]          = IO.unit
      def updateStateValidated(update: AppState => AppState): IO[Unit] = IO.unit
      def updateBufferAnimations(update: Map[BufferId, AnimationState] => Map[BufferId, AnimationState]): IO[Unit] =
        IO.unit
      def applyEvent(event: Event): IO[Unit] = IO.unit

  "The fast render phase" should "paint app state and buffer animations from the same model snapshot" in {
    val program =
      for
        version              <- Ref.of[IO, Int](0)
        fastModeSignal       <- SignallingRef.of[IO, Boolean](true)
        pendingDamage        <- Ref.of[IO, Damage](Damage.Nothing)
        pendingPaintDamage   <- Ref.of[IO, Damage](Damage.Nothing)
        animationTickCadence <- Ref.of[IO, AppRuntime.AnimationTickCadence](AppRuntime.AnimationTickCadence.empty)
        painted              <- Ref.of[IO, Vector[(List[String], List[String])]](Vector.empty)
        _ <- AppRuntimeRenderLoops
          .fastRenderPhase(
            committingBetweenReads(version),
            AnimationTicker(advanceAnimationsOnTick = IO.pure(true)),
            fastModeSignal,
            pendingDamage,
            pendingPaintDamage,
            animationTickCadence,
            IO.pure(None),
            IO.unit,
            (
              state: AppState,
              _: Boolean,
              _: Option[Color],
              _: Damage,
              animations: Map[BufferId, AnimationState]
            ) => painted.update(_ :+ (versionOf(state) -> versionOf(animations))),
            _ => IO.unit
          )
          .take(3)
          .compile
          .drain
        frames <- painted.get
      yield frames

    val frames = program.unsafeRunTimed(10.seconds).getOrElse(fail("the fast render phase did not finish"))

    frames should have size 3
    all(frames.map((stateVersion, animationsVersion) => stateVersion == animationsVersion)) shouldBe true
  }
