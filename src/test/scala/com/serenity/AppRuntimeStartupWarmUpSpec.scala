package com.serenity

import java.awt.Color

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref, Resource}
import com.serenity.app.AppRuntime
import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendRuntime
import com.serenity.input.InputHandler
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.layout.ViewportSize
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}
import org.typelevel.log4cats.{Logger, LoggerFactory}

/** Where the startup warm-up sits in the runtime: after the first frame, behind its config switch. */
class AppRuntimeStartupWarmUpSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = NoOpFactory[IO]
  given Logger[IO]        = NoOpLogger[IO]

  private object SilentInput extends InputHandler[IO]:
    override def keyStrokeInfoStream: Stream[IO, KeyStrokeInfo] = Stream.never
    override def eventStream: Stream[IO, Event]                 = Stream.never
    override def shutdown: IO[Unit]                             = IO.unit

  /** Runs the app until `quitWhen` completes, recording the order of visible and off-screen frames. */
  private def runRecording(config: AppConfig, quitWhen: Ref[IO, Vector[String]] => IO[Unit]): Vector[String] =
    val program = Ref.of[IO, Vector[String]](Vector.empty).flatMap { log =>
      val visible = (
        _: AppState,
        _: Boolean,
        _: Option[Color],
        _: Damage,
        _: RenderCaches
      ) => log.update(_ :+ "visible")
      val offscreen = FrontendRuntime.OffscreenFrames(
        full = (_, _, _) => log.update(_ :+ "offscreen"),
        cursorOnly = (_, _) => IO.unit
      )
      AppRuntime.run(
        initialViewportSize = ViewportSize(100, 30),
        checkResize = IO.pure(None),
        runtime = FrontendRuntime(
          inputHandler = _ => IO.pure(SilentInput),
          renderFull = visible,
          renderCursorOnly = visible,
          offscreenFrames = Some(Resource.pure(offscreen))
        ),
        appConfig = config,
        awaitExternalQuit = quitWhen(log)
      ) >> log.get
    }
    program.unsafeRunTimed(60.seconds).getOrElse(fail("the runtime did not quit"))

  "AppRuntime.run" should "start the warm-up only after the first visible frame" in {
    val untilWarmingUp =
      (log: Ref[IO, Vector[String]]) => (IO.sleep(20.millis) >> log.get).iterateUntil(_.contains("offscreen")).void
    val frames = runRecording(AppConfig.default, untilWarmingUp)

    frames.headOption shouldBe Some("visible")
    frames should contain("offscreen")
  }

  it should "not warm up when startup.warm_up is off" in {
    val frames = runRecording(AppConfig.default.withStartupWarmUp(false), _ => IO.sleep(1.second))

    frames should contain("visible")
    frames should not contain "offscreen"
  }
