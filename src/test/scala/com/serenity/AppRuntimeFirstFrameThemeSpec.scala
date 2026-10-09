package com.serenity

import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.app.AppRuntime
import com.serenity.config.{AppConfig, ThemeFollowConfig}
import com.serenity.frontend.FrontendRuntime
import com.serenity.input.InputHandler
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event
import com.serenity.rope.Balance
import com.serenity.state.manager.{RenderCaches, StateManager}
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.appearance.{OsAppearance, OsAppearanceDetector}
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}
import org.typelevel.log4cats.{Logger, LoggerFactory}

/** With `theme.follow_system` on, the first frame is drawn in the theme the OS asks for: the saved theme is never on
  * screen for a frame first.
  */
class AppRuntimeFirstFrameThemeSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = NoOpFactory[IO]
  given Logger[IO]        = NoOpLogger[IO]

  private object SilentInput extends InputHandler[IO]:
    override def keyStrokeInfoStream: Stream[IO, KeyStrokeInfo] = Stream.never
    override def eventStream: Stream[IO, Event]                 = Stream.never
    override def shutdown: IO[Unit]                             = IO.unit

  private def counting(appearance: IO[OsAppearance], reads: Ref[IO, Int]): OsAppearanceDetector =
    new OsAppearanceDetector:
      def detect: IO[OsAppearance] = reads.update(_ + 1) >> appearance

  /** The theme of the first frame drawn, and how many times the OS was asked. */
  private def firstFrame(followSystem: Boolean, appearance: IO[OsAppearance]): (String, Int) =
    val program =
      (Ref.of[IO, Vector[String]](Vector.empty), Ref.of[IO, Int](0)).flatMapN { (themes, reads) =>
        val visible = (
          state: AppState,
          _: Boolean,
          _: Option[RenderColor],
          _: Damage,
          _: RenderCaches
        ) => themes.update(_ :+ state.persisted.theme.name)
        val config = AppConfig.default.withThemeFollowConfig(ThemeFollowConfig(followSystem = followSystem))
        val manager = (logger: Logger[IO]) =>
          IO.blocking(Files.createTempDirectory("first-frame-theme")).flatMap { root =>
            StateManager(
              logger,
              sessionRootOverride = Some(root),
              initialConfig = config,
              appearanceDetector = counting(appearance, reads)
            )
          }
        AppRuntime.run(
          initialViewportSize = ViewportSize(100, 30),
          checkResize = IO.pure(None),
          runtime = FrontendRuntime(
            inputHandler = _ => IO.pure(SilentInput),
            renderFull = visible,
            renderCursorOnly = visible
          ),
          appConfig = config.withStartupWarmUp(false),
          makeStateManager = Some(manager),
          awaitExternalQuit = (IO.sleep(20.millis) >> themes.get).iterateUntil(_.nonEmpty).void
        ) >> (themes.get, reads.get).tupled
      }
    val (themes, reads) = program
      .unsafeRunTimed(20.seconds)
      .getOrElse(fail("startup did not reach a first frame within 20 seconds"))
    (themes.headOption.getOrElse(fail("no frame was drawn")), reads)

  "The first frame with follow_system on" should "be drawn in the light theme when the OS is light" in {
    firstFrame(followSystem = true, IO.pure(OsAppearance.Light))._1 shouldBe "light"
  }

  it should "be drawn in the high-contrast theme when the OS asks for high contrast" in {
    firstFrame(followSystem = true, IO.pure(OsAppearance.HighContrast))._1 shouldBe "high-contrast"
  }

  it should "be drawn in the saved theme when the OS does not say" in {
    firstFrame(followSystem = true, IO.pure(OsAppearance.Unknown))._1 shouldBe "dark"
  }

  it should "still be drawn, in the saved theme, when the detector never answers" in {
    firstFrame(followSystem = true, IO.never)._1 shouldBe "dark"
  }

  "The first frame with follow_system off" should "be drawn in the saved theme without asking the OS" in {
    firstFrame(followSystem = false, IO.pure(OsAppearance.Light)) shouldBe (("dark", 0))
  }
