package com.serenity.ui.terminal

import java.awt.GraphicsEnvironment
import java.util.Locale

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{MenuBarMode, WindowChromeMode}
import com.serenity.diagnostics.FrameTimings
import com.serenity.input.{FileDropHandler, InputRouter}
import com.serenity.keystroke.events.{Event, InsertChar}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.ui.host.{InputSettings, WindowHost}
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Swing window as the host the application is written against: one window answers for painting and for control. */
class SwingWindowHostSpec extends AnyFlatSpec with Matchers:

  private def withHost[A](use: WindowHost => A): A =
    assume(!GraphicsEnvironment.isHeadless, "A Swing window needs a display")
    SwingWindowHost
      .resource(
        SwingWindow.DefaultMetrics,
        SwingWindow.DefaultMetrics,
        WindowChromeMode.Auto,
        None,
        FrameTimings(),
        "host spec"
      )
      .use(host => IO(use(host)))
      .unsafeRunSync()

  "SwingWindowHost" should "serve one window as both the render target and the control" in
    withHost(host => host.target should be theSameInstanceAs host.control)

  it should "report the window's metrics and viewport through the render target" in
    withHost { host =>
      host.target.metrics shouldBe SwingWindow.DefaultMetrics
      host.target.viewportSize.width should be > 0
    }

  it should "offer no menu bar when the mode is hidden" in
    withHost(host => host.menus(MenuBarMode.Hidden) shouldBe empty)

  it should "offer a menu bar on every system but macOS, which has its own" in {
    val onMac = System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("mac")
    withHost(host => host.menus(MenuBarMode.Visible).isDefined shouldBe !onMac)
  }

  it should "take a file-drop handler through the control, not only on the Swing window" in
    withHost(host => noException should be thrownBy host.control.installFileDrop(FileDropHandler(_ => (), _ => ())))

  it should "queue a typed character into the input handler it built, for the start-up probe" in
    withHost { host =>
      val router  = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
      val metrics = () => CellMetrics(8, 16, 13)
      val handler = host.inputHandler(router, InputSettings(metrics, metrics, 3, FrameTimings(), None)).unsafeRunSync()

      host.typeCharacter('a').unsafeRunSync()

      handler.eventStream.take(1L).compile.toList.unsafeRunTimed(10.seconds) shouldBe Some(List(InsertChar('a')))
    }
