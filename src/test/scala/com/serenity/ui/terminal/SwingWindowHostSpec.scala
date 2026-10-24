package com.serenity.ui.terminal

import java.awt.GraphicsEnvironment
import java.util.Locale

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{MenuBarMode, WindowChromeMode}
import com.serenity.diagnostics.FrameTimings
import com.serenity.ui.host.WindowHost
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
