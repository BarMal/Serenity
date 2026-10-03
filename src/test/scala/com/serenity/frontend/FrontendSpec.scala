package com.serenity.frontend

import scala.concurrent.duration.*

import cats.effect.IO
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, CursorMode}
import com.serenity.keystroke.KeyboardFidelityTier
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #1669: `GuiFrontend`/`TuiFrontend` are the two `Frontend` instances `Main` chooses between exactly once, each
  * publishing its own `FrontendCapabilities` and owning the shell-level behaviour (cursor-blink scheduling, log
  * routing) that used to be a scattered `isTuiMode` boolean.
  */
class FrontendSpec extends AnyFlatSpec with Matchers:

  "GuiFrontend" should "publish GUI capabilities" in {
    GuiFrontend.capabilities shouldBe FrontendCapabilities.gui
  }

  it should "never suppress the console log appender" in {
    GuiFrontend.logRouting shouldBe LogRouting(suppressConsole = false)
  }

  it should "tick the idle cursor cadence at the fixed blink interval, regardless of cursor mode" in {
    GuiFrontend.cursorIdleInterval(AppConfig.default) shouldBe Some(Frontend.BlinkInterval)
    GuiFrontend.cursorIdleInterval(AppConfig.default.withCursorMode(CursorMode.Blink)) shouldBe Some(
      Frontend.BlinkInterval
    )
  }

  it should "keep blinking when the cursor motion speed is zero, since blink is not a motion setting" in {
    GuiFrontend.cursorIdleInterval(AppConfig.default.withCursorTransitionSpeedScale(Some(0.0))) shouldBe Some(
      Frontend.BlinkInterval
    )
  }

  "Frontend.BlinkInterval" should "stay at the long-standing 500ms" in {
    Frontend.BlinkInterval shouldBe 500.millis
  }

  "TuiFrontend" should "publish TUI capabilities carrying the negotiated keyboard tier" in {
    val frontend = TuiFrontend(KeyboardFidelityTier.ModifyOtherKeys)

    frontend.capabilities shouldBe FrontendCapabilities.tui(KeyboardFidelityTier.ModifyOtherKeys)
  }

  it should "suppress the console log appender -- stdout is the terminal surface it owns exclusively" in {
    TuiFrontend(KeyboardFidelityTier.Full).logRouting shouldBe LogRouting(suppressConsole = true)
  }

  it should "delegate blink-mode cursor idle ticking to the terminal's own hardware cursor" in {
    val frontend = TuiFrontend(KeyboardFidelityTier.Full)

    frontend.cursorIdleInterval(AppConfig.default.withCursorMode(CursorMode.Blink)) shouldBe None
  }

  it should "keep ticking in breathe mode, since a terminal cursor style can't represent colour/opacity animation" in {
    val frontend = TuiFrontend(KeyboardFidelityTier.Full)

    frontend.cursorIdleInterval(AppConfig.default.withCursorMode(CursorMode.Breathe)) shouldBe Some(
      Frontend.BlinkInterval
    )
  }

  "Frontend.guiLogRouting/tuiLogRouting" should "match what a constructed instance's own logRouting answers" in {
    Frontend.guiLogRouting shouldBe GuiFrontend.logRouting
    Frontend.tuiLogRouting shouldBe TuiFrontend(KeyboardFidelityTier.Full).logRouting
  }

  "GuiFrontend" should "never report a spawned Markdown preview window -- the GUI uses the in-app split panel instead" in {
    GuiFrontend.markdownPreviewWindow shouldBe MarkdownPreviewWindowAvailability.Unavailable
  }

  "TuiFrontend" should "default to no Markdown preview window when none is given" in {
    TuiFrontend(KeyboardFidelityTier.Full).markdownPreviewWindow shouldBe MarkdownPreviewWindowAvailability.Unavailable
  }

  it should "carry whatever Markdown preview window availability TuiRuntime resolved at startup" in {
    val fakeWindow = new com.serenity.ui.tui.MarkdownPreviewWindow:
      def show(): IO[Unit]                                           = IO.unit
      def hide(): IO[Unit]                                           = IO.unit
      def updateImage(image: java.awt.image.BufferedImage): IO[Unit] = IO.unit
      def currentSize: IO[(Int, Int)]                                = IO.pure((0, 0))
      def setOnUserClose(callback: () => Unit): Unit                 = ()
    val availability = MarkdownPreviewWindowAvailability.Available(fakeWindow)

    TuiFrontend(
      KeyboardFidelityTier.Full,
      markdownPreviewWindow = availability
    ).markdownPreviewWindow shouldBe availability
  }
