package com.serenity.frontend

import scala.concurrent.duration.*

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

  it should "tick the idle cursor cadence purely from the configured motion speed, regardless of cursor mode" in {
    GuiFrontend.cursorIdleInterval(AppConfig.default) shouldBe Some(500.millis)
    GuiFrontend.cursorIdleInterval(AppConfig.default.withCursorMode(CursorMode.Blink)) shouldBe Some(500.millis)
    GuiFrontend.cursorIdleInterval(AppConfig.default.withCursorTransitionSpeedScale(Some(0.0))) shouldBe None
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

    frontend.cursorIdleInterval(AppConfig.default.withCursorMode(CursorMode.Breathe)) shouldBe Some(500.millis)
  }

  "Frontend.guiLogRouting/tuiLogRouting" should "match what a constructed instance's own logRouting answers" in {
    Frontend.guiLogRouting shouldBe GuiFrontend.logRouting
    Frontend.tuiLogRouting shouldBe TuiFrontend(KeyboardFidelityTier.Full).logRouting
  }
