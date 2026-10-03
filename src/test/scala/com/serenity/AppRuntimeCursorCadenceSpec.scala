package com.serenity

import scala.concurrent.duration.*

import com.serenity.config.*
import com.serenity.frontend.{GuiFrontend, TuiFrontend}
import com.serenity.keystroke.KeyboardFidelityTier
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AppRuntimeCursorCadenceSpec extends AnyFlatSpec with Matchers:

  private val tui = TuiFrontend(KeyboardFidelityTier.Full)

  "GuiFrontend.cursorIdleInterval" should "blink at the fixed interval" in {
    GuiFrontend.cursorIdleInterval(AppConfig.default) shouldBe Some(500.millis)
  }

  it should "never delegate the caret to a hardware cursor, since a GUI canvas paints its own caret" in {
    GuiFrontend.cursorIdleInterval(AppConfig.default) shouldBe Some(500.millis)
    GuiFrontend.cursorIdleInterval(AppConfig.default.withCursorMode(CursorMode.Blink)) shouldBe Some(500.millis)
  }

  "TuiFrontend.cursorIdleInterval" should
    "delegate the caret to the terminal's own cursor in TUI blink mode, eliding the idle cadence entirely" in {
      // #1170: the terminal owns blink timing for the caret in TUI mode, so the idle phase has
      // nothing left to tick for -- outside TUI mode the same config still ticks, since a GUI caret is always
      // app-painted.
      tui.cursorIdleInterval(AppConfig.default) shouldBe None
      GuiFrontend.cursorIdleInterval(AppConfig.default) shouldBe Some(500.millis)
      tui.cursorIdleInterval(AppConfig.default.withCursorMode(CursorMode.Blink)) shouldBe None
    }
