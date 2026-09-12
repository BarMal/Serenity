package com.serenity

import scala.concurrent.duration.*

import com.serenity.app.AppRuntime
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AppRuntimeCursorCadenceSpec extends AnyFlatSpec with Matchers:

  "AppRuntime" should "derive cursor idle cadence from the cursor motion speed scale" in {
    AppRuntime.cursorIdleInterval(AppConfig.default) shouldBe Some(500.millis)
    AppRuntime.cursorIdleInterval(AppConfig.default.withElementTransitionSpeedScale(2.0)) shouldBe Some(1000.millis)
    AppRuntime.cursorIdleInterval(
      AppConfig.default
        .withElementTransitionSpeedScale(2.0)
        .withCursorTransitionSpeedScale(Some(0.5))
    ) shouldBe Some(250.millis)
    AppRuntime.cursorIdleInterval(AppConfig.default.withCursorTransitionSpeedScale(Some(0.0))) shouldBe None
    AppRuntime.cursorIdleInterval(
      AppConfig.default.withMotionAccessibility(MotionAccessibility.Off)
    ) shouldBe None
  }

  it should "delegate the caret to the terminal's own cursor in TUI blink mode, eliding the idle cadence entirely" in {
    // #1170: the terminal owns blink timing for the normal (non-breathe) caret in TUI mode, so the idle phase has
    // nothing left to tick for -- outside TUI mode the same config still ticks, since a GUI caret is always
    // app-painted.
    AppRuntime.cursorIdleInterval(AppConfig.default, isTuiMode = true) shouldBe None
    AppRuntime.cursorIdleInterval(AppConfig.default, isTuiMode = false) shouldBe Some(500.millis)
    AppRuntime.cursorIdleInterval(AppConfig.default.withCursorMode(CursorMode.Blink), isTuiMode = true) shouldBe None
  }

  it should "keep the cursor idle cadence in TUI breathe mode, since breathe genuinely needs app ticks" in {
    // Breathe animates color/opacity over time -- a terminal cursor style can't represent that -- so it stays the
    // documented, explicit exception to #1170's terminal-delegated caret.
    AppRuntime.cursorIdleInterval(
      AppConfig.default.withCursorMode(CursorMode.Breathe),
      isTuiMode = true
    ) shouldBe Some(500.millis)
  }
