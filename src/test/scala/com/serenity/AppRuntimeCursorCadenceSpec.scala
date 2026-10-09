package com.serenity

import java.nio.file.Files

import scala.concurrent.duration.*

import com.serenity.app.AppRuntime
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

  "AppConfig.cursorBlinkTimeout" should "default to ten seconds, after GTK's gtk-cursor-blink-timeout" in {
    AppConfig.default.cursorBlinkTimeout shouldBe Some(10.seconds)
  }

  it should "be absent, so the caret blinks for as long as the window is focused, when set to 0" in {
    AppConfig.default.withCursorBlinkTimeoutMillis(0L).cursorBlinkTimeout shouldBe None
    AppConfig.default.withCursorBlinkTimeoutMillis(-1L).cursorBlinkTimeout shouldBe None
  }

  it should "load from and write to the config file" in {
    val configFile = Files.createTempFile("serenity-cursor-blink-timeout-config", ".conf")
    Files.writeString(
      configFile,
      """editor.cursor.blink_timeout_ms = 3000
        |""".stripMargin
    )

    val config = ConfigManagerTestSupport.loadConfig(Some(configFile.toString))

    config.cursorBlinkTimeout shouldBe Some(3.seconds)
    ConfigManager.configToString(config) should include("editor.cursor.blink_timeout_ms = 3000")
  }

  "AppRuntime.keepsBlinking" should "blink until the time spent blinking reaches the timeout" in {
    val config = AppConfig.default.withCursorBlinkTimeoutMillis(2000L)

    AppRuntime.keepsBlinking(config, 500.millis, 3) shouldBe true
    AppRuntime.keepsBlinking(config, 500.millis, 4) shouldBe false
  }

  it should "never stop blinking when the timeout is 0" in {
    AppRuntime.keepsBlinking(AppConfig.default.withCursorBlinkTimeoutMillis(0L), 500.millis, 10000) shouldBe true
  }
