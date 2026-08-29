package com.serenity.diagnostics

import ch.qos.logback.classic.spi.LoggingEvent
import ch.qos.logback.core.spi.FilterReply
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers issue #1215: stdout is the terminal surface `TerminalRenderSurface` owns exclusively in TUI mode, so any
  * console log line racing its ANSI writes corrupts the display and drags the terminal's real cursor away. This filter
  * is `Main.run`'s guard against that -- denying every console event once TUI mode is known, leaving GUI mode (where
  * console output is harmless) untouched.
  */
class TuiConsoleLogFilterSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach:

  private val filter = new TuiConsoleLogFilter
  private val event  = new LoggingEvent()

  override def afterEach(): Unit =
    System.clearProperty(TuiConsoleLogFilter.EnabledProperty)

  "decide" should "deny every console event once TUI mode is signalled" in {
    System.setProperty(TuiConsoleLogFilter.EnabledProperty, "true")

    filter.decide(event) shouldBe FilterReply.DENY
  }

  it should "stay neutral (let logback's own level threshold decide) in GUI mode" in {
    System.setProperty(TuiConsoleLogFilter.EnabledProperty, "false")

    filter.decide(event) shouldBe FilterReply.NEUTRAL
  }

  it should "stay neutral when the property was never set at all" in {
    filter.decide(event) shouldBe FilterReply.NEUTRAL
  }
