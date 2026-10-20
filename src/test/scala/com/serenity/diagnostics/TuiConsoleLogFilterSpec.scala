package com.serenity.diagnostics

import scala.jdk.CollectionConverters.*

import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.classic.spi.{ILoggingEvent, LoggingEvent}
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.core.Appender
import ch.qos.logback.core.spi.FilterReply
import com.serenity.frontend.LogRouting
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger

/** Covers issue #1215: stdout is the terminal surface `TerminalRenderSurface` owns exclusively in TUI mode, so any
  * console log line racing its ANSI writes corrupts the display and drags the terminal's real cursor away. `Main`
  * attaches [[TuiConsoleLogFilter]] to the console appender when the selected frontend's `LogRouting` says so, leaving
  * GUI mode (where console output is harmless) untouched.
  */
class TuiConsoleLogFilterSpec extends AnyFlatSpec with Matchers:

  private val event = new LoggingEvent()

  private def packagedConfiguration(): LoggerContext =
    val context      = LoggerContext()
    val configurator = JoranConfigurator()
    configurator.setContext(context)
    configurator.doConfigure(getClass.getClassLoader.getResource("logback.xml"))
    context

  private def appender(context: LoggerContext, name: String): Appender[ILoggingEvent] =
    Option(context.getLogger(Logger.ROOT_LOGGER_NAME).getAppender(name)).getOrElse(fail(s"no $name appender"))

  "The filter" should "deny every event it sees" in {
    new TuiConsoleLogFilter().decide(event) shouldBe FilterReply.DENY
  }

  "The packaged logback.xml" should "attach no filter to the console appender itself" in {
    appender(packagedConfiguration(), "STDOUT").getCopyOfAttachedFiltersList.asScala shouldBe empty
  }

  "install" should "deny every console event in TUI mode" in {
    val context = packagedConfiguration()

    TuiConsoleLogFilter.install(context, LogRouting(suppressConsole = true))

    appender(context, "STDOUT").getFilterChainDecision(event) shouldBe FilterReply.DENY
  }

  it should "leave the app log file appender alone in TUI mode" in {
    val context = packagedConfiguration()

    TuiConsoleLogFilter.install(context, LogRouting(suppressConsole = true))

    appender(context, "APP_FILE").getFilterChainDecision(event) shouldBe FilterReply.NEUTRAL
  }

  it should "leave the console appender alone in GUI mode" in {
    val context = packagedConfiguration()

    TuiConsoleLogFilter.install(context, LogRouting(suppressConsole = false))

    appender(context, "STDOUT").getFilterChainDecision(event) shouldBe FilterReply.NEUTRAL
  }

  it should "do nothing when there is no console appender to attach to" in {
    val context = LoggerContext()

    noException should be thrownBy TuiConsoleLogFilter.install(context, LogRouting(suppressConsole = true))
  }

  "installOnActiveLogback" should "reach the console appender of the logback the application logs through" in {
    val active = org.slf4j.LoggerFactory.getILoggerFactory match
      case context: LoggerContext => context
      case other                  => fail(s"expected logback, got ${other.getClass}")
    val console = appender(active, "STDOUT")

    try
      TuiConsoleLogFilter.installOnActiveLogback(LogRouting(suppressConsole = true))

      console.getFilterChainDecision(event) shouldBe FilterReply.DENY
    finally console.clearAllFilters()
  }
