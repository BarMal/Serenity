package com.serenity.diagnostics

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.filter.Filter
import ch.qos.logback.core.spi.FilterReply
import com.serenity.frontend.LogRouting
import org.slf4j.{Logger, LoggerFactory}

/** Denies every log event it is attached to. Attached to `logback.xml`'s console appender while Serenity is running in
  * TUI mode (issue #1215). In TUI mode, stdout *is* the terminal surface `TerminalRenderSurface` owns exclusively --
  * JLine's `TerminalBuilder.builder().system(true)` opens the same controlling tty the JVM's console output already
  * writes to, so an ordinary `INFO` log line (startup, LSP status, session auto-save, all frequent in normal use) is
  * plain text landing on the same physical screen the app's own ANSI diff/caret writes control, racing them with no
  * ordering guarantee. That plain write both corrupts visible content and drags the terminal's real cursor to wherever
  * it printed, entirely outside `TerminalRenderSurface.flush`'s own caret bookkeeping -- observed directly as the
  * cursor drifting to a fixed, wrong position and staying there. GUI mode is unaffected: console output there is just
  * terminal noise beside a window the user isn't reading as the app's own display.
  */
final class TuiConsoleLogFilter extends Filter[ILoggingEvent]:

  override def decide(event: ILoggingEvent): FilterReply = FilterReply.DENY

object TuiConsoleLogFilter:

  private val ConsoleAppenderName = "STDOUT"

  /** Attaches the filter to the console appender of the active logback when `routing` says the console is the terminal
    * surface. `Main.launch` calls this before the first application log line: initialising logback here builds its
    * appenders but logs nothing, so no event can reach the terminal before the filter is on (#1215, #1669).
    */
  def installOnActiveLogback(routing: LogRouting): Unit =
    LoggerFactory.getILoggerFactory match
      case context: LoggerContext => install(context, routing)
      case _                      => ()

  /** The filter is built in code with the `LogRouting` in hand, rather than named in `logback.xml` where logback would
    * instantiate it by reflection and leave a static flag as the only way to reach it.
    */
  def install(context: LoggerContext, routing: LogRouting): Unit =
    if routing.suppressConsole then
      Option(context.getLogger(Logger.ROOT_LOGGER_NAME).getAppender(ConsoleAppenderName))
        .foreach(_.addFilter(new TuiConsoleLogFilter))
