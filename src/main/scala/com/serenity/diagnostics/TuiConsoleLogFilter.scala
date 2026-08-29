package com.serenity.diagnostics

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.filter.Filter
import ch.qos.logback.core.spi.FilterReply

/** Denies every log event from `logback.xml`'s console appender while Serenity is running in TUI mode (issue #1215). In
  * TUI mode, stdout *is* the terminal surface `TerminalRenderSurface` owns exclusively -- JLine's
  * `TerminalBuilder.builder().system(true)` opens the same controlling tty the JVM's console output already writes to,
  * so an ordinary `INFO` log line (startup, LSP status, session auto-save, all frequent in normal use) is plain text
  * landing on the same physical screen the app's own ANSI diff/caret writes control, racing them with no ordering
  * guarantee. That plain write both corrupts visible content and drags the terminal's real cursor to wherever it
  * printed, entirely outside `TerminalRenderSurface.flush`'s own caret bookkeeping -- observed directly as the cursor
  * drifting to a fixed, wrong position and staying there. GUI mode is unaffected: console output there is just terminal
  * noise beside a window the user isn't reading as the app's own display.
  *
  * [[TuiConsoleLogFilter.Enabled]] is checked per event rather than once at logback's XML-parse time, since `Main.run`
  * only learns whether this launch is TUI mode after parsing CLI args -- well after SLF4J's first
  * `LoggerFactory.getLogger` call has already initialized logback and its appenders. Re-checking here means the
  * property only has to be set before the first event that must actually be suppressed, not before appender
  * construction.
  */
final class TuiConsoleLogFilter extends Filter[ILoggingEvent]:

  override def decide(event: ILoggingEvent): FilterReply =
    if TuiConsoleLogFilter.isTuiMode then FilterReply.DENY else FilterReply.NEUTRAL

object TuiConsoleLogFilter:

  /** The system property `Main.run` (the default package, not `com.serenity`, hence no access-narrowed visibility here)
    * sets from `LaunchOptions.resolveTuiMode` before any TUI-mode logging can occur.
    */
  val EnabledProperty: String = "serenity.tuiMode"

  private[serenity] def isTuiMode: Boolean = System.getProperty(EnabledProperty) == "true"
