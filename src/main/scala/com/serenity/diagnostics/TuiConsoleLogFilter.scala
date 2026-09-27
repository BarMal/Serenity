package com.serenity.diagnostics

import java.util.concurrent.atomic.AtomicBoolean

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.filter.Filter
import ch.qos.logback.core.spi.FilterReply
import com.serenity.frontend.LogRouting

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
  * [[TuiConsoleLogFilter.isSuppressed]] is checked per event rather than once at logback's XML-parse time, since
  * `Main.run` only learns which frontend this launch selected after parsing CLI args -- well after SLF4J's first
  * `LoggerFactory.getLogger` call has already initialized logback and its appenders. Re-checking here means
  * [[configure]] only has to run before the first event that must actually be suppressed, not before appender
  * construction.
  */
final class TuiConsoleLogFilter extends Filter[ILoggingEvent]:

  override def decide(event: ILoggingEvent): FilterReply =
    if TuiConsoleLogFilter.isSuppressed then FilterReply.DENY else FilterReply.NEUTRAL

object TuiConsoleLogFilter:

  /** A plain static flag, not a JVM system property (issue #1669) -- `logback.xml` instantiates this filter by
    * reflection from `<filter class="...">`, with no way to inject a `Frontend`/`LogRouting` value into that
    * construction, so some global mutable state here is unavoidable. What issue #1669 actually retires is the
    * stringly-typed `System.setProperty`/`getProperty` pair: [[configure]] takes a real [[LogRouting]] value sourced
    * from the selected `Frontend` (`Frontend.guiLogRouting`/`tuiLogRouting`, read in `Main.launch` before a concrete
    * `GuiFrontend`/`TuiFrontend` instance exists), not an arbitrary string any code could set.
    */
  private val suppressConsole = new AtomicBoolean(false)

  /** `Main.launch` calls this with the selected frontend's `LogRouting` before the very first SLF4J
    * `LoggerFactory.getLogger` call, which triggers logback's one-time console-appender setup -- must happen before the
    * first event a TUI launch could otherwise leak onto the terminal surface it is about to take over.
    */
  def configure(routing: LogRouting): Unit = suppressConsole.set(routing.suppressConsole)

  private[serenity] def isSuppressed: Boolean = suppressConsole.get()
