package com.serenity.ui.tui

import java.io.PrintWriter
import java.util.concurrent.atomic.AtomicReference

import cats.effect.{IO, Resource}
import com.serenity.ui.layout.ViewportSize
import org.jline.terminal.Terminal.Signal
import org.jline.terminal.{Terminal, TerminalBuilder}
import org.jline.utils.InfoCmp.Capability

/** Owns a real JLine terminal for the lifetime of TUI mode: raw input, the alternate screen buffer, a hidden hardware
  * cursor, and `SIGWINCH`/`SIGINT` signal handling. Acquired and released as a [[Resource]] so the terminal is always
  * restored to its original state -- on clean shutdown, on an escaping error, and on fiber cancellation alike, since
  * `Resource`'s release runs on every one of those outcomes.
  */
final class TerminalShell private (
    private[tui] val terminal: Terminal,
    private val originalAttributes: org.jline.terminal.Attributes
):

  private val pendingResize  = new AtomicReference[Option[ViewportSize]](None)
  private val resizeCallback = new AtomicReference[Option[() => Unit]](None)
  private val quitSignalled  = new AtomicReference[Option[() => Unit]](None)

  /** The writer [[com.serenity.ui.tui.TerminalRenderSurface]] flushes damage-diffed ANSI bytes through. */
  def writer: PrintWriter = terminal.writer()

  def viewportSize: IO[ViewportSize] = IO(currentViewportSize())

  private def currentViewportSize(): ViewportSize =
    val size = terminal.getSize
    ViewportSize(size.getColumns, size.getRows)

  /** Drains and reports the most recent size a `SIGWINCH` handler observed since the last call, satisfying
    * `AppRuntime.run`'s `checkResize: IO[Option[ViewportSize]]` capability -- `None` when nothing has changed.
    */
  val checkResize: IO[Option[ViewportSize]] = IO(pendingResize.getAndSet(None))

  /** Satisfies `AppRuntime.run`'s `registerResizeCallback: (() => Unit) => Unit` capability: the runtime's own resize
    * bridge is invoked, in addition to updating [[checkResize]]'s pending value, whenever `SIGWINCH` fires.
    */
  def registerResizeCallback(callback: () => Unit): Unit = resizeCallback.set(Some(callback))

  /** Completes once `SIGINT` (Ctrl+C at the raw terminal level, since raw mode disables the kernel's own INT-to-signal
    * delivery via canonical mode) is observed, satisfying `AppRuntime.run`'s `awaitExternalQuit: IO[Unit]` capability
    * so a Ctrl+C still drives a graceful shutdown -- and, via the enclosing [[Resource]], guaranteed terminal
    * restoration -- rather than leaving the terminal corrupted.
    */
  val awaitExternalQuit: IO[Unit] = IO.async_(callback => quitSignalled.set(Some(() => callback(Right(())))))

  private[tui] def handleWinch(): Unit =
    val size = currentViewportSize()
    pendingResize.set(Some(size))
    resizeCallback.get().foreach(_.apply())

  private[tui] def handleInt(): Unit =
    quitSignalled.get().foreach(_.apply())

  private[tui] def restore(): Unit =
    terminal.setAttributes(originalAttributes)
    val _ = terminal.puts(Capability.cursor_normal)
    val _ = terminal.puts(Capability.exit_ca_mode)
    terminal.flush()

object TerminalShell:

  /** Acquire a real system terminal in raw mode with the alternate screen active and the hardware cursor hidden;
    * unconditionally restore it -- attributes, screen buffer, and cursor visibility -- on release.
    */
  def resource: Resource[IO, TerminalShell] =
    Resource
      .make(IO.blocking(TerminalBuilder.builder().system(true).nativeSignals(true).build()))(terminal =>
        IO.blocking(terminal.close()).attempt.void
      )
      .flatMap(forTerminal)

  /** Build a shell over an already-constructed [[Terminal]] -- the real system terminal in production, or a
    * streams-backed test terminal in specs -- entering raw mode / alternate screen / hidden cursor on acquire and
    * restoring them unconditionally on release. Does not close `terminal`; the caller owns that (see [[resource]]).
    */
  private[tui] def forTerminal(terminal: Terminal): Resource[IO, TerminalShell] =
    Resource.make(IO.blocking(acquire(terminal)))(shell => IO.blocking(shell.restore()).attempt.void)

  private def acquire(terminal: Terminal): TerminalShell =
    val originalAttributes = terminal.enterRawMode()
    val _                  = terminal.puts(Capability.enter_ca_mode)
    val _                  = terminal.puts(Capability.cursor_invisible)
    terminal.flush()
    val shell = new TerminalShell(terminal, originalAttributes)
    val _     = terminal.handle(Signal.WINCH, _ => shell.handleWinch())
    val _     = terminal.handle(Signal.INT, _ => shell.handleInt())
    shell
