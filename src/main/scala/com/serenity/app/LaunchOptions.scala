package com.serenity.app

import java.nio.file.Path

import cats.syntax.all.*
import com.monovore.decline.{Command, Help, Opts}

/** @param showVersion
  *   `--version`: print the build identity and exit without starting the editor. Carried here rather than handled
  *   before parsing so that an unrecognised flag alongside it is still reported, instead of `--version` masking it.
  */
final case class LaunchOptions(
    openPath: Option[Path] = None,
    eco: Boolean = false,
    tui: Boolean = false,
    gui: Boolean = false,
    alpha: Boolean = false,
    showVersion: Boolean = false
)

object LaunchOptions:

  private val open: Opts[Option[Path]] =
    Opts
      .option[Path]("open", "Open this file on startup.", metavar = "path")
      .orElse(Opts.option[Path]("file", "Open this file on startup (alias for --open).", metavar = "path"))
      .orElse(Opts.argument[Path]("path"))
      .orNone

  private val eco: Opts[Boolean] =
    Opts.flag("eco", "Lower the frame-rate target and reduce motion.").orFalse

  private val tui: Opts[Boolean] =
    Opts.flag("tui", "Force the terminal interface.").orFalse

  private val gui: Opts[Boolean] =
    Opts.flag("gui", "Force the windowed interface. Wins over --tui.").orFalse

  private val alpha: Opts[Boolean] =
    Opts.flag("alpha", "Enable gated experimental features.").orFalse

  private val version: Opts[Boolean] =
    Opts.flag("version", "Print the build identity and exit.").orFalse

  val command: Command[LaunchOptions] =
    Command("serenity", "A calm text editor.")((open, eco, tui, gui, alpha, version).mapN(LaunchOptions.apply))

  /** `Left` carries the text to print. `Help.errors` distinguishes the two reasons: empty for a `--help` request,
    * non-empty for an argument the parser rejected -- which is what lets the caller exit zero for one and non-zero for
    * the other.
    *
    * This replaces a hand-rolled parser that swallowed everything it did not understand: `--unknown notes.md` silently
    * opened nothing at all, `--open` with no path started with no file, and a misspelled flag was indistinguishable
    * from a correct one. Issue #1280.
    */
  def parse(args: List[String]): Either[Help, LaunchOptions] =
    command.parse(args, sys.env)

  /** Whether this launch should use the terminal shell rather than Swing.
    *
    * `--gui` always wins when both flags are given -- it exists specifically to force the Swing path even when
    * auto-detection would otherwise pick the terminal (see issue #1112). Absent an explicit flag, the terminal is used
    * only when there is no display to put a window on (`$DISPLAY` and `$WAYLAND_DISPLAY` both unset/empty) *and* stdout
    * is actually a terminal a person can interact with -- a display-less, non-interactive invocation (e.g. a script
    * piping stdout, or a CI job with neither a display nor a pty) falls through to the GUI path rather than silently
    * entering raw terminal mode against a stream that can never supply keystrokes.
    */
  def resolveTuiMode(
    options: LaunchOptions,
    env: Map[String, String] = sys.env,
    stdoutIsTty: Boolean = System.console() != null
  ): Boolean =
    if options.gui then false
    else if options.tui then true
    else detectTuiByDefault(env, stdoutIsTty)

  def detectTuiByDefault(env: Map[String, String], stdoutIsTty: Boolean): Boolean =
    !isDisplayReachable(env) && stdoutIsTty

  /** Whether a display Serenity could put a Swing window on is reachable -- also used by the TUI clipboard strategy
    * (#1111) to choose AWT reuse over a terminal-local fallback.
    */
  def isDisplayReachable(env: Map[String, String]): Boolean =
    env.get("DISPLAY").exists(_.nonEmpty) || env.get("WAYLAND_DISPLAY").exists(_.nonEmpty)
