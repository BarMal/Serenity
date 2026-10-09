package com.serenity.app

import java.nio.file.Path

import cats.syntax.all.*
import com.monovore.decline.{Command, Help, Opts}

/** @param showVersion
  *   `--version`: print the build identity and exit without starting the editor. Carried here rather than handled
  *   before parsing so that an unrecognised flag alongside it is still reported, instead of `--version` masking it.
  * @param safeMode
  *   `--safe-mode`: default settings, no session, no language servers, spell check or project tasks, and nothing on
  *   disk touched. See [[SafeMode]].
  * @param smokeTest
  *   `--smoke-test`: open the window, print [[SmokeTest.readyLine]] once the first frame is painted, then quit as if
  *   the window had been closed. Used by CI to prove a packaged app image starts. Always the windowed interface.
  * @param resetConfig
  *   `--reset-config`: move `config.conf` aside to a timestamped backup before starting. See [[LaunchReset]].
  * @param resetSession
  *   `--reset-session`: move the saved session aside to a timestamped backup before starting. See [[LaunchReset]].
  */
final case class LaunchOptions(
    openPaths: List[Path] = Nil,
    eco: Boolean = false,
    tui: Boolean = false,
    gui: Boolean = false,
    alpha: Boolean = false,
    showVersion: Boolean = false,
    safeMode: Boolean = false,
    smokeTest: Boolean = false,
    resetConfig: Boolean = false,
    resetSession: Boolean = false
):
  def openPath: Option[Path]     = openPaths.headOption
  def extraOpenPaths: List[Path] = openPaths.drop(1)

object LaunchOptions:

  /** `--open`/`--file` first, then any bare paths, which a file manager passes several of for a multi-file selection.
    */
  private val open: Opts[List[Path]] =
    (
      Opts
        .option[Path]("open", "Open this file or folder on startup.", metavar = "path")
        .orElse(Opts.option[Path]("file", "Open this file or folder on startup (alias for --open).", metavar = "path"))
        .orNone,
      Opts.arguments[Path]("path").orEmpty
    ).mapN((flagged, bare) => flagged.toList ++ bare)

  private val eco: Opts[Boolean] =
    Opts.flag("eco", "Lower the frame-rate target.").orFalse

  private val tui: Opts[Boolean] =
    Opts.flag("tui", "Force the terminal interface.").orFalse

  private val gui: Opts[Boolean] =
    Opts.flag("gui", "Force the windowed interface. Wins over --tui.").orFalse

  private val alpha: Opts[Boolean] =
    Opts.flag("alpha", "Enable gated experimental features.").orFalse

  private val version: Opts[Boolean] =
    Opts.flag("version", "Print the build identity and exit.").orFalse

  private val safeMode: Opts[Boolean] =
    (
      Opts
        .flag(
          "safe-mode",
          "Start with default settings and without the saved session, language servers, spell check or project " +
            "tasks. Nothing on disk is changed. Offered automatically when recent starts did not finish starting."
        )
        .orFalse,
      Opts.flag("safe", "Alias for --safe-mode.").orFalse
    ).mapN(_ || _)

  private val smokeTest: Opts[Boolean] =
    Opts
      .flag(
        "smoke-test",
        "Open the window, print a ready line once the first frame is painted, then quit. Used to check a packaged build."
      )
      .orFalse

  private val resetConfig: Opts[Boolean] =
    Opts
      .flag(
        "reset-config",
        "Move config.conf aside to a timestamped backup, then start with default settings."
      )
      .orFalse

  private val resetSession: Opts[Boolean] =
    Opts
      .flag(
        "reset-session",
        "Move the saved session aside to a timestamped backup folder, then start without it."
      )
      .orFalse

  val command: Command[LaunchOptions] =
    Command("serenity", "A calm text editor.")(
      (open, eco, tui, gui, alpha, version, safeMode, smokeTest, resetConfig, resetSession).mapN(LaunchOptions.apply)
    )

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
    * `--smoke-test` always uses the window, as `--gui` does. `--gui` always wins when both flags are given -- it exists
    * specifically to force the Swing path even when auto-detection would otherwise pick the terminal (see issue #1112).
    * Absent an explicit flag, the terminal is used only when there is no display to put a window on (`$DISPLAY` and
    * `$WAYLAND_DISPLAY` both unset/empty) *and* stdout is actually a terminal a person can interact with -- a
    * display-less, non-interactive invocation (e.g. a script piping stdout, or a CI job with neither a display nor a
    * pty) falls through to the GUI path rather than silently entering raw terminal mode against a stream that can never
    * supply keystrokes.
    */
  def resolveTuiMode(
    options: LaunchOptions,
    env: Map[String, String] = sys.env,
    stdoutIsTty: Boolean = System.console() != null,
    osName: String = osNameProperty
  ): Boolean =
    if options.gui || options.smokeTest then false
    else if options.tui then true
    else detectTuiByDefault(env, stdoutIsTty, osName)

  def detectTuiByDefault(
    env: Map[String, String],
    stdoutIsTty: Boolean,
    osName: String = osNameProperty
  ): Boolean =
    !isDisplayReachable(env, osName) && stdoutIsTty

  /** Whether a display Serenity could put a Swing window on is reachable -- also used by the TUI clipboard strategy
    * (#1111) to choose AWT reuse over a terminal-local fallback. `$DISPLAY`/`$WAYLAND_DISPLAY` are X11/Wayland (Linux)
    * signals only; on Windows and macOS a desktop display is always present, so their absence must not force the TUI --
    * otherwise launching from any Windows console (PowerShell/cmd, where those vars are never set) drops into the
    * terminal UI unexpectedly.
    */
  def isDisplayReachable(env: Map[String, String], osName: String = osNameProperty): Boolean =
    if isDesktopDisplayOs(osName) then true
    else env.get("DISPLAY").exists(_.nonEmpty) || env.get("WAYLAND_DISPLAY").exists(_.nonEmpty)

  private def osNameProperty: String = sys.props.getOrElse("os.name", "")

  private def isDesktopDisplayOs(osName: String): Boolean =
    val normalized = osName.toLowerCase
    normalized.contains("win") || normalized.contains("mac") || normalized.contains("darwin")
