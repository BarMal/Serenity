package com.serenity.app

import java.nio.file.Path

final case class LaunchOptions(
    openPath: Option[Path] = None,
    eco: Boolean = false,
    tui: Boolean = false,
    gui: Boolean = false
)

object LaunchOptions:

  def parse(args: List[String]): LaunchOptions =
    val eco = args.contains("--eco")
    val tui = args.contains("--tui")
    val gui = args.contains("--gui")
    // --eco/--tui/--gui are bare flags with no value, so they're stripped before the positional/--open/--file
    // matching below -- that logic only looks at the head of the list and shouldn't have to account for their
    // position.
    args.filterNot(arg => arg == "--eco" || arg == "--tui" || arg == "--gui") match
      case "--open" :: path :: _ =>
        LaunchOptions(openPath = Some(Path.of(path)), eco = eco, tui = tui, gui = gui)
      case "--file" :: path :: _ =>
        LaunchOptions(openPath = Some(Path.of(path)), eco = eco, tui = tui, gui = gui)
      case path :: _ if !path.startsWith("-") =>
        LaunchOptions(openPath = Some(Path.of(path)), eco = eco, tui = tui, gui = gui)
      case _ =>
        LaunchOptions(eco = eco, tui = tui, gui = gui)

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
