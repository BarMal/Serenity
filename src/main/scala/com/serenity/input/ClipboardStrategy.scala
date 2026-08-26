package com.serenity.input

/** The clipboard implementation TUI mode should use, chosen from probed capabilities. */
enum ClipboardStrategy:
  case Awt
  case Osc52
  case ExternalTool(tool: ExternalClipboardTool)
  case InProcess

object ClipboardStrategy:

  /** Prefer a clipboard shared with the rest of the system over anything terminal-local: AWT reuse when a display is
    * reachable, OSC 52 when the terminal shell exposes a writer, an external CLI tool next, and an in-process clipboard
    * scoped to this run of Serenity as the last resort.
    */
  def select(
    hasDisplay: Boolean,
    hasTerminalWriter: Boolean,
    externalTool: Option[ExternalClipboardTool]
  ): ClipboardStrategy =
    if hasDisplay then Awt
    else if hasTerminalWriter then Osc52
    else
      externalTool match
        case Some(tool) => ExternalTool(tool)
        case None       => InProcess
