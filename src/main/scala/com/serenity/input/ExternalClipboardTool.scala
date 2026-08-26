package com.serenity.input

import java.nio.file.{Files, Paths}

/** An external CLI clipboard tool detected on `PATH`, for environments with no display and no live terminal writer to
  * send OSC 52 through.
  */
enum ExternalClipboardTool(val writeCommand: List[String], val readCommand: List[String]):
  case WlClipboard extends ExternalClipboardTool(List("wl-copy"), List("wl-paste", "-n"))

  case Xclip
      extends ExternalClipboardTool(
        List("xclip", "-selection", "clipboard"),
        List("xclip", "-selection", "clipboard", "-o")
      )

  case Xsel
      extends ExternalClipboardTool(List("xsel", "--clipboard", "--input"), List("xsel", "--clipboard", "--output"))

object ExternalClipboardTool:

  /** Preference order: the Wayland tool first (both its write and read executables are checked), then the two X11 tools
    * most commonly preinstalled.
    */
  def detect(isOnPath: String => Boolean): Option[ExternalClipboardTool] =
    List(WlClipboard, Xclip, Xsel).find { tool =>
      tool.writeCommand.headOption.exists(isOnPath) && tool.readCommand.headOption.exists(isOnPath)
    }

  def detect(): Option[ExternalClipboardTool] = detect(isExecutableOnPath)

  private[input] def isExecutableOnPath(command: String): Boolean =
    sys.env
      .getOrElse("PATH", "")
      .split(java.io.File.pathSeparator)
      .toList
      .exists(dir => Files.isExecutable(Paths.get(dir, command)))
