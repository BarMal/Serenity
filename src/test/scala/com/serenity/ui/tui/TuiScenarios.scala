package com.serenity.ui.tui

import cats.syntax.all.*

import TuiScript.*

/** Openings and manoeuvres more than one scenario needs, as ordinary [[TuiScript]] values.
  *
  * These exist so a spec says what the user is doing ("open the command palette, search for the line-numbers command")
  * rather than repeating the keystrokes that get there -- and so a binding change is one edit rather than twenty.
  */
object TuiScenarios:

  /** Ctrl+P, the terminal binding for the command palette (`HotkeyConfig.forTerminalUse` rewrites the macOS default). */
  val openCommandPalette: TuiScript[Unit] = ctrl('p')

  /** Open the palette and narrow it to commands matching `query`. */
  def searchCommands(query: String): TuiScript[Unit] =
    openCommandPalette >> typeText(query)

  /** Open the palette, search for a command, and run the top match. */
  def runCommand(query: String): TuiScript[Unit] =
    searchCommands(query) >> enter

  /** The dedicated settings surface, reached through the palette the way a user reaches it. */
  val openSettings: TuiScript[Unit] = runCommand("open settings")

  /** Dismiss whatever surface is open, one level at a time, until none is (issue #1059: Escape pops one level). */
  def dismissSurfaces(maxLevels: Int = 8): TuiScript[Unit] =
    def loop(remaining: Int): TuiScript[Unit] =
      state.flatMap { current =>
        if current.runtime.uiSurfaces.isEmpty || remaining <= 0 then TuiScript.unit
        else escape >> loop(remaining - 1)
      }
    loop(maxLevels)

  val save: TuiScript[Unit]   = ctrl('s')
  val saveAs: TuiScript[Unit] = ctrlShift('s')
  val newTab: TuiScript[Unit] = ctrl('t')
  val closeTab: TuiScript[Unit] = ctrl('w')
  val nextTab: TuiScript[Unit]     = press(TuiKeys.CtrlTab)
  val previousTab: TuiScript[Unit] = press(TuiKeys.CtrlShiftTab)
  val undo: TuiScript[Unit]     = ctrl('z')
  val redo: TuiScript[Unit]     = ctrl('y')
  val selectAll: TuiScript[Unit] = ctrl('a')
  val copy: TuiScript[Unit]      = ctrl('c')
  val cut: TuiScript[Unit]       = ctrl('x')
  val pasteClipboard: TuiScript[Unit] = ctrl('v')

  /** Type a document, one line at a time, pressing Enter between lines the way a person would. */
  def typeDocument(lines: String*): TuiScript[Unit] =
    lines.toList.zipWithIndex.traverse_ { (line, index) =>
      (if index == 0 then TuiScript.unit else enter) >> typeText(line)
    }
end TuiScenarios
