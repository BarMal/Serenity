package com.serenity.frontend

import com.serenity.keystroke.KeyboardFidelityTier

/** Which grid a frontend lays text out on: a real terminal's fixed character cell, or pixels measured from an actual
  * font. Replaces the binary "is this TUI" question everywhere a reducer/model/manager only ever needed this one fact
  * -- whether to wrap/scroll/hit-test on `CellMetrics.cellUnit` or on `CellMetrics.fromFont` for whatever font a
  * buffer's typography role resolves to. Every real call site still measures its own `CellMetrics` for the font/role it
  * actually cares about (`EditorGeometryProducer`, `CursorViewport`, `MouseTargetCache`) -- this carries no cell-
  * metrics payload of its own, deliberately: a single "the" measurement here could not represent code/prose/UI font
  * roles at once, and a stale or synthetic one would be worse than no payload at all.
  */
enum MetricGrid:
  case Cells
  case Pixels

/** What the pure core (reducers, models) may assume about the frontend currently running, published once into
  * `Runtime.capabilities` at startup (issue #1669) -- never a `Frontend` instance itself, which stays on the effectful
  * shell side as `GuiFrontend`/`TuiFrontend`.
  *
  * Three kinds of concern used to hide behind one plain "is this TUI?" boolean: shell/effect behaviour (cursor-blink
  * scheduling, markdown-preview window vs. pinned panel, log routing -- now [[Frontend.cursorIdleInterval]] and
  * [[Frontend.logRouting]], or a direct `grid` check at the shell boundary), measurement (cell vs. font metrics --
  * [[grid]]), and presentation policy (default gaps, "inert in TUI" hints, caret glide -- [[pixelMotion]],
  * [[typography]], [[postProcessing]]). Splitting them means pure code asks the capability it actually needs ("can this
  * surface animate the caret?") instead of the frontend's identity ("is this TUI?") -- capabilities can already vary
  * *within* TUI (a mintty session's narrower keyboard tier, issue #1532), which a two-way GUI/TUI split cannot express.
  */
final case class FrontendCapabilities(
    grid: MetricGrid,
    // Caret glide and smooth scroll: sub-cell motion a fixed character grid cannot represent (issue #1085).
    pixelMotion: Boolean,
    // Font family/size/ligature pickers paint nothing different on a fixed-cell surface (epic #1103).
    typography: Boolean,
    // Glow/blur/CRT-style post-processing: pixel effects a fixed-cell surface cannot apply (epic #1103).
    postProcessing: Boolean,
    // The keyboard wire protocol actually negotiated (issue #1194/#1320) -- `Full` unconditionally in GUI mode, since
    // a focused Swing window decodes AWT key events directly with no protocol to negotiate.
    keyboardFidelityTier: KeyboardFidelityTier
):
  def isCellGrid: Boolean = grid == MetricGrid.Cells

object FrontendCapabilities:

  /** Every GUI session's capabilities. */
  val gui: FrontendCapabilities =
    FrontendCapabilities(
      grid = MetricGrid.Pixels,
      pixelMotion = true,
      typography = true,
      postProcessing = true,
      keyboardFidelityTier = KeyboardFidelityTier.Full
    )

  /** Every TUI session's capabilities -- `keyboardFidelityTier` is the one field that actually varies per session,
    * following whatever wire protocol `TerminalShell` negotiated with the real terminal.
    */
  def tui(keyboardFidelityTier: KeyboardFidelityTier = KeyboardFidelityTier.Full): FrontendCapabilities =
    FrontendCapabilities(
      grid = MetricGrid.Cells,
      pixelMotion = false,
      typography = false,
      postProcessing = false,
      keyboardFidelityTier = keyboardFidelityTier
    )
