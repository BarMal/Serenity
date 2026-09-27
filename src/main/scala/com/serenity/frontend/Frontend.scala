package com.serenity.frontend

import scala.concurrent.duration.*

import com.serenity.config.{AppConfig, CursorMode}
import com.serenity.keystroke.KeyboardFidelityTier

/** Whether the console (STDOUT) log appender should be denied entirely -- issue #1215: in TUI mode stdout is the
  * terminal surface `TerminalRenderSurface` owns exclusively, and an ordinary console log line racing its ANSI writes
  * corrupts the display and drags the terminal's real cursor away. See `TuiConsoleLogFilter`'s own doc for why a static
  * flag, not a constructor argument, is still how this actually reaches the filter.
  */
final case class LogRouting(suppressConsole: Boolean)

/** Selected exactly once, in `Main`, and injected into the effectful shell (issue #1669) -- nothing downstream asks "am
  * I TUI?" again. `GuiFrontend`/`TuiFrontend` hold the shell/effect-boundary behaviour that used to be scattered
  * scattered "is this TUI?" branches (cursor-blink scheduling below, log routing); the pure core never sees either,
  * only the immutable [[FrontendCapabilities]] value each publishes into `Runtime.capabilities` at startup.
  *
  * `Frontend` deliberately does not also own `renderSurface`/`inputHandler`/`openMarkdownPreview` as trait methods the
  * way this issue's original sketch proposed: `AppRuntime.run`'s Swing and terminal call sites (`Main.runGui`,
  * `TuiRuntime.run`) already construct those -- a real `SwingWindow`/`TerminalShell` resource, their own resize/focus
  * callback wiring, their own input handler construction -- in ways different enough (a window vs. a terminal session)
  * that collapsing them into one generic trait method `AppRuntime.run` calls polymorphically would be a separate,
  * materially larger rewrite of the runtime's own IO plumbing, not a mechanical extraction. That consolidation is left
  * as explicit follow-up scope (see this issue's PR discussion) rather than attempted partially here.
  */
sealed trait Frontend:
  def capabilities: FrontendCapabilities
  def logRouting: LogRouting

  /** The idle render phase's per-tick cadence (`AppRuntime.awaitFocusedIdleTick`), or `None` when this frontend has no
    * idle work to do and the loop should wait indefinitely for a real input event instead.
    */
  def cursorIdleInterval(config: AppConfig): Option[FiniteDuration]

object Frontend:
  private[frontend] val DefaultCursorIdleInterval: FiniteDuration = 500.millis

  /** The cursor-motion-driven idle cadence shared by both frontends: `None` when cursor motion is disabled or scaled to
    * zero (accessibility, the `Reduced` preset), otherwise the default interval scaled by the configured speed.
    */
  private[frontend] def motionDrivenIdleInterval(config: AppConfig): Option[FiniteDuration] =
    val cursorMotion =
      config.surfaceConfig.effectiveMotionConfiguration.family(com.serenity.config.MotionFamily.Cursor)
    val scale = AppConfig.clampElementTransitionSpeedScale(cursorMotion.speedScale)
    Option.when(cursorMotion.enabled && scale > 0.0)(
      FiniteDuration(
        math.max(1L, math.round(DefaultCursorIdleInterval.toNanos.toDouble * scale)),
        NANOSECONDS
      )
    )

  /** The `LogRouting` a GUI launch configures the console filter with, before a `GuiFrontend` instance necessarily
    * exists yet -- `Main.launch` sets this ahead of the first SLF4J `getLogger` call, well before the Swing window (and
    * so the real `GuiFrontend`) is constructed. A `val`, not a case object member lookup, so reading it never requires
    * building a `GuiFrontend`.
    */
  val guiLogRouting: LogRouting = LogRouting(suppressConsole = false)

  /** The TUI counterpart to [[guiLogRouting]], read the same way before a `TuiFrontend` exists. */
  val tuiLogRouting: LogRouting = LogRouting(suppressConsole = true)

/** The GUI frontend: a real font-measured pixel grid, full motion/typography/post-processing, and a cursor-blink
  * cadence driven purely by the configured motion family -- a focused Swing window has no hardware cursor to delegate
  * blink timing to, unlike a real terminal (see [[TuiFrontend]]).
  */
case object GuiFrontend extends Frontend:
  val capabilities: FrontendCapabilities = FrontendCapabilities.gui
  val logRouting: LogRouting             = Frontend.guiLogRouting

  def cursorIdleInterval(config: AppConfig): Option[FiniteDuration] =
    Frontend.motionDrivenIdleInterval(config)

/** The TUI frontend: the terminal's own fixed cell grid, no sub-cell motion/typography/post-processing (epic #1103's
  * accepted degradations), and whatever keyboard fidelity the terminal actually negotiated (issue #1194/#1320).
  *
  * `keyboardFidelityTier` is the one thing that varies per session -- resolved once from `TerminalShell`'s negotiated
  * protocol tier before this is constructed (see `TuiRuntime.run`/`TuiRuntime.keyboardFidelityTier`).
  */
final case class TuiFrontend(keyboardFidelityTier: KeyboardFidelityTier) extends Frontend:
  val capabilities: FrontendCapabilities = FrontendCapabilities.tui(keyboardFidelityTier)
  val logRouting: LogRouting             = Frontend.tuiLogRouting

  /** Issue #1170: in TUI blink mode the caret is delegated to the terminal's own hardware cursor
    * (`RendererCursorOverlay.presentHardwareCursor`), which owns blink timing entirely, so there is no idle work left
    * to do -- breathe mode is the documented exception, since it animates colour/opacity over time, which a terminal
    * cursor style can't represent, so it keeps the normal motion-driven cadence.
    */
  def cursorIdleInterval(config: AppConfig): Option[FiniteDuration] =
    if config.cursorMode == CursorMode.Blink then None else Frontend.motionDrivenIdleInterval(config)
