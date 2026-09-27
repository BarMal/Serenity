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
  * scattered "is this TUI?" branches (cursor-blink scheduling below, log routing, the Markdown preview window); the
  * pure core never sees either, only the immutable [[FrontendCapabilities]] value each publishes into
  * `Runtime.capabilities` at startup.
  *
  * `Frontend` itself stays a cheap, stateless descriptor -- every member here is a plain value or a pure function of
  * `AppConfig`, which is exactly why `GuiFrontend` is a singleton `case object` rather than a class, and is read that
  * way (`GuiFrontend.capabilities`, `GuiFrontend.cursorIdleInterval(config)`, ...) throughout the test suite
  * (`FrontendSpec`, `AppRuntimeCursorCadenceSpec`, `AppRuntimeFocusIdleSpec`, `AppRuntimeIdleCursorRenderSpec`) without
  * ever constructing a real session. `renderSurface`/`inputHandler` from this issue's original sketch are *not* members
  * here for that reason: a real render/input session needs a live `SwingWindow`/`TerminalShell` resource that only
  * exists once `Main.runGui`/`TuiRuntime.run` acquire it, and forcing that liveness onto this type would break the
  * singleton/stateless shape every one of those call sites relies on. That pair is instead [[FrontendRuntime]] -- see
  * its own doc for why it is a sibling type rather than two more members here. `openMarkdownPreview` *is* a member here
  * ([[markdownPreviewWindow]]) -- its availability is exactly as cheap and session-independent as `capabilities` is,
  * carrying only what TUI's startup already resolved rather than a live handle.
  */
sealed trait Frontend:
  def capabilities: FrontendCapabilities
  def logRouting: LogRouting

  /** The idle render phase's per-tick cadence (`AppRuntime.awaitFocusedIdleTick`), or `None` when this frontend has no
    * idle work to do and the loop should wait indefinitely for a real input event instead.
    */
  def cursorIdleInterval(config: AppConfig): Option[FiniteDuration]

  /** Whether this frontend can spawn a Markdown preview window (issue #1113), replacing the
    * `MarkdownPreviewWindowAvailability` that used to reach `StateManager`/`StateManagerPanelEffects` as a bare
    * constructor parameter threaded in from `com.serenity.ui.tui` directly -- a `state.manager` reference
    * `ArchitectureChecks`' #1669 rule now catches for any *new* such reference, with the five pre-existing ones
    * grandfathered in `architecture-baseline.tsv` until this. Held here instead: `state.manager` reads it off the
    * `Frontend` it already takes, rather than importing a concrete frontend's own implementation package.
    */
  def markdownPreviewWindow: MarkdownPreviewWindowAvailability

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
  val capabilities: FrontendCapabilities                       = FrontendCapabilities.gui
  val logRouting: LogRouting                                   = Frontend.guiLogRouting
  val markdownPreviewWindow: MarkdownPreviewWindowAvailability = MarkdownPreviewWindowAvailability.Unavailable

  def cursorIdleInterval(config: AppConfig): Option[FiniteDuration] =
    Frontend.motionDrivenIdleInterval(config)

/** The TUI frontend: the terminal's own fixed cell grid, no sub-cell motion/typography/post-processing (epic #1103's
  * accepted degradations), and whatever keyboard fidelity the terminal actually negotiated (issue #1194/#1320).
  *
  * `keyboardFidelityTier` is the one thing that varies per session -- resolved once from `TerminalShell`'s negotiated
  * protocol tier before this is constructed (see `TuiRuntime.run`/`TuiRuntime.keyboardFidelityTier`).
  */
final case class TuiFrontend(
    keyboardFidelityTier: KeyboardFidelityTier,
    markdownPreviewWindow: MarkdownPreviewWindowAvailability = MarkdownPreviewWindowAvailability.Unavailable
) extends Frontend:
  val capabilities: FrontendCapabilities = FrontendCapabilities.tui(keyboardFidelityTier)
  val logRouting: LogRouting             = Frontend.tuiLogRouting

  /** Issue #1170: in TUI blink mode the caret is delegated to the terminal's own hardware cursor
    * (`RendererCursorOverlay.presentHardwareCursor`), which owns blink timing entirely, so there is no idle work left
    * to do -- breathe mode is the documented exception, since it animates colour/opacity over time, which a terminal
    * cursor style can't represent, so it keeps the normal motion-driven cadence.
    */
  def cursorIdleInterval(config: AppConfig): Option[FiniteDuration] =
    if config.cursorMode == CursorMode.Blink then None else Frontend.motionDrivenIdleInterval(config)
