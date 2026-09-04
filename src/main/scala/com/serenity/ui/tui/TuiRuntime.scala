package com.serenity.ui.tui

import java.awt.Font
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.serenity.app.AppRuntime
import com.serenity.config.AppConfig
import com.serenity.input.{
  ClipboardStrategy,
  ExternalClipboardTool,
  ExternalToolClipboard,
  InProcessClipboard,
  Osc52Clipboard,
  SystemClipboard
}
import com.serenity.keystroke.KeyboardFidelityTier
import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{AppState, Buffer, BufferId, Damage}
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.Renderer
import org.typelevel.log4cats.{Logger, LoggerFactory}

/** The TUI capability bundle for `AppRuntime.run` (issue #1112): the terminal-mode counterpart to `Main`'s Swing
  * wiring, built from #1104-#1108's already-merged pieces (surface-generic renderer entry points, cell fallback, screen
  * buffer, JLine shell, input decoder) plus #1110's in-app save-as fallback and #1111's clipboard strategies.
  *
  * `shell` is a `Resource[IO, TerminalShell]` rather than a concrete `TerminalShell` so this can run over the real
  * system terminal in production (`TerminalShell.resource`) or a JLine `DumbTerminal` wired to in-memory streams in
  * specs (`TerminalShell.forTerminal`), exercising the exact same wiring either way -- this object owns no Swing window
  * reference at all, so the TUI launch path structurally cannot construct one.
  */
object TuiRuntime:

  /** A monospaced default font: `TerminalRenderSurface.fontRenderContext` is always `None` (#1105's cell-fallback
    * trigger), so no glyph is ever measured or drawn from this font's metrics -- it exists only to satisfy the renderer
    * entry points' signatures, which is also why code/text/UI all share the one instance rather than resolving the
    * user's configured (and, in TUI mode, inert) font family/size.
    */
  private val CellFont: Font = new Font(Font.MONOSPACED, Font.PLAIN, 12)

  private val CellMetricsOne: CellMetrics = CellMetrics.cellUnit

  def run(
    shell: Resource[IO, TerminalShell],
    appConfig: AppConfig,
    openPath: Option[Path],
    configPersistencePath: Option[Path],
    hasDisplay: Boolean,
    sessionRootOverride: Option[Path] = None,
    configNotice: Option[String] = None
  )(using logger: Logger[IO], loggerFactory: LoggerFactory[IO], balance: com.serenity.rope.Balance): IO[Unit] =
    // #1213: a real terminal cannot deliver Cmd/Meta as an ordinary keystroke the way AWT does for a focused Swing
    // window, so any hotkey still at its macOS/Cmd-conditioned platform default (Quit, Save, ...) is rewritten here
    // to the Ctrl-based binding every terminal actually forwards, before that config reaches the input translator or
    // the running session's own state -- see `HotkeyConfig.forTerminalUse`.
    val terminalConfig =
      appConfig.withHotkeyConfig(appConfig.inputConfig.hotkeyConfig.forTerminalUse)
    (shell, markdownPreviewWindowResource(hasDisplay)).tupled.use {
      case (terminalShell, previewWindowAvailability) =>
        for
          initialViewportSize <- terminalShell.viewportSize
          surfaceHolder = new SurfaceHolder(terminalShell)
          systemClipboard <- buildClipboard(terminalShell, hasDisplay)
          // TerminalInputHandler decodes terminal focus reporting (CSI I/CSI O, #1171) and is only constructed once
          // `makeInputHandler` runs -- which `AppRuntime.run` sequences before its own `registerFocusCallback` call --
          // so this holder is always populated by the time that callback registration reaches into it.
          inputHandlerHolder <- IO(new AtomicReference[Option[TerminalInputHandler]](None))
          _ <- AppRuntime.run(
            initialViewportSize = initialViewportSize,
            makeInputHandler = router =>
              TerminalInputHandler
                .create(
                  terminalShell.terminal,
                  router,
                  systemClipboard,
                  terminalShell.pendingInputPrefix,
                  terminalConfig.inputConfig.wheelScrollLines
                )
                .map { handler =>
                  inputHandlerHolder.set(Some(handler))
                  handler
                },
            checkResize = terminalShell.checkResize,
            renderFull = renderFullFn(surfaceHolder, terminalShell, previewWindowAvailability),
            renderCursorOnly = renderCursorOnlyFn(surfaceHolder, terminalShell, previewWindowAvailability),
            appConfig = terminalConfig,
            registerFocusCallback = cb => inputHandlerHolder.get().foreach(_.registerFocusCallback(cb)),
            makeStateManager = Some(
              makeStateManager(terminalConfig, sessionRootOverride, configPersistencePath, previewWindowAvailability)
            ),
            awaitExternalQuit = terminalShell.awaitExternalQuit,
            registerResizeCallback = cb => terminalShell.registerResizeCallback(cb),
            registerMarkdownPreviewCloseCallback = registerMarkdownPreviewCloseCallbackFn(previewWindowAvailability),
            openPath = openPath,
            systemClipboard = systemClipboard,
            isTuiMode = true,
            keyboardFidelityTier = keyboardFidelityTier(terminalShell.keyboardProtocolTier),
            configNotice = configNotice
          )
        yield ()
    }

  /** The `StateManager` a TUI session runs on: a terminal has no device pixel ratio, no window to resize, no typography
    * to resync and no native file dialog, so those four capabilities are inert stubs rather than real providers. Named
    * rather than inlined into [[run]] so a spec can build the same state manager a real TUI session gets instead of
    * approximating one.
    */
  private[tui] def makeStateManager(
    terminalConfig: AppConfig,
    sessionRootOverride: Option[Path],
    configPersistencePath: Option[Path],
    previewWindowAvailability: MarkdownPreviewWindowAvailability
  )(using LoggerFactory[IO], com.serenity.rope.Balance): Logger[IO] => IO[StateManager] =
    logger =>
      StateManager.apply(
        logger,
        initialConfig = terminalConfig,
        sessionRootOverride = sessionRootOverride,
        configPersistencePath = configPersistencePath,
        deviceTextScaleProvider = IO.pure(1.0),
        windowSizeProvider = IO.pure(None),
        onPreferredWindowSizeChanged = _ => IO.unit,
        onFontConfigChanged = _ => IO.unit,
        // None: no native dialog exists in a terminal. StateManager's save-as/open workflow already falls back to the
        // in-app form (#1110) whenever fileDialog is None, distinct from a dialog being shown and cancelled.
        fileDialog = None,
        markdownPreviewWindow = previewWindowAvailability
      )

  /** Maps #1109's negotiated wire-protocol tier onto the state layer's fidelity concept (issue #1194) --
    * `TerminalShell.KeyboardProtocolTier` is this method's only caller outside `ui.tui`, kept from leaking into
    * `AppState`/`CommandRunner` themselves so neither depends on the terminal-negotiation package.
    *
    * `Legacy` folds into [[KeyboardFidelityTier.ModifyOtherKeys]] rather than a dedicated case: no CSI-u sequence of
    * either shape ever arrives under `Legacy`, so it is strictly less capable than a confirmed `ModifyOtherKeys`
    * terminal, but `KeyboardFidelityTier` (and the recording-time warning built on it, `CommandRunnerReducer`) only
    * distinguishes "bare-modifier bindings work" from "they don't" -- ordinary combos not decoding either is a
    * pre-existing gap this tier-confirmation follow-up doesn't newly introduce, and widening `KeyboardFidelityTier`
    * itself is out of scope here.
    */
  private[tui] def keyboardFidelityTier(tier: TerminalShell.KeyboardProtocolTier): KeyboardFidelityTier =
    tier match
      case TerminalShell.KeyboardProtocolTier.Kitty           => KeyboardFidelityTier.Full
      case TerminalShell.KeyboardProtocolTier.ModifyOtherKeys => KeyboardFidelityTier.ModifyOtherKeys
      case TerminalShell.KeyboardProtocolTier.Legacy          => KeyboardFidelityTier.ModifyOtherKeys

  /** Holds the one [[TerminalRenderSurface]] live for the current viewport size, rebuilding it (and so resetting its
    * damage-diff history) whenever the size actually changes -- a terminal resize warrants a full repaint anyway, so
    * losing the previous frame's diff state on that transition is the correct behaviour, not a gap.
    */
  final private[tui] class SurfaceHolder(shell: TerminalShell):
    private val current = new AtomicReference[Option[(ViewportSize, TerminalRenderSurface)]](None)

    def forSize(size: ViewportSize): TerminalRenderSurface =
      current.get() match
        case Some((existingSize, surface)) if existingSize == size => surface
        case _ =>
          val surface = new TerminalRenderSurface(size.width, size.height, shell.writer, CellMetricsOne)
          current.set(Some((size, surface)))
          surface

  /** The full-frame paint itself, separated from the `RenderFn` that sequences it so a spec can drive exactly the
    * renderer call a real TUI session makes -- same entry points, same inert cell font, same unit cell metrics --
    * rather than a copy of it that can drift from what production actually paints.
    */
  private[tui] def paintFrame(
    state: AppState,
    surface: TerminalRenderSurface,
    size: ViewportSize,
    cursorVisible: Boolean,
    cursorColor: Option[java.awt.Color],
    damage: Damage
  ): Unit =
    if cursorVisible then
      val _ = Renderer.renderWithCursorOverlay(
        state,
        surface,
        size,
        CellFont,
        CellFont,
        CellFont,
        CellMetricsOne,
        CellMetricsOne,
        cursorColor,
        damage
      )
      ()
    else
      Renderer.render(
        state,
        cursorVisible = false,
        surface,
        size,
        CellFont,
        CellFont,
        CellFont,
        CellMetricsOne,
        CellMetricsOne,
        None,
        damage
      )

  /** The cursor-only counterpart to [[paintFrame]], separated for the same reason. */
  private[tui] def paintCursorOnly(
    state: AppState,
    surface: TerminalRenderSurface,
    size: ViewportSize,
    cursorVisible: Boolean,
    cursorColor: Option[java.awt.Color],
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState]
  ): Unit =
    val _ = Renderer.renderCursorOnly(
      state,
      cursorVisible,
      surface,
      size,
      CellFont,
      CellFont,
      CellFont,
      CellMetricsOne,
      CellMetricsOne,
      cursorColor,
      bufferAnimations
    )

  private def renderFullFn(
    surfaceHolder: SurfaceHolder,
    shell: TerminalShell,
    previewWindowAvailability: MarkdownPreviewWindowAvailability
  ): AppRuntime.RenderFn =
    (state, cursorVisible, cursorColor, damage, _) =>
      for
        size <- shell.viewportSize
        surface = surfaceHolder.forSize(size)
        _ <- IO(paintFrame(state, surface, size, cursorVisible, cursorColor, damage))
        _ <- syncMarkdownPreviewWindow(state, previewWindowAvailability)
      yield ()

  private def renderCursorOnlyFn(
    surfaceHolder: SurfaceHolder,
    shell: TerminalShell,
    previewWindowAvailability: MarkdownPreviewWindowAvailability
  ): AppRuntime.RenderFn =
    (state, cursorVisible, cursorColor, _, bufferAnimations) =>
      for
        size <- shell.viewportSize
        surface = surfaceHolder.forSize(size)
        _ <- IO(paintCursorOnly(state, surface, size, cursorVisible, cursorColor, bufferAnimations))
        _ <- syncMarkdownPreviewWindow(state, previewWindowAvailability)
      yield ()

  /** Pushes a freshly rendered image into the spawned preview window (issue #1113) whenever one is open for the active
    * buffer, following the same rate-limited/reuse-while-editing cadence the GUI's in-app panel already uses
    * (`Buffer.markdownPreviewEditGeneration`/`markdownPreviewCommittedGeneration`, see `MarkdownDocumentPreview`) -- it
    * is called from both render phases, exactly like the terminal cell surface itself, so the window updates on the
    * same cadence as everything else rather than polling independently.
    */
  private def syncMarkdownPreviewWindow(
    state: AppState,
    availability: MarkdownPreviewWindowAvailability
  ): IO[Unit] =
    (availability, state.runtime.markdownPreviewWindowBuffer.flatMap(state.persisted.buffers.get)) match
      case (MarkdownPreviewWindowAvailability.Available(window), Some(buffer)) =>
        window.currentSize.flatMap {
          case (widthPx, heightPx) if widthPx > 0 && heightPx > 0 =>
            IO(renderMarkdownPreviewImage(state, buffer, widthPx, heightPx)).flatMap(window.updateImage)
          case _ =>
            // The window hasn't been shown/laid out yet (its panel reports a 0x0 size) -- nothing to draw into.
            IO.unit
        }
      case _ =>
        IO.unit

  private def renderMarkdownPreviewImage(
    state: AppState,
    buffer: Buffer,
    widthPx: Int,
    heightPx: Int
  ): java.awt.image.BufferedImage =
    val title = buffer.document.filePath
      .flatMap(path => Option(path.getFileName).map(_.toString))
      .getOrElse("Untitled")
    val previewWindow = markdownPreviewSourceWindow(buffer, heightPx)
    val baseUri       = buffer.document.filePath.flatMap(path => Option(path.toAbsolutePath.getParent).map(_.toUri))
    MarkdownDocumentPreview.renderImage(
      source = previewWindow.source,
      title = title,
      widthPx = widthPx,
      heightPx = heightPx,
      theme = state.persisted.theme,
      font = MarkdownPreviewWindow.PreviewFont,
      baseUri = baseUri,
      reuseLastRenderWhileEditing = buffer.markdownPreviewEditGeneration != buffer.markdownPreviewCommittedGeneration
    )

  private val MinMarkdownPreviewSourceLines = 40

  /** The Markdown source window to render, following the editor viewport (scroll position) the same way the GUI's
    * in-app split-preview panel does -- approximating visible line count from the window's pixel height and the preview
    * font's line height, since a Swing window (unlike the GUI's cell grid) has no natural row count.
    */
  private[tui] def markdownPreviewSourceWindow(
    buffer: Buffer,
    heightPx: Int
  ): MarkdownDocumentPreview.PreviewWindow =
    val lineCount = buffer.document.content.lineCount
    if lineCount == 0 then MarkdownDocumentPreview.PreviewWindow(0, 0, "")
    else
      val approxLineHeightPx = math.ceil(MarkdownPreviewWindow.PreviewFont.getSize2D * 1.6).toInt.max(1)
      val visibleLines       = math.max(1, heightPx / approxLineHeightPx)
      val maxSourceLines     = math.max(MinMarkdownPreviewSourceLines, visibleLines)
      val maxStart           = (lineCount - maxSourceLines).max(0)
      val fallbackStart      = buffer.viewport.topLine.max(0).min(maxStart)
      val anchorLine = buffer.editing.cursors.headOption
        .map(_.line)
        .filter(line => line >= 0 && line < lineCount)
        .getOrElse(buffer.viewport.topLine.max(0).min(lineCount - 1))
      val firstSourceLine =
        if anchorLine < fallbackStart then anchorLine.min(maxStart)
        else if anchorLine >= fallbackStart + maxSourceLines then (anchorLine - maxSourceLines / 2).max(0).min(maxStart)
        else fallbackStart
      MarkdownDocumentPreview.PreviewWindow(
        firstSourceLine,
        firstPreviewRow = 0,
        buffer.document.content.linesFrom(firstSourceLine, maxSourceLines).mkString("\n")
      )

  private def registerMarkdownPreviewCloseCallbackFn(
    availability: MarkdownPreviewWindowAvailability
  ): (() => Unit) => Unit =
    cb =>
      availability match
        case MarkdownPreviewWindowAvailability.Available(window) => window.setOnUserClose(cb)
        case MarkdownPreviewWindowAvailability.Unavailable       => ()

  /** Builds the spawned preview window only when a display is reachable (issue #1113) -- never touching AWT/Swing
    * otherwise, so an SSH session with no X11/Wayland forwarding can never trip a `HeadlessException`.
    */
  private def markdownPreviewWindowResource(hasDisplay: Boolean): Resource[IO, MarkdownPreviewWindowAvailability] =
    if hasDisplay then MarkdownPreviewWindow.resource.map(MarkdownPreviewWindowAvailability.Available.apply)
    else Resource.pure(MarkdownPreviewWindowAvailability.Unavailable)

  /** Resolve which [[SystemClipboard]] to use per #1111's `ClipboardStrategy.select`: AWT reuse when a display is
    * reachable, OSC 52 through the terminal's own writer otherwise, an external CLI tool next, and an in-process
    * clipboard as the last resort. The in-process clipboard is always built (cheap, a bare `Ref`) since OSC 52 also
    * uses it as its read-side fallback.
    */
  private def buildClipboard(shell: TerminalShell, hasDisplay: Boolean)(using
    Logger[IO]
  ): IO[SystemClipboard[IO]] =
    for
      inProcess    <- InProcessClipboard[IO]
      externalTool <- IO.blocking(ExternalClipboardTool.detect())
      strategy = ClipboardStrategy.select(hasDisplay, hasTerminalWriter = true, externalTool)
    yield strategy match
      case ClipboardStrategy.Awt => SystemClipboard.awt[IO]
      case ClipboardStrategy.Osc52 =>
        Osc52Clipboard[IO](
          write = text => IO.blocking { shell.writer.write(text); shell.writer.flush() },
          fallback = inProcess
        )
      case ClipboardStrategy.ExternalTool(tool) => ExternalToolClipboard(tool)
      case ClipboardStrategy.InProcess          => inProcess
