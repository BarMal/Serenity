package com.serenity.ui.renderer

import java.util.concurrent.atomic.AtomicReference

import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.*

/** The package's entry point for painting a whole frame: a full frame either on a `SwingWindow` or a surface-generic
  * [[RenderSurface]], plus the startup-page plumbing every frame goes through first. Cursor-only and cursor-overlay
  * frames are a separate entry point, [[RendererCursorOverlay]]; it composes the same plumbing exposed here
  * ([[withSceneIfNeeded]], [[renderStartPageFrame]]), which is why those are public rather than private to this object
  * -- both entry points must resolve a start page and a scene the same way or the two paths disagree about what frame
  * they are painting.
  */
object RendererEntryPoints:

  private def startPageOnly(state: AppState): Option[StartupPage] =
    // A blocking dialog (#814) painted from the startup page (#1289) needs the full frame path, since that's the one
    // that paints the modal layer -- the fast path below skips straight to the start page frame and nothing else.
    if state.runtime.modalStack.nonEmpty then None
    else
      state.runtime.uiSurfaces match
        case List(UiSurface(_, SurfaceContent.StartPage(page), _, _)) => Some(page)
        case _                                                        => None

  def renderStartPageFrame(
    state: AppState,
    page: StartupPage,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    uiFont: FontSpec,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    output: Option[FrameOutput],
    caches: RenderCaches = RenderCaches.create()
  ): Unit =
    surface.hideCursor()
    surface.clearViewport(state.persisted.theme.background)
    RendererFramePlanner.forgetPreservedContent(surface, output, caches)
    RendererStartPage.renderStartPage(
      page,
      surface,
      viewportSize,
      state.persisted.theme,
      uiFont.toAwt,
      cellMetrics,
      uiMetrics
    )
    surface.flush()

  private[serenity] def withSceneIfNeeded[A](
    state: AppState,
    buildScene: => UiSceneSnapshot
  )(startup: StartupPage => A)(editor: UiSceneSnapshot => A): A =
    startPageOnly(state).fold(editor(buildScene))(startup)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    codeFont: FontSpec,
    textFont: FontSpec,
    uiFont: FontSpec,
    uiMetrics: CellMetrics,
    cursorColor: Option[RenderColor],
    repaintOnFlush: Boolean,
    damage: Damage = Damage.Everything,
    caches: RenderCaches = RenderCaches.create()
  ): Unit =
    // Set while the frame is drawn, read when it is flushed: None asks for a whole-canvas repaint, Some(rect) for a
    // repaint bounded to the pane rows this frame actually changed.
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(ScreenIdentity(swingWin.canvas), repaintRegion))
    val publishFrame: java.awt.image.BufferedImage => Unit =
      if repaintOnFlush then
        image => swingWin.onImageReady(image, repaintRegion.get().map(RendererFrameState.toAwtRectangle))
      else swingWin.onBaseImageReady
    val surface = Java2DRenderSurface.forFrame(
      swingWin.metrics,
      codeFont.toAwt,
      swingWin.canvas,
      publishFrame,
      swingWin.acquireBaseImage
    )
    val viewportSize = swingWin.viewportSize
    // A Swing/Java2D surface always has a real FontRenderContext to measure against, so this scene keeps its
    // long-standing behaviour of deriving cell metrics from `codeFont` and letting `TextLayoutSnapshot` auto-detect
    // measured vs. cell layout per buffer font -- no override needed (#1215's cellMetrics override is for a surface
    // with no real font rendering at all, i.e. TUI's `TerminalRenderSurface`, reached only through the
    // surface-generic entry points below).
    val _ = withSceneIfNeeded(
      state,
      caches.authoritativeScene.forState(state, viewportSize, codeFont.toAwt, textFont.toAwt)
    )(page =>
      renderStartPageFrame(state, page, surface, viewportSize, uiFont, swingWin.metrics, uiMetrics, output, caches)
    ) { scene =>
      RendererFramePlanner.renderFrame(
        state,
        cursorVisible,
        surface,
        viewportSize,
        scene,
        codeFont.toAwt,
        textFont.toAwt,
        uiFont.toAwt,
        swingWin.metrics,
        uiMetrics,
        cursorColor,
        output,
        damage,
        caches
      )
    }

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: FontSpec,
    textFont: FontSpec,
    cellMetrics: CellMetrics,
    cursorColor: Option[RenderColor],
    caches: RenderCaches
  ): Unit =
    render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      cellMetrics,
      cursorColor,
      Damage.Everything,
      caches
    )

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: FontSpec,
    textFont: FontSpec,
    cellMetrics: CellMetrics,
    cursorColor: Option[RenderColor],
    damage: Damage,
    caches: RenderCaches
  ): Unit =
    val defaultUiFont = java.awt
      .Font(java.awt.Font.SANS_SERIF, java.awt.Font.PLAIN, codeFont.toAwt.getSize)
      .deriveFont(codeFont.sizePt)
    render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      FontSpec.fromAwt(defaultUiFont),
      cellMetrics,
      CellMetrics.fromFont(defaultUiFont),
      cursorColor,
      damage,
      caches
    )

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: FontSpec,
    textFont: FontSpec,
    uiFont: FontSpec,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[RenderColor],
    caches: RenderCaches
  ): Unit =
    render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      cursorColor,
      Damage.Everything,
      caches
    )

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: FontSpec,
    textFont: FontSpec,
    uiFont: FontSpec,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[RenderColor],
    damage: Damage,
    caches: RenderCaches
  ): Unit =
    val _ = renderWithRepaintRegion(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      cursorColor,
      damage,
      caches
    )

  /** Render one frame and report which part of the canvas it changed.
    *
    * `None` means the whole canvas has to be repainted. `Some(rect)` means everything outside `rect` is already correct
    * on screen; an empty rect means the frame is pixel-identical to the one on screen.
    */
  private[serenity] def renderWithRepaintRegion(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: FontSpec,
    textFont: FontSpec,
    uiFont: FontSpec,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[RenderColor],
    damage: Damage = Damage.Everything,
    caches: RenderCaches = RenderCaches.create()
  ): Option[PixelRect] =
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(ScreenIdentity(surface), repaintRegion))
    // #1105/#1215: see the surface-generic renderCursorOnly in RendererCursorOverlay for why this is scoped to a
    // surface with no real FontRenderContext.
    val cellMetricsOverride = Option.when(surface.text.fontRenderContext.isEmpty)(cellMetrics)
    val _ = withSceneIfNeeded(
      state,
      caches.authoritativeScene
        .forState(state, viewportSize, codeFont.toAwt, textFont.toAwt, cellMetrics = cellMetricsOverride)
    )(page => renderStartPageFrame(state, page, surface, viewportSize, uiFont, cellMetrics, uiMetrics, output, caches)) {
      scene =>
        RendererFramePlanner.renderFrame(
          state,
          cursorVisible,
          surface,
          viewportSize,
          scene,
          codeFont.toAwt,
          textFont.toAwt,
          uiFont.toAwt,
          cellMetrics,
          uiMetrics,
          cursorColor,
          output,
          damage,
          caches = caches
        )
    }
    repaintRegion.get()

  /** Report the pixel rects the visible cursors in `state` would be painted at, without needing a real `SwingWindow`.
    * Exposed for testing #963's bounded-repaint region: `onCursorOverlayReady` unions this same geometry (from both the
    * previous and current frame) with the base frame's own dirty region.
    */
  private[serenity] def cursorRepaintRects(
    state: AppState,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: FontSpec,
    textFont: FontSpec,
    uiFont: FontSpec,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[RenderColor],
    caches: RenderCaches = RenderCaches.create()
  ): List[PixelRect] =
    // #1105/#1215: see the surface-generic renderCursorOnly in RendererCursorOverlay for why this is scoped to a
    // surface with no real FontRenderContext.
    val cellMetricsOverride = Option.when(surface.text.fontRenderContext.isEmpty)(cellMetrics)
    withSceneIfNeeded(
      state,
      caches.authoritativeScene
        .forState(state, viewportSize, codeFont.toAwt, textFont.toAwt, cellMetrics = cellMetricsOverride)
    )(_ => Nil) { scene =>
      val prepared = RendererFramePlanner.prepareScene(
        state,
        surface,
        viewportSize,
        scene,
        cursorVisible = true,
        cursorColor,
        codeFont.toAwt,
        textFont.toAwt,
        uiFont.toAwt,
        cellMetrics,
        uiMetrics,
        caches
      )
      val context = RenderContext(
        surface,
        prepared.scene.calculatedLayout,
        true,
        cursorColor,
        codeFont.toAwt,
        textFont.toAwt,
        uiFont.toAwt,
        cellMetrics,
        uiMetrics,
        caches = caches
      )
      RendererPaneContent.renderEditorCursors(state, context, prepared.renderPlan)
    }

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    caches: RenderCaches
  ): Unit =
    render(state, cursorVisible, surface, viewportSize, None, Damage.Everything, caches)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    cursorColor: Option[RenderColor],
    caches: RenderCaches
  ): Unit =
    render(state, cursorVisible, surface, viewportSize, cursorColor, Damage.Everything, caches)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    cursorColor: Option[RenderColor],
    damage: Damage,
    caches: RenderCaches
  ): Unit =
    val defaultFont = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
    render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      FontSpec.fromAwt(defaultFont),
      FontSpec.fromAwt(defaultFont),
      CellMetrics.fromFont(defaultFont),
      cursorColor,
      damage,
      caches
    )
