package com.serenity.ui.renderer

import java.util.concurrent.atomic.AtomicReference

import com.serenity.animation.ThemeInterpolator
import com.serenity.state.manager.AuthoritativeUiScene
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** The package's entry point for painting a whole frame: a full frame either on a `SwingWindow` or a surface-generic
  * [[RenderSurface]], plus the startup-page and theme-blend plumbing every frame goes through first. Cursor-only and
  * cursor-overlay frames are a separate entry point, [[RendererCursorOverlay]]; it composes the same plumbing exposed
  * here ([[withEffectiveTheme]], [[withSceneIfNeeded]], [[renderStartPageFrame]]), which is why those are public rather
  * than private to this object -- both entry points must resolve a theme transition, a start page and a scene the same
  * way or the two paths disagree about what frame they are painting.
  */
object RendererEntryPoints:

  def withEffectiveTheme(state: AppState): AppState =
    state.runtime.themeTransition match
      case None => state
      case Some(t) =>
        state.copy(persisted =
          state.persisted.copy(theme = ThemeInterpolator.blend(t.previousTheme, state.persisted.theme, t.progress))
        )

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
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    output: Option[FrameOutput]
  ): Unit =
    surface.hideCursor()
    surface.clearViewport(state.persisted.theme.background)
    RendererFramePlanner.forgetPreservedContent(surface, output)
    RendererStartPage.renderStartPage(
      page,
      surface,
      viewportSize,
      state.persisted.theme,
      uiFont,
      cellMetrics,
      uiMetrics
    )
    surface.effects.foreach(_.applyPostProcessing(state.persisted.config.surfaceConfig.postProcessingEffect))
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
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color],
    repaintOnFlush: Boolean,
    damage: Damage = Damage.Everything,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState] = Map.empty
  ): Unit =
    val state0 = withEffectiveTheme(state)
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
      codeFont,
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
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont)
    )(page => renderStartPageFrame(state0, page, surface, viewportSize, uiFont, swingWin.metrics, uiMetrics, output)) {
      scene =>
        RendererFramePlanner.renderFrame(
          state0,
          cursorVisible,
          surface,
          viewportSize,
          scene,
          codeFont,
          textFont,
          uiFont,
          swingWin.metrics,
          uiMetrics,
          cursorColor,
          output,
          damage,
          bufferAnimations
        )
    }

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    cellMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Unit =
    render(state, cursorVisible, surface, viewportSize, codeFont, textFont, cellMetrics, cursorColor, Damage.Everything)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    cellMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color],
    damage: Damage
  ): Unit =
    val defaultUiFont = java.awt
      .Font(java.awt.Font.SANS_SERIF, java.awt.Font.PLAIN, codeFont.getSize)
      .deriveFont(codeFont.getSize2D)
    render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      defaultUiFont,
      cellMetrics,
      CellMetrics.fromFont(defaultUiFont),
      cursorColor,
      damage
    )

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
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
      Damage.Everything
    )

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color],
    damage: Damage
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
      damage
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
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color],
    damage: Damage = Damage.Everything
  ): Option[PixelRect] =
    val state0        = withEffectiveTheme(state)
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(ScreenIdentity(surface), repaintRegion))
    // #1105/#1215: see the surface-generic renderCursorOnly in RendererCursorOverlay for why this is scoped to a
    // surface with no real FontRenderContext.
    val cellMetricsOverride = Option.when(surface.text.fontRenderContext.isEmpty)(cellMetrics)
    val _ = withSceneIfNeeded(
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont, cellMetrics = cellMetricsOverride)
    )(page => renderStartPageFrame(state0, page, surface, viewportSize, uiFont, cellMetrics, uiMetrics, output)) {
      scene =>
        RendererFramePlanner.renderFrame(
          state0,
          cursorVisible,
          surface,
          viewportSize,
          scene,
          codeFont,
          textFont,
          uiFont,
          cellMetrics,
          uiMetrics,
          cursorColor,
          output,
          damage
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
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): List[PixelRect] =
    val state0 = withEffectiveTheme(state)
    // #1105/#1215: see the surface-generic renderCursorOnly in RendererCursorOverlay for why this is scoped to a
    // surface with no real FontRenderContext.
    val cellMetricsOverride = Option.when(surface.text.fontRenderContext.isEmpty)(cellMetrics)
    withSceneIfNeeded(
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont, cellMetrics = cellMetricsOverride)
    )(_ => Nil) { scene =>
      val prepared = RendererFramePlanner.prepareScene(
        state0,
        surface,
        viewportSize,
        scene,
        cursorVisible = true,
        cursorColor,
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics
      )
      val context = RenderContext(
        surface,
        prepared.scene.calculatedLayout,
        true,
        cursorColor,
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics
      )
      RendererPaneContent.renderEditorCursors(state0, context, prepared.renderPlan)
    }

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize
  ): Unit =
    render(state, cursorVisible, surface, viewportSize, None, Damage.Everything)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    cursorColor: Option[java.awt.Color]
  ): Unit =
    render(state, cursorVisible, surface, viewportSize, cursorColor, Damage.Everything)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    cursorColor: Option[java.awt.Color],
    damage: Damage
  ): Unit =
    val defaultFont = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
    render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      defaultFont,
      defaultFont,
      CellMetrics.fromFont(defaultFont),
      cursorColor,
      damage
    )
