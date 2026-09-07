package com.serenity.ui.renderer

import com.serenity.state.models.*
import com.serenity.ui.layout.*

final case class RenderContext(
    surface: RenderSurface,
    layout: CalculatedLayout,
    cursorVisible: Boolean = true,
    cursorColorOverride: Option[java.awt.Color] = None,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState] = Map.empty
):

  def fontForRole(role: TypographyRole): java.awt.Font =
    role match
      case TypographyRole.Code            => codeFont
      case TypographyRole.Prose           => textFont
      case TypographyRole.MarkdownSource  => textFont
      case TypographyRole.MarkdownPreview => textFont
      case TypographyRole.Ui              => uiFont
      case TypographyRole.Mixed           => textFont

  def fontForBuffer(buffer: Buffer): java.awt.Font =
    fontForRole(buffer.typographyRole)

/** Renders an [[AppState]] frame onto a [[RenderSurface]]. The bulk of the implementation lives in the sibling
  * `Renderer*` objects in this package (frame planning, pane content, highlights, markdown lens, the gutter, floating
  * panels, cursor glyphs, and the mutable per-frame damage/cache state); this object keeps only the public entry points
  * so every existing `Renderer.render(...)`/`Renderer.renderCursorOnly(...)`/`Renderer.renderWithCursorOverlay(...)`
  * call site keeps working unchanged (see issue #1323).
  */
object Renderer:

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
    RendererEntryPoints.render(
      state,
      cursorVisible,
      swingWin,
      codeFont,
      textFont,
      uiFont,
      uiMetrics,
      cursorColor,
      repaintOnFlush,
      damage,
      bufferAnimations
    )

  def renderCursorOnly(
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
  ): Boolean =
    RendererCursorOverlay.renderCursorOnly(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      cursorColor
    )

  def renderCursorOnly(
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
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState]
  ): Boolean =
    RendererCursorOverlay.renderCursorOnly(
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
      bufferAnimations
    )

  def renderCursorOnly(
    state: AppState,
    cursorVisible: Boolean,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color],
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState] = Map.empty
  ): Boolean =
    RendererCursorOverlay.renderCursorOnly(
      state,
      cursorVisible,
      swingWin,
      codeFont,
      textFont,
      uiFont,
      uiMetrics,
      cursorColor,
      bufferAnimations
    )

  def renderWithCursorOverlay(
    state: AppState,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Boolean =
    RendererCursorOverlay.renderWithCursorOverlay(
      state,
      surface,
      viewportSize,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      cursorColor
    )

  def renderWithCursorOverlay(
    state: AppState,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color],
    damage: Damage
  ): Boolean =
    RendererCursorOverlay.renderWithCursorOverlay(
      state,
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

  /** Render a base frame and its cursor overlay without recalculating the editor layout. */
  def renderWithCursorOverlay(
    state: AppState,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color],
    damage: Damage = Damage.Everything,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState] = Map.empty
  ): Boolean =
    RendererCursorOverlay.renderWithCursorOverlay(
      state,
      swingWin,
      codeFont,
      textFont,
      uiFont,
      uiMetrics,
      cursorColor,
      damage,
      bufferAnimations
    )

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
    RendererEntryPoints.render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      cellMetrics,
      cursorColor
    )

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
    RendererEntryPoints.render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      cellMetrics,
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
    RendererEntryPoints.render(
      state,
      cursorVisible,
      surface,
      viewportSize,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      cursorColor
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
    RendererEntryPoints.render(
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

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize
  ): Unit =
    RendererEntryPoints.render(state, cursorVisible, surface, viewportSize)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    cursorColor: Option[java.awt.Color]
  ): Unit =
    RendererEntryPoints.render(state, cursorVisible, surface, viewportSize, cursorColor)

  def render(
    state: AppState,
    cursorVisible: Boolean,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    cursorColor: Option[java.awt.Color],
    damage: Damage
  ): Unit =
    RendererEntryPoints.render(state, cursorVisible, surface, viewportSize, cursorColor, damage)

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
    RendererEntryPoints.renderWithRepaintRegion(
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
    RendererEntryPoints.cursorRepaintRects(
      state,
      surface,
      viewportSize,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      cursorColor
    )

  private[serenity] def withSceneIfNeeded[A](
    state: AppState,
    buildScene: => UiSceneSnapshot
  )(startup: StartupPage => A)(editor: UiSceneSnapshot => A): A =
    RendererEntryPoints.withSceneIfNeeded(state, buildScene)(startup)(editor)

  private[serenity] def visibleAnnotationLines[A](
    visibleLines: Set[Int],
    indexed: Map[Int, List[A]]
  ): Map[Int, List[A]] =
    RendererPaneSetup.visibleAnnotationLines(visibleLines, indexed)

  private[serenity] def commentHighlightBackground(theme: com.serenity.ui.theme.Theme): java.awt.Color =
    RendererHighlights.commentHighlightBackground(theme)

  private[serenity] def diagnosticHighlightBackground(
    theme: com.serenity.ui.theme.Theme,
    severityCode: Option[Int]
  ): java.awt.Color =
    RendererHighlights.diagnosticHighlightBackground(theme, severityCode)
