package com.serenity.ui.renderer

import java.util.concurrent.atomic.AtomicReference

import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.*

/** Cursor-only redraws (blink ticks that don't need a full layout) and cursor-overlay frames (a base frame painted
  * without the caret, then the caret drawn on top so blinking never has to redraw the base). Both flavours reuse the
  * cached [[PreparedScene]] from the last full layout via [[resolveCursorRenderPlan]] when it is still valid.
  */
object RendererCursorOverlay:

  /** The render plan a cursor-only redraw needs: the cached one from the last full layout, if it is still valid for
    * these fonts/metrics/viewport, or a freshly prepared one otherwise. Shared by every cursor-only entry point (Swing
    * and surface-generic alike) so the cache is consulted exactly the same way regardless of shell.
    */
  private def resolveCursorRenderPlan(
    state0: AppState,
    authoritativeScene: UiSceneSnapshot,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    cursorVisible: Boolean,
    cursorColor: Option[RenderColor],
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    caches: RenderCaches
  ): (CalculatedLayout, EditorPaneRenderPlan) =
    caches.frameState
      .preparedSceneFor(surface)
      .filter(_.matches(authoritativeScene, codeFont, textFont, uiFont, cellMetrics, uiMetrics, viewportSize))
      .map(value => value.scene.calculatedLayout -> value.renderPlan)
      .getOrElse {
        val next = RendererFramePlanner.prepareScene(
          state0,
          surface,
          viewportSize,
          authoritativeScene,
          cursorVisible,
          cursorColor,
          codeFont,
          textFont,
          uiFont,
          cellMetrics,
          uiMetrics,
          caches
        )
        caches.frameState.rememberPreparedScene(surface, next)
        next.scene.calculatedLayout -> next.renderPlan
      }

  /** Draw just the cursor glyphs for `renderPlan` into `surface` and flush. Shared tail of every cursor-only /
    * cursor-overlay entry point: the only thing that differs between them is which surface the cursor lands on and how
    * that surface's owning shell was told a fresh layout was needed.
    */
  private def paintCursorsOnly(
    state0: AppState,
    surface: RenderSurface,
    layout: CalculatedLayout,
    renderPlan: EditorPaneRenderPlan,
    cursorVisible: Boolean,
    cursorColor: Option[RenderColor],
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    caches: RenderCaches
  ): List[PixelRect] =
    val context = RenderContext(
      surface,
      layout,
      cursorVisible,
      cursorColor,
      codeFont,
      textFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      caches
    )
    val cursorRects = RendererPaneContent.renderEditorCursors(state0, context, renderPlan)
    presentHardwareCursor(surface, cursorVisible, cursorRects, cellMetrics)
    surface.flush()
    cursorRects

  /** #1170: on a surface that can delegate the caret to a real hardware/terminal cursor, position and style it from the
    * primary caret's rect instead of leaving it to be painted as surface content. A surface with no hardware cursor to
    * delegate to is entirely unaffected: this is a no-op there.
    */
  private def presentHardwareCursor(
    surface: RenderSurface,
    cursorVisible: Boolean,
    cursorRects: List[PixelRect],
    cellMetrics: CellMetrics
  ): Unit =
    surface.hardwareCursor.foreach { hardwareCursor =>
      cursorRects.headOption match
        case Some(rect) if cursorVisible =>
          hardwareCursor.present(
            cellMetrics.toCol(rect.xPx),
            cellMetrics.toRow(rect.yPx),
            HardwareCursorStyle(HardwareCursorShape.Block, blinking = true)
          )
        case _ =>
          hardwareCursor.hide()
    }

  /** Surface-generic form of [[renderCursorOnly]]: redraws just the cursor glyphs into `surface`, reusing the cached
    * render plan from the last full layout when it is still valid. `false` only for a start-page-only state, which has
    * no cursor to draw.
    */
  def renderCursorOnly(
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
  ): Boolean =
    // #1105/#1215: a surface reporting no FontRenderContext (a terminal) has no real font rendering to measure
    // against at all -- its own declared cellMetrics must reach this scene's cell-based layout, and must force it,
    // rather than being silently discarded by the font's own measured-vs-cell auto-detection. A surface that does
    // report one (every GUI canvas, and any other surface-generic caller with real font rendering) keeps this scene's
    // long-standing auto-detected behaviour untouched.
    val cellMetricsOverride = Option.when(surface.text.fontRenderContext.isEmpty)(cellMetrics)
    RendererEntryPoints.withSceneIfNeeded(
      state,
      caches.authoritativeScene
        .forState(state, viewportSize, codeFont.toAwt, textFont.toAwt, cellMetrics = cellMetricsOverride)
    )(_ => false) { authoritativeScene =>
      val (layout, renderPlan) = resolveCursorRenderPlan(
        state,
        authoritativeScene,
        surface,
        viewportSize,
        cursorVisible,
        cursorColor,
        codeFont.toAwt,
        textFont.toAwt,
        uiFont.toAwt,
        cellMetrics,
        uiMetrics,
        caches
      )
      val _ = paintCursorsOnly(
        state,
        surface,
        layout,
        renderPlan,
        cursorVisible,
        cursorColor,
        codeFont.toAwt,
        textFont.toAwt,
        uiFont.toAwt,
        cellMetrics,
        uiMetrics,
        caches
      )
      true
    }

  def renderCursorOnly(
    state: AppState,
    cursorVisible: Boolean,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    codeFont: FontSpec,
    textFont: FontSpec,
    uiFont: FontSpec,
    uiMetrics: CellMetrics,
    cursorColor: Option[RenderColor],
    caches: RenderCaches = RenderCaches.create()
  ): Boolean =
    val viewportSize = swingWin.viewportSize
    // Swing/Java2D always has a real FontRenderContext -- no cellMetrics override needed, see the render() entry
    // point in RendererEntryPoints.
    RendererEntryPoints.withSceneIfNeeded(
      state,
      caches.authoritativeScene.forState(state, viewportSize, codeFont.toAwt, textFont.toAwt)
    )(_ => false) { authoritativeScene =>
      // No base content is redrawn by this call, so the base frame contributes nothing to the repaint bound --
      // only the cursor's own old and new pixel rects can have changed on screen.
      swingWin.onCursorOverlayReady(Some(new java.awt.Rectangle(0, 0, 0, 0))) {
        val surface = CaretRecordingSurface.forCanvas(swingWin.metrics, codeFont.toAwt, swingWin.canvas)
        val (layout, renderPlan) = resolveCursorRenderPlan(
          state,
          authoritativeScene,
          surface,
          viewportSize,
          cursorVisible,
          cursorColor,
          codeFont.toAwt,
          textFont.toAwt,
          uiFont.toAwt,
          swingWin.metrics,
          uiMetrics,
          caches
        )
        val _ = paintCursorsOnly(
          state,
          surface,
          layout,
          renderPlan,
          cursorVisible,
          cursorColor,
          codeFont.toAwt,
          textFont.toAwt,
          uiFont.toAwt,
          swingWin.metrics,
          uiMetrics,
          caches
        )
        caretPaints(surface)
      }
    }

  /** Surface-generic form of [[renderWithCursorOverlay]]: renders a base frame with the cursor left out, then draws the
    * cursor directly on top of the same surface and flushes again. Unlike the Swing form, whose window fills the carets
    * over the presented frame so blinking never has to redraw the base, this issues two flushes -- fine for any
    * `RenderSurface`, and inexpensive on a damage-diffed one, where the second flush only ever touches the handful of
    * cells the cursor occupies.
    */
  def renderWithCursorOverlay(
    state: AppState,
    surface: RenderSurface,
    viewportSize: ViewportSize,
    codeFont: FontSpec,
    textFont: FontSpec,
    uiFont: FontSpec,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[RenderColor],
    caches: RenderCaches
  ): Boolean =
    renderWithCursorOverlay(
      state,
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

  def renderWithCursorOverlay(
    state: AppState,
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
  ): Boolean =
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(ScreenIdentity(surface), repaintRegion))
    // #1105/#1215: see the surface-generic renderCursorOnly above for why this is scoped to a surface with no real
    // FontRenderContext.
    val cellMetricsOverride = Option.when(surface.text.fontRenderContext.isEmpty)(cellMetrics)
    RendererEntryPoints.withSceneIfNeeded(
      state,
      caches.authoritativeScene
        .forState(state, viewportSize, codeFont.toAwt, textFont.toAwt, cellMetrics = cellMetricsOverride)
    ) { page =>
      RendererEntryPoints.renderStartPageFrame(
        state,
        page,
        surface,
        viewportSize,
        uiFont,
        cellMetrics,
        uiMetrics,
        output,
        caches
      )
      true
    } { scene =>
      RendererFramePlanner
        .renderFrame(
          state,
          cursorVisible = false,
          surface,
          viewportSize,
          scene,
          codeFont.toAwt,
          textFont.toAwt,
          uiFont.toAwt,
          cellMetrics,
          uiMetrics,
          cursorColor = None,
          output,
          damage,
          caches = caches
        )
        .fold(false) { renderPlan =>
          val _ = paintCursorsOnly(
            state,
            surface,
            scene.calculatedLayout,
            renderPlan,
            true,
            cursorColor,
            codeFont.toAwt,
            textFont.toAwt,
            uiFont.toAwt,
            cellMetrics,
            uiMetrics,
            caches = caches
          )
          true
        }
    }

  /** Render a base frame and its cursor overlay without recalculating the editor layout. */
  def renderWithCursorOverlay(
    state: AppState,
    swingWin: com.serenity.ui.terminal.SwingWindow,
    codeFont: FontSpec,
    textFont: FontSpec,
    uiFont: FontSpec,
    uiMetrics: CellMetrics,
    cursorColor: Option[RenderColor],
    damage: Damage = Damage.Everything,
    caches: RenderCaches = RenderCaches.create()
  ): Boolean =
    val viewportSize = swingWin.viewportSize
    // The window fills the carets over the presented frame, so this base frame is repainted whole every time; the
    // record is still kept up to date so the next frame knows what the screen is showing.
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(ScreenIdentity(swingWin.canvas), repaintRegion))
    // The pooled acquirer is what lets this frame reuse the pixels of the last frame drawn into the same image; the
    // carets are filled over it separately, so the base frame here is pure pane content and chrome.
    val surface = Java2DRenderSurface.forFrame(
      swingWin.metrics,
      codeFont.toAwt,
      swingWin.canvas,
      swingWin.onBaseImageReady,
      swingWin.acquireBaseImage
    )
    // Swing/Java2D always has a real FontRenderContext -- no cellMetrics override needed, see the render() entry
    // point in RendererEntryPoints.
    RendererEntryPoints.withSceneIfNeeded(
      state,
      caches.authoritativeScene.forState(state, viewportSize, codeFont.toAwt, textFont.toAwt)
    ) { page =>
      RendererEntryPoints.renderStartPageFrame(
        state,
        page,
        surface,
        viewportSize,
        uiFont,
        swingWin.metrics,
        uiMetrics,
        output,
        caches
      )
      // The base frame published above went through onBaseImageReady, which does not repaint the canvas by
      // itself -- only onCursorOverlayReady does. The editor branch below reaches it naturally via its cursor
      // composite step; the startup page has no cursor to draw, but still needs this call to actually get painted.
      // A start page also has no persisted content to reason about, so this always forces a full repaint.
      swingWin.onCursorOverlayReady(None)(Nil)
    } { scene =>
      RendererFramePlanner
        .renderFrame(
          state,
          cursorVisible = false,
          surface,
          viewportSize,
          scene,
          codeFont.toAwt,
          textFont.toAwt,
          uiFont.toAwt,
          swingWin.metrics,
          uiMetrics,
          cursorColor = None,
          output,
          damage,
          caches
        )
        .fold(false) { renderPlan =>
          val baseDirtyRegion = repaintRegion.get().map(RendererFrameState.toAwtRectangle)
          swingWin.onCursorOverlayReady(baseDirtyRegion) {
            val cursorSurface = CaretRecordingSurface.forCanvas(swingWin.metrics, codeFont.toAwt, swingWin.canvas)
            val _ = paintCursorsOnly(
              state,
              cursorSurface,
              scene.calculatedLayout,
              renderPlan,
              true,
              cursorColor,
              codeFont.toAwt,
              textFont.toAwt,
              uiFont.toAwt,
              swingWin.metrics,
              uiMetrics,
              caches
            )
            caretPaints(cursorSurface)
          }
        }
    }

  private def caretPaints(surface: CaretRecordingSurface): List[com.serenity.ui.terminal.SwingWindow.CaretPaint] =
    surface.recordedFills.map(fill =>
      com.serenity.ui.terminal.SwingWindow.CaretPaint(RendererFrameState.toAwtRectangle(fill.rect), fill.color)
    )
