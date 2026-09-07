package com.serenity.ui.renderer

import java.util.concurrent.atomic.AtomicReference

import com.serenity.config.CursorMode
import com.serenity.state.manager.AuthoritativeUiScene
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Cursor-only redraws (blink ticks that don't need a full layout) and cursor-overlay frames (a base frame painted
  * without the caret, then the caret composited on top so blinking never has to redraw the base). Both flavours reuse
  * the cached [[PreparedScene]] from the last full layout via [[resolveCursorRenderPlan]] when it is still valid.
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
    cursorColor: Option[java.awt.Color],
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics
  ): (CalculatedLayout, EditorPaneRenderPlan) =
    RendererFrameState
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
          uiMetrics
        )
        RendererFrameState.rememberPreparedScene(surface, next)
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
    cursorColor: Option[java.awt.Color],
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    bufferAnimations: Map[BufferId, com.serenity.animation.AnimationState] = Map.empty
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
      bufferAnimations
    )
    val cursorRects = RendererPaneContent.renderEditorCursors(state0, context, renderPlan)
    presentHardwareCursor(surface, state0, cursorVisible, cursorRects, cellMetrics, cursorColor)
    surface.flush()
    cursorRects

  /** #1170: on a surface that can delegate the caret to a real hardware/terminal cursor, position and style it from the
    * primary caret's rect instead of leaving it to be painted as surface content -- except in breathe mode, which
    * animates color/opacity over time (something a DECSCUSR style can't represent) and so stays the documented,
    * app-painted exception on any surface that can actually paint it. A cell-addressed terminal has no such content
    * path (`fillPixelRect` is necessarily a no-op there, see #1012), which left breathe mode's caret invisible on TUI
    * outright; the `(CursorMode.Breathe, ...)` case below approximates it instead by thresholding the same alpha
    * `computeIdleCursorFrame` already modulates into a slow present/hide blink, so the exception still holds for every
    * GUI canvas (unaffected: `hardwareCursor` is `None` there) while TUI gets a visible cursor rather than none at all.
    * A surface with no hardware cursor to delegate to is entirely unaffected either way: this is a no-op there.
    */
  private def presentHardwareCursor(
    surface: RenderSurface,
    state0: AppState,
    cursorVisible: Boolean,
    cursorRects: List[PixelRect],
    cellMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Unit =
    surface.hardwareCursor.foreach { hardwareCursor =>
      (state0.persisted.config.cursorMode, cursorRects.headOption) match
        case (CursorMode.Blink, Some(rect)) if cursorVisible =>
          hardwareCursor.present(
            cellMetrics.toCol(rect.xPx),
            cellMetrics.toRow(rect.yPx),
            HardwareCursorStyle(HardwareCursorShape.Block, blinking = true)
          )
        // `forall`, not `exists`: only the idle cursor phase supplies a colour, so a frame without one is an ordinary
        // content frame rather than the faded half of a breathe cycle. Treating a missing colour as faded hid the
        // caret on every content frame and left the terminal's own cursor wherever the content diff last wrote --
        // the bottom of the screen (#1215).
        case (CursorMode.Breathe, Some(rect)) if cursorVisible && cursorColor.forall(_.getAlpha >= 128) =>
          hardwareCursor.present(
            cellMetrics.toCol(rect.xPx),
            cellMetrics.toRow(rect.yPx),
            HardwareCursorStyle(HardwareCursorShape.Block, blinking = false)
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
    codeFont: java.awt.Font,
    textFont: java.awt.Font,
    uiFont: java.awt.Font,
    cellMetrics: CellMetrics,
    uiMetrics: CellMetrics,
    cursorColor: Option[java.awt.Color]
  ): Boolean =
    renderCursorOnly(
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
      Map.empty
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
    val state0 = RendererEntryPoints.withEffectiveTheme(state)
    // #1105/#1215: a surface reporting no FontRenderContext (a terminal) has no real font rendering to measure
    // against at all -- its own declared cellMetrics must reach this scene's cell-based layout, and must force it,
    // rather than being silently discarded by the font's own measured-vs-cell auto-detection. A surface that does
    // report one (every GUI canvas, and any other surface-generic caller with real font rendering) keeps this scene's
    // long-standing auto-detected behaviour untouched.
    val cellMetricsOverride = Option.when(surface.text.fontRenderContext.isEmpty)(cellMetrics)
    RendererEntryPoints.withSceneIfNeeded(
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont, cellMetrics = cellMetricsOverride)
    )(_ => false) { authoritativeScene =>
      val (layout, renderPlan) = resolveCursorRenderPlan(
        state0,
        authoritativeScene,
        surface,
        viewportSize,
        cursorVisible,
        cursorColor,
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics
      )
      val _ = paintCursorsOnly(
        state0,
        surface,
        layout,
        renderPlan,
        cursorVisible,
        cursorColor,
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        bufferAnimations
      )
      true
    }

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
    val state0       = RendererEntryPoints.withEffectiveTheme(state)
    val viewportSize = swingWin.viewportSize
    // Swing/Java2D always has a real FontRenderContext -- no cellMetrics override needed, see the render() entry
    // point in RendererEntryPoints.
    RendererEntryPoints.withSceneIfNeeded(
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont)
    )(_ => false) { authoritativeScene =>
      // No base content is redrawn by this call, so the base frame contributes nothing to the repaint bound --
      // only the cursor's own old and new pixel rects can have changed on screen.
      swingWin.onCursorOverlayReady(Some(new java.awt.Rectangle(0, 0, 0, 0))) { image =>
        val surface =
          Java2DRenderSurface.forImage(image, swingWin.metrics, codeFont, swingWin.canvas, _ => ())
        val (layout, renderPlan) = resolveCursorRenderPlan(
          state0,
          authoritativeScene,
          surface,
          viewportSize,
          cursorVisible,
          cursorColor,
          codeFont,
          textFont,
          uiFont,
          swingWin.metrics,
          uiMetrics
        )
        paintCursorsOnly(
          state0,
          surface,
          layout,
          renderPlan,
          cursorVisible,
          cursorColor,
          codeFont,
          textFont,
          uiFont,
          swingWin.metrics,
          uiMetrics,
          bufferAnimations
        ).map(RendererFrameState.toAwtRectangle)
      }
    }

  /** Surface-generic form of [[renderWithCursorOverlay]]: renders a base frame with the cursor left out, then draws the
    * cursor directly on top of the same surface and flushes again. Unlike the Swing form, which composites the cursor
    * into a separate overlay image so blinking never has to redraw the base, this issues two flushes -- fine for any
    * `RenderSurface`, and inexpensive on a damage-diffed one, where the second flush only ever touches the handful of
    * cells the cursor occupies.
    */
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
      Damage.Everything
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
    val state0        = RendererEntryPoints.withEffectiveTheme(state)
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(ScreenIdentity(surface), repaintRegion))
    // #1105/#1215: see the surface-generic renderCursorOnly above for why this is scoped to a surface with no real
    // FontRenderContext.
    val cellMetricsOverride = Option.when(surface.text.fontRenderContext.isEmpty)(cellMetrics)
    RendererEntryPoints.withSceneIfNeeded(
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont, cellMetrics = cellMetricsOverride)
    ) { page =>
      RendererEntryPoints.renderStartPageFrame(
        state0,
        page,
        surface,
        viewportSize,
        uiFont,
        cellMetrics,
        uiMetrics,
        output
      )
      true
    } { scene =>
      RendererFramePlanner
        .renderFrame(
          state0,
          cursorVisible = false,
          surface,
          viewportSize,
          scene,
          codeFont,
          textFont,
          uiFont,
          cellMetrics,
          uiMetrics,
          cursorColor = None,
          output,
          damage
        )
        .fold(false) { renderPlan =>
          val _ = paintCursorsOnly(
            state0,
            surface,
            scene.calculatedLayout,
            renderPlan,
            true,
            cursorColor,
            codeFont,
            textFont,
            uiFont,
            cellMetrics,
            uiMetrics
          )
          true
        }
    }

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
    val state0       = RendererEntryPoints.withEffectiveTheme(state)
    val viewportSize = swingWin.viewportSize
    // The caret is composited from a separate overlay image, so this base frame is repainted whole every time; the
    // record is still kept up to date so the next frame knows what the screen is showing.
    val repaintRegion = new AtomicReference[Option[PixelRect]](None)
    val output        = Some(FrameOutput(ScreenIdentity(swingWin.canvas), repaintRegion))
    // The pooled acquirer is what lets this frame reuse the pixels of the last frame drawn into the same image; the
    // cursor rides on a separate overlay image, so the base frame here is pure pane content and chrome.
    val surface = Java2DRenderSurface.forFrame(
      swingWin.metrics,
      codeFont,
      swingWin.canvas,
      swingWin.onBaseImageReady,
      swingWin.acquireBaseImage
    )
    // Swing/Java2D always has a real FontRenderContext -- no cellMetrics override needed, see the render() entry
    // point in RendererEntryPoints.
    RendererEntryPoints.withSceneIfNeeded(
      state0,
      AuthoritativeUiScene.forState(state0, viewportSize, codeFont, textFont)
    ) { page =>
      RendererEntryPoints.renderStartPageFrame(
        state0,
        page,
        surface,
        viewportSize,
        uiFont,
        swingWin.metrics,
        uiMetrics,
        output
      )
      // The base frame published above went through onBaseImageReady, which does not repaint the canvas by
      // itself -- only onCursorOverlayReady does. The editor branch below reaches it naturally via its cursor
      // composite step; the startup page has no cursor to draw, but still needs this call to actually get painted.
      // A start page also has no persisted content to reason about, so this always forces a full repaint.
      swingWin.onCursorOverlayReady(None)(_ => Nil)
    } { scene =>
      RendererFramePlanner
        .renderFrame(
          state0,
          cursorVisible = false,
          surface,
          viewportSize,
          scene,
          codeFont,
          textFont,
          uiFont,
          swingWin.metrics,
          uiMetrics,
          cursorColor = None,
          output,
          damage,
          bufferAnimations
        )
        .fold(false) { renderPlan =>
          val baseDirtyRegion = repaintRegion.get().map(RendererFrameState.toAwtRectangle)
          swingWin.onCursorOverlayReady(baseDirtyRegion) { image =>
            val cursorSurface =
              Java2DRenderSurface.forImage(image, swingWin.metrics, codeFont, swingWin.canvas, _ => ())
            paintCursorsOnly(
              state0,
              cursorSurface,
              scene.calculatedLayout,
              renderPlan,
              true,
              cursorColor,
              codeFont,
              textFont,
              uiFont,
              swingWin.metrics,
              uiMetrics,
              bufferAnimations
            ).map(RendererFrameState.toAwtRectangle)
          }
        }
    }
