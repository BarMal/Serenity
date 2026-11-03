package com.serenity.app

import com.serenity.ui.color.RenderColor
import com.serenity.ui.renderer.{FontSpec, FrameSurfaceFactory, RendererCursorOverlay, RendererEntryPoints}
import com.serenity.ui.terminal.SwingWindow

object GuiFramePainting:

  /** Paint a whole frame.
    *
    * `display` is taken once by the caller and threaded through: reading each font and metric from the runtime
    * separately would let a concurrent font-config change land mid-frame and paint glyphs at one generation's advance
    * with another's metrics.
    */
  def paintFullFrame(
    state: com.serenity.state.models.AppState,
    cursorVisible: Boolean,
    cursorColor: Option[RenderColor],
    window: SwingWindow,
    display: RuntimeDisplayState.Snapshot,
    damage: com.serenity.state.models.Damage,
    caches: com.serenity.state.manager.RenderCaches,
    surfaces: FrameSurfaceFactory
  ): Unit =
    if cursorVisible then
      val _ = RendererCursorOverlay.renderWithCursorOverlay(
        state,
        window,
        FontSpec.fromAwt(display.codeFont),
        FontSpec.fromAwt(display.textFont),
        FontSpec.fromAwt(display.uiFont),
        display.uiMetrics,
        cursorColor,
        damage,
        caches,
        surfaces
      )
      ()
    else
      RendererEntryPoints.render(
        state,
        cursorVisible = false,
        window,
        FontSpec.fromAwt(display.codeFont),
        FontSpec.fromAwt(display.textFont),
        FontSpec.fromAwt(display.uiFont),
        display.uiMetrics,
        None,
        repaintOnFlush = SwingWindow.shouldRepaintBaseFrameBeforeCursorOverlay(cursorVisible),
        damage = damage,
        caches = caches,
        surfaces = surfaces
      )

  /** Repaint only the cursor overlay, falling back to a full frame when the overlay path declines. */
  def paintCursorFrame(
    state: com.serenity.state.models.AppState,
    cursorVisible: Boolean,
    cursorColor: Option[RenderColor],
    window: SwingWindow,
    display: RuntimeDisplayState.Snapshot,
    damage: com.serenity.state.models.Damage,
    caches: com.serenity.state.manager.RenderCaches,
    surfaces: FrameSurfaceFactory
  ): Unit =
    val rendered = RendererCursorOverlay.renderCursorOnly(
      state,
      cursorVisible,
      window,
      FontSpec.fromAwt(display.codeFont),
      FontSpec.fromAwt(display.textFont),
      FontSpec.fromAwt(display.uiFont),
      display.uiMetrics,
      cursorColor,
      caches
    )
    if !rendered then
      RendererEntryPoints.render(
        state,
        cursorVisible,
        window,
        FontSpec.fromAwt(display.codeFont),
        FontSpec.fromAwt(display.textFont),
        FontSpec.fromAwt(display.uiFont),
        display.uiMetrics,
        cursorColor,
        repaintOnFlush = true,
        damage = damage,
        caches = caches,
        surfaces = surfaces
      )
