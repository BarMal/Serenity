package com.serenity.app

import cats.effect.IO
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{
  FontSpec,
  FrameSurfaceFactory,
  HostedCanvas,
  HostedFrame,
  HostedOutcome,
  RendererCursorOverlay,
  RendererEntryPoints,
  ScreenIdentity
}
import com.serenity.ui.terminal.SwingWindow

object GuiFramePainting:

  /** A frame that failed inside a hosted canvas fails the frame loop's step, as one that fails on an image does. */
  def settle(outcome: HostedOutcome): IO[Unit] =
    outcome match
      case HostedOutcome.Failed(cause) => IO.raiseError(cause)
      case _                           => IO.unit

  /** Paint a whole frame.
    *
    * `display` is taken once by the caller and threaded through: reading each font and metric from the runtime
    * separately would let a concurrent font-config change land mid-frame and paint glyphs at one generation's advance
    * with another's metrics.
    *
    * A backend with a hosted canvas draws the frame inside the canvas's own callback instead, and reports how that
    * ended.
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
  ): HostedOutcome =
    surfaces.hostedCanvas match
      case Some(hosted) =>
        paintHostedFrame(
          hosted,
          state,
          cursorVisible,
          cursorColor,
          window.viewportSize,
          window.metrics,
          ScreenIdentity(window.canvas),
          display,
          damage,
          caches
        )
      case None =>
        paintOnImage(state, cursorVisible, cursorColor, window, display, damage, caches, surfaces)
        HostedOutcome.Drawn

  /** One frame, caret included, drawn by a canvas that keeps nothing between frames, so every frame is a whole one and
    * a caret blink costs the same as any other repaint. The caret goes in with the base frame rather than over it
    * afterwards, because a surface over a window's canvas can only be flushed once.
    */
  def paintHostedFrame(
    hosted: HostedCanvas,
    state: AppState,
    cursorVisible: Boolean,
    cursorColor: Option[RenderColor],
    viewportSize: ViewportSize,
    metrics: CellMetrics,
    screen: ScreenIdentity,
    display: RuntimeDisplayState.Snapshot,
    damage: Damage,
    caches: RenderCaches
  ): HostedOutcome =
    val codeFont = FontSpec.fromAwt(display.codeFont)
    val textFont = FontSpec.fromAwt(display.textFont)
    val uiFont   = FontSpec.fromAwt(display.uiFont)
    hosted.drawFrame(HostedFrame(metrics, codeFont, screen)) { surface =>
      RendererEntryPoints.render(
        state,
        cursorVisible,
        surface,
        viewportSize,
        codeFont,
        textFont,
        uiFont,
        metrics,
        display.uiMetrics,
        cursorColor,
        damage,
        caches
      )
    }

  private def paintOnImage(
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

  /** Repaint only the cursor overlay, falling back to a full frame when the overlay path declines. A hosted canvas has
    * no frame underneath to paint a caret over, so for it a caret repaint is a whole frame.
    */
  def paintCursorFrame(
    state: com.serenity.state.models.AppState,
    cursorVisible: Boolean,
    cursorColor: Option[RenderColor],
    window: SwingWindow,
    display: RuntimeDisplayState.Snapshot,
    damage: com.serenity.state.models.Damage,
    caches: com.serenity.state.manager.RenderCaches,
    surfaces: FrameSurfaceFactory
  ): HostedOutcome =
    if surfaces.hostedCanvas.isDefined then
      paintFullFrame(state, cursorVisible, cursorColor, window, display, damage, caches, surfaces)
    else
      paintCursorOnImage(state, cursorVisible, cursorColor, window, display, damage, caches, surfaces)
      HostedOutcome.Drawn

  private def paintCursorOnImage(
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
