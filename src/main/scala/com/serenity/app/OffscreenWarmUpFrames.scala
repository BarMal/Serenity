package com.serenity.app

import java.awt.image.BufferedImage
import javax.swing.JPanel

import cats.effect.{IO, Resource}
import com.serenity.frontend.FrontendRuntime
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.{
  FontSpec,
  Java2DRenderSurface,
  RendererCursorOverlay,
  RendererEntryPoints,
  ScreenIdentity
}

/** Window-sized Java2D frames for the startup warm-up, drawn into an image of their own that is never presented. */
object OffscreenWarmUpFrames:

  /** Sized and scaled like `canvas`, so the warm-up takes the same paths a real frame does. The image is allocated when
    * the warm-up starts and dropped with it, rather than held for the life of the window.
    */
  def forCanvas(
    canvas: JPanel,
    viewportSize: () => ViewportSize,
    display: () => RuntimeDisplayState.Snapshot
  ): Resource[IO, FrontendRuntime.OffscreenFrames] =
    Resource.eval(IO {
      val width  = Java2DRenderSurface.logicalCanvasDimension(canvas.getWidth, canvas.getPreferredSize.width)
      val height = Java2DRenderSurface.logicalCanvasDimension(canvas.getHeight, canvas.getPreferredSize.height)
      val scale  = Java2DRenderSurface.deviceScaleFor(canvas)
      val screen = ScreenIdentity(new Object)
      val image = new BufferedImage(
        Java2DRenderSurface.deviceImageDimension(width, scale.x),
        Java2DRenderSurface.deviceImageDimension(height, scale.y),
        BufferedImage.TYPE_INT_ARGB
      )
      def surface(snapshot: RuntimeDisplayState.Snapshot): Java2DRenderSurface =
        new Java2DRenderSurface(
          image,
          snapshot.codeMetrics,
          snapshot.codeFont,
          _ => (),
          logicalWidthPx = width,
          logicalHeightPx = height,
          deviceScaleX = scale.x,
          deviceScaleY = scale.y,
          contentPersists = true,
          layerCacheOwnerOverride = Some(screen)
        )

      FrontendRuntime.OffscreenFrames(
        full = (state, damage, caches) =>
          IO {
            val snapshot = display()
            RendererEntryPoints.render(
              state,
              cursorVisible = false,
              surface(snapshot),
              viewportSize(),
              FontSpec.fromAwt(snapshot.codeFont),
              FontSpec.fromAwt(snapshot.textFont),
              FontSpec.fromAwt(snapshot.uiFont),
              snapshot.codeMetrics,
              snapshot.uiMetrics,
              None,
              damage,
              caches
            )
          },
        cursorOnly = (state, caches) =>
          IO {
            val snapshot = display()
            val _ = RendererCursorOverlay.renderCursorOnly(
              state,
              cursorVisible = true,
              surface(snapshot),
              viewportSize(),
              FontSpec.fromAwt(snapshot.codeFont),
              FontSpec.fromAwt(snapshot.textFont),
              FontSpec.fromAwt(snapshot.uiFont),
              snapshot.codeMetrics,
              snapshot.uiMetrics,
              None,
              caches
            )
          }
      )
    })
