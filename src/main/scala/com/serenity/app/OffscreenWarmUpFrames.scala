package com.serenity.app

import java.awt.image.BufferedImage

import cats.effect.{IO, Resource}
import com.serenity.frontend.FrontendRuntime
import com.serenity.state.manager.RenderCaches
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.{
  FontSpec,
  Java2DRenderSurface,
  RenderTarget,
  RendererCursorOverlay,
  RendererEntryPoints,
  ScreenIdentity
}

/** Window-sized Java2D frames for the startup warm-up, drawn into an image of their own that is never presented. */
object OffscreenWarmUpFrames:

  /** Sized and scaled like `target`, so the warm-up takes the same paths a real frame does. The image is allocated when
    * the warm-up starts and dropped with it, rather than held for the life of the window.
    */
  def forTarget(
    target: RenderTarget,
    viewportSize: () => ViewportSize,
    display: () => RuntimeDisplayState.Snapshot
  ): Resource[IO, FrontendRuntime.OffscreenFrames] =
    Resource.eval(IO {
      val geometry = target.geometry
      val width    = geometry.logicalWidthPx
      val height   = geometry.logicalHeightPx
      val scale    = geometry.deviceScale
      val screen   = ScreenIdentity(new Object)
      val image    = new BufferedImage(geometry.deviceWidthPx, geometry.deviceHeightPx, BufferedImage.TYPE_INT_ARGB)
      def surface(snapshot: RuntimeDisplayState.Snapshot, caches: RenderCaches): Java2DRenderSurface =
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
          layerCacheOwnerOverride = Some(screen),
          textCaches = caches.surfaceText
        )

      FrontendRuntime.OffscreenFrames(
        full = (state, damage, caches) =>
          IO {
            val snapshot = display()
            RendererEntryPoints.render(
              state,
              cursorVisible = false,
              surface(snapshot, caches),
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
              surface(snapshot, caches),
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
