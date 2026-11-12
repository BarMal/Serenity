package com.serenity.app

import cats.effect.{IO, Resource}
import com.serenity.frontend.FrontendRuntime
import com.serenity.state.manager.RenderCaches
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.{FontSpec, RenderTarget, RendererCursorOverlay, RendererEntryPoints, ScreenIdentity}
import com.serenity.ui.skia.{SkiaRasterSurfaceFactory, SkiaRenderSurface, SkiaTextCaches}
import org.jetbrains.skia.Surface

/** Window-sized frames for the startup warm-up, painted on a Skia raster of their own that is never presented. */
object OffscreenWarmUpFrames:

  /** Sized and scaled like `target`, so the warm-up takes the same paths a real frame does. The raster is made when the
    * warm-up starts and closed with it, rather than held for the life of the window, and persists across the warm-up's
    * frames as the window's does.
    */
  def forTarget(
    target: RenderTarget,
    viewportSize: () => ViewportSize,
    display: () => RuntimeDisplayState.Snapshot,
    rasterFor: (Int, Int) => Surface = SkiaRasterSurfaceFactory.newSurface
  ): Resource[IO, FrontendRuntime.OffscreenFrames] =
    val geometry = target.geometry
    val scale    = geometry.deviceScale
    val screen   = ScreenIdentity(new Object)
    Resource
      .make(IO(rasterFor(geometry.deviceWidthPx, geometry.deviceHeightPx)))(raster => IO(raster.close()))
      .map { raster =>
        def surface(snapshot: RuntimeDisplayState.Snapshot, caches: RenderCaches): SkiaRenderSurface =
          SkiaRenderSurface.retaining(
            raster,
            snapshot.codeMetrics,
            FontSpec.fromAwt(snapshot.codeFont),
            geometry.logicalWidthPx,
            geometry.logicalHeightPx,
            scale.x,
            SkiaTextCaches.of(caches.backendCaches),
            caches.surfaceText.styledFonts,
            layerCacheOwner = Some(screen)
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
      }
