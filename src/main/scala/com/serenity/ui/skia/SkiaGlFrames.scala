package com.serenity.ui.skia

import scala.util.Try

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{BackendCaches, FontSpec, RenderSurface, ScreenIdentity, SurfaceTextCaches}

/** What the renderer needs to draw one frame of a GL window: the geometry it lays out for and the caches it draws with.
  * The pixel size follows from the logical size and the scale.
  */
final case class GlFrameSettings(
    metrics: CellMetrics,
    font: FontSpec,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    deviceScale: Double,
    textCaches: SurfaceTextCaches,
    backendCaches: BackendCaches,
    screen: ScreenIdentity
)

/** Draws frames straight onto the persistent offscreen surface of a GL window (#2380): the renderer's own drawing runs
  * over a [[SkiaRenderSurface.retaining]] that surface, so it may draw only what changed, and the frame stays there
  * until the next one. Showing it on the window is the presenter's step.
  *
  * Call it on the thread the GL context is current on; frames drawn there are drawn in the order they are asked for.
  */
final class SkiaGlFrames private (context: SkiaGlContext, targets: SkiaGlTargets):

  /** Runs `render` over the offscreen surface and flushes the GPU work, or says why it could not: no surface of that
    * size, or `render` failing. A frame that failed has drawn whatever it drew before; the next frame still draws.
    */
  def draw(settings: GlFrameSettings)(render: RenderSurface => Unit): Either[String, Unit] =
    val widthPx  = SkiaRenderSurface.deviceDimension(settings.logicalWidthPx, settings.deviceScale)
    val heightPx = SkiaRenderSurface.deviceDimension(settings.logicalHeightPx, settings.deviceScale)
    targets.ensure(widthPx, heightPx, settings.deviceScale).flatMap { made =>
      val surface = SkiaRenderSurface.retaining(
        made.offscreen,
        settings.metrics,
        settings.font,
        settings.logicalWidthPx,
        settings.logicalHeightPx,
        settings.deviceScale,
        SkiaTextCaches.of(settings.backendCaches),
        settings.textCaches.styledFonts,
        layerCacheOwner = Some(settings.screen)
      )
      Try {
        render(surface)
        surface.flush()
      }.toEither.left
        .map(failure => Option(failure.getMessage).getOrElse(failure.getClass.getName))
        .map(_ => context.direct.flushAndSubmit(made.offscreen, false))
    }

object SkiaGlFrames:

  def apply(context: SkiaGlContext, targets: SkiaGlTargets): SkiaGlFrames = new SkiaGlFrames(context, targets)
