package com.serenity.app

import com.serenity.ui.renderer.FrameSurfaceFactory
import com.serenity.ui.skia.SkiaRasterSurfaceFactory

/** The surface factories this build can offer. Each Skia slot is a thunk so that building the factory, and with it
  * loading Skiko and Kotlin classes, happens only when that backend was chosen. `skia` paints through a Skiko layer;
  * `skiaRaster` paints on the CPU into an image that the Java2D canvas presents.
  */
final case class SurfaceCatalogue(
    skia: Option[() => FrameSurfaceFactory],
    skiaRaster: Option[() => FrameSurfaceFactory] = None
):

  def factoryFor(backend: RenderBackend): FrameSurfaceFactory =
    backend match
      case RenderBackend.Java2D     => FrameSurfaceFactory.Java2D
      case RenderBackend.Skia       => skia.fold(FrameSurfaceFactory.Java2D)(build => build())
      case RenderBackend.SkiaRaster => skiaRaster.fold(FrameSurfaceFactory.Java2D)(build => build())

object SurfaceCatalogue:
  val Java2DOnly: SurfaceCatalogue = SurfaceCatalogue(None)

  /** What a release offers: the CPU rasteriser. The layer surface joins it once it has been judged on a real GPU. */
  val Available: SurfaceCatalogue = SurfaceCatalogue(None, Some(() => SkiaRasterSurfaceFactory))
