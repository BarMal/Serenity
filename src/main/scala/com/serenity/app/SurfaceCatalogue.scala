package com.serenity.app

import com.serenity.ui.renderer.FrameSurfaceFactory
import com.serenity.ui.skia.{SkiaLayerSurfaceFactory, SkiaRasterSurfaceFactory}

/** The surface factories this build offers. Each is a thunk so that building the factory, and with it loading Skiko and
  * Kotlin classes, happens only when that backend was chosen. `skia` paints through a Skiko layer; `skiaRaster` paints
  * on the CPU into an image that the Swing canvas presents.
  */
final case class SurfaceCatalogue(skia: () => FrameSurfaceFactory, skiaRaster: () => FrameSurfaceFactory):

  /** `None` for the terminal, which paints no window. */
  def factoryFor(backend: RenderBackend): Option[FrameSurfaceFactory] =
    backend match
      case RenderBackend.Terminal   => None
      case RenderBackend.Skia       => Some(skia())
      case RenderBackend.SkiaRaster => Some(skiaRaster())

object SurfaceCatalogue:

  /** What a release offers: the GPU layer, and the CPU rasteriser a Wayland launch can ask for. */
  val Available: SurfaceCatalogue = SurfaceCatalogue(() => SkiaLayerSurfaceFactory, () => SkiaRasterSurfaceFactory)
