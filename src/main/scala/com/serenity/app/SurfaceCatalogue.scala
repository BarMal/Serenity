package com.serenity.app

import com.serenity.ui.renderer.FrameSurfaceFactory

/** The surface factories this build can offer. `skia` is a thunk so that building the Skia factory, and with it loading
  * Skiko and Kotlin classes, happens only when Skia was chosen.
  */
final case class SurfaceCatalogue(skia: Option[() => FrameSurfaceFactory]):

  def factoryFor(backend: RenderBackend): FrameSurfaceFactory =
    backend match
      case RenderBackend.Java2D => FrameSurfaceFactory.Java2D
      case RenderBackend.Skia   => skia.fold(FrameSurfaceFactory.Java2D)(build => build())

object SurfaceCatalogue:
  val Java2DOnly: SurfaceCatalogue = SurfaceCatalogue(None)
