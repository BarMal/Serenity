package com.serenity.app

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.ui.renderer.{FrameSurfaceFactory, FrameSurfaceRequest, RenderSurface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceCatalogueSpec extends AnyFlatSpec with Matchers:

  private val skiaFactory: FrameSurfaceFactory = new FrameSurfaceFactory:
    def create(request: FrameSurfaceRequest): RenderSurface = FrameSurfaceFactory.Java2D.create(request)

  private val rasterFactory: FrameSurfaceFactory = new FrameSurfaceFactory:
    def create(request: FrameSurfaceRequest): RenderSurface = FrameSurfaceFactory.Java2D.create(request)

  final private class Built:
    val skia: AtomicInteger   = new AtomicInteger(0)
    val raster: AtomicInteger = new AtomicInteger(0)

    val catalogue: SurfaceCatalogue = SurfaceCatalogue(
      () =>
        skia.incrementAndGet()
        skiaFactory
      ,
      () =>
        raster.incrementAndGet()
        rasterFactory
    )

  "SurfaceCatalogue.factoryFor" should "build nothing for the terminal" in {
    val built = new Built
    built.catalogue.factoryFor(RenderBackend.Terminal) shouldBe None
    (built.skia.get, built.raster.get) shouldBe (0, 0)
  }

  it should "build the Skia factory only when Skia is asked for" in {
    val built = new Built
    built.catalogue.factoryFor(RenderBackend.Skia) shouldBe Some(skiaFactory)
    (built.skia.get, built.raster.get) shouldBe (1, 0)
  }

  it should "build the raster factory only for the raster backend" in {
    val built = new Built
    built.catalogue.factoryFor(RenderBackend.SkiaRaster) shouldBe Some(rasterFactory)
    (built.skia.get, built.raster.get) shouldBe (0, 1)
  }

  it should "offer both Skia factories in the catalogue a release uses" in {
    SurfaceCatalogue.Available.factoryFor(RenderBackend.Skia) shouldBe
      Some(com.serenity.ui.skia.SkiaLayerSurfaceFactory)
    SurfaceCatalogue.Available.factoryFor(RenderBackend.SkiaRaster) shouldBe
      Some(com.serenity.ui.skia.SkiaRasterSurfaceFactory)
  }

end SurfaceCatalogueSpec
