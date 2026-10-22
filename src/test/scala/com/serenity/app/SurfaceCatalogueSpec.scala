package com.serenity.app

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.ui.renderer.{FrameSurfaceFactory, FrameSurfaceRequest, RenderSurface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceCatalogueSpec extends AnyFlatSpec with Matchers:

  private val skiaFactory: FrameSurfaceFactory = new FrameSurfaceFactory:
    def create(request: FrameSurfaceRequest): RenderSurface = FrameSurfaceFactory.Java2D.create(request)

  "SurfaceCatalogue.factoryFor" should "hand out the Java2D factory for the Java2D backend without building Skia" in {
    val built = new AtomicInteger(0)
    val catalogue = SurfaceCatalogue(Some(() =>
      built.incrementAndGet(); skiaFactory
    ))
    catalogue.factoryFor(RenderBackend.Java2D) shouldBe theSameInstanceAs(FrameSurfaceFactory.Java2D)
    built.get shouldBe 0
  }

  it should "build the Skia factory only when Skia is asked for" in {
    val built = new AtomicInteger(0)
    val catalogue = SurfaceCatalogue(Some(() =>
      built.incrementAndGet(); skiaFactory
    ))
    catalogue.factoryFor(RenderBackend.Skia) shouldBe theSameInstanceAs(skiaFactory)
    built.get shouldBe 1
  }

  it should "offer Java2D alone by default" in {
    SurfaceCatalogue.Java2DOnly.skia shouldBe None
    SurfaceCatalogue.Java2DOnly.factoryFor(RenderBackend.Skia) shouldBe theSameInstanceAs(FrameSurfaceFactory.Java2D)
  }

  it should "build the raster factory only for the raster backend, and fall back to Java2D without one" in {
    val built = new AtomicInteger(0)
    val raster = SurfaceCatalogue(
      None,
      Some(() =>
        built.incrementAndGet(); skiaFactory
      )
    )
    raster.factoryFor(RenderBackend.Java2D) shouldBe theSameInstanceAs(FrameSurfaceFactory.Java2D)
    built.get shouldBe 0
    raster.factoryFor(RenderBackend.SkiaRaster) shouldBe theSameInstanceAs(skiaFactory)
    built.get shouldBe 1
    SurfaceCatalogue.Java2DOnly.factoryFor(RenderBackend.SkiaRaster) shouldBe
      theSameInstanceAs(FrameSurfaceFactory.Java2D)
  }

  it should "offer both Skia factories in the catalogue a release uses" in {
    SurfaceCatalogue.Available.factoryFor(RenderBackend.Skia) shouldBe
      theSameInstanceAs(com.serenity.ui.skia.SkiaLayerSurfaceFactory.Default)
    SurfaceCatalogue.Available.factoryFor(RenderBackend.SkiaRaster) shouldBe
      theSameInstanceAs(com.serenity.ui.skia.SkiaRasterSurfaceFactory)
  }
