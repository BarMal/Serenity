package com.serenity.app

import java.awt.Font

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.display.DisplayScale.DeviceScale
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.FakeRenderTarget
import com.serenity.ui.skia.SkiaTextCaches
import java.util.concurrent.atomic.AtomicReference
import org.jetbrains.skia.Surface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The startup warm-up draws what a window's frames draw -- on a Skia raster -- into a surface of its own that nothing
  * ever presents.
  */
class OffscreenWarmUpFramesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(80, 24)
  private val font     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val metrics  = CellMetrics.fromFont(font)
  private val widthPx  = viewport.width * metrics.charWidth
  private val heightPx = viewport.height * metrics.lineHeight

  private val display = RuntimeDisplayState.Snapshot(font, font, font, metrics, metrics, metrics)

  private def target = new FakeRenderTarget(metrics, viewport, widthPx, heightPx, DeviceScale(1.0, 1.0))

  private val state: AppState =
    StartupWarmUp.seeded(AppState.initial, StartupWarmUp.document(20)).getOrElse(AppState.initial)

  final private case class Run(
      target: FakeRenderTarget,
      requested: Vector[(Int, Int)],
      rasters: Vector[Surface],
      caches: RenderCaches,
      closedInside: Vector[Boolean]
  )

  private def draw: Run =
    val requested = new AtomicReference(Vector.empty[(Int, Int)])
    val rasters   = new AtomicReference(Vector.empty[Surface])
    val caches    = RenderCaches.create()
    val fake      = target
    val makeRaster = (width: Int, height: Int) =>
      val raster = Surface.Companion.makeRasterN32Premul(width, height)
      val _      = requested.updateAndGet(_ :+ (width -> height))
      val _      = rasters.updateAndGet(_ :+ raster)
      raster
    val closedInside = OffscreenWarmUpFrames
      .forTarget(fake, () => viewport, () => display, makeRaster)
      .use(frames =>
        for
          _ <- frames.full(state, Damage.Everything, caches)
          _ <- frames.cursorOnly(state, caches)
        yield rasters.get().map(_.isClosed)
      )
      .unsafeRunSync()
    Run(fake, requested.get(), rasters.get(), caches, closedInside)

  "OffscreenWarmUpFrames" should "draw a full frame and a caret-only frame without presenting either" in {
    val run = draw

    run.target.published shouldBe empty
    run.target.baseOnly shouldBe empty
    run.target.overlayCarets shouldBe empty
    run.target.acquisitions shouldBe empty
  }

  it should "draw on one Skia raster of the window's device size, kept across both frames" in {
    val run = draw

    run.requested shouldBe Vector(widthPx -> heightPx)
  }

  it should "close the raster when the warm-up is released, and not before" in {
    val run = draw

    run.closedInside shouldBe Vector(false)
    run.rasters.map(_.isClosed) shouldBe Vector(true)
  }

  it should "warm the Skia text caches the window's frames share" in {
    val run = draw

    SkiaTextCaches.of(run.caches.backendCaches).warmup.snapshot.faces should be > 0
  }

end OffscreenWarmUpFramesSpec
