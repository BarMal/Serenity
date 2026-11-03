package com.serenity.perf

import java.util.concurrent.atomic.AtomicReference

import cats.effect.unsafe.implicits.global
import com.serenity.diagnostics.FrameCounter
import com.serenity.rope.Balance
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.Damage
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.{FrameSurfaceFactory, FrameSurfaceRequest, RenderSurface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NullRenderSurfaceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val widthPx  = 900
  private val heightPx = 600
  private val metrics  = PresentBenchmarks.Fonts.default.metrics
  private val grid     = ViewportSize(widthPx / metrics.charWidth, heightPx / metrics.lineHeight)

  private def proseStates(): PresentBenchmarks.ProseStates =
    val (prose, _) = LaptopFrameBenchmarks.proseStateManager(grid = grid)
    PresentBenchmarks.ProseStates(prose)

  private def pixelsOf(image: java.awt.image.BufferedImage): Array[Int] =
    image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth)

  private def recording(delegate: FrameSurfaceFactory, created: AtomicReference[Vector[RenderSurface]]) =
    new FrameSurfaceFactory:
      def create(request: FrameSurfaceRequest): RenderSurface =
        val surface = delegate.create(request)
        created.updateAndGet(_ :+ surface)
        surface

  "A typing frame on the null surface" should "draw rows and publish an image of the window's size" in {
    val states = proseStates()
    val window = PresentBenchmarks.Window(NullSurfaceFactory(), widthPx, heightPx, 1.0, grid)
    val image  = window.render(states.beforeTyping, Damage.Everything)
    window.lastFrame(FrameCounter.RowsDrawn) should be > 0L
    (image.getWidth, image.getHeight) shouldBe (widthPx, heightPx)
  }

  it should "publish the device-pixel size on a scaled window" in {
    val states = proseStates()
    val window = PresentBenchmarks.Window(NullSurfaceFactory(), widthPx, heightPx, 2.0, grid)
    val image  = window.render(states.beforeTyping, Damage.Everything)
    (image.getWidth, image.getHeight) shouldBe (widthPx * 2, heightPx * 2)
  }

  it should "paint nothing" in {
    val states = proseStates()
    val window = PresentBenchmarks.Window(NullSurfaceFactory(), widthPx, heightPx, 1.0, grid)
    val first  = pixelsOf(window.render(states.beforeTyping, Damage.Everything))
    val second = pixelsOf(
      window.render(states.afterTyping, DamageProducer.forTransition(states.beforeTyping, states.afterTyping))
    )
    first.forall(rgb => (rgb & 0xffffff) == 0) shouldBe true
    second.forall(rgb => (rgb & 0xffffff) == 0) shouldBe true
  }

  it should "report the same persistent content key and layer owner on every frame" in {
    val states   = proseStates()
    val created  = new AtomicReference(Vector.empty[RenderSurface])
    val window   = PresentBenchmarks.Window(recording(NullSurfaceFactory(), created), widthPx, heightPx, 1.0, grid)
    val _        = window.render(states.beforeTyping, Damage.Everything)
    val _        = window.render(states.afterTyping, Damage.Everything)
    val surfaces = created.get
    surfaces.size should be >= 2
    surfaces.flatMap(_.persistentContentKey).distinct should have size 1
    surfaces.map(_.persistentContentKey).forall(_.isDefined) shouldBe true
    surfaces.map(_.layerCacheOwner).distinct should have size 1
  }

  "The null surface" should "round-trip its background colour" in {
    val created = new AtomicReference(Vector.empty[RenderSurface])
    val states  = proseStates()
    val window  = PresentBenchmarks.Window(recording(NullSurfaceFactory(), created), widthPx, heightPx, 1.0, grid)
    val _       = window.render(states.beforeTyping, Damage.Everything)
    val surface = created.get.head
    surface.setBackgroundColor(RenderColor.White)
    surface.getBackgroundColor shouldBe RenderColor.White
    surface.setBackgroundColor(RenderColor.Black)
    surface.getBackgroundColor shouldBe RenderColor.Black
  }
