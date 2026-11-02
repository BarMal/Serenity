package com.serenity.ui.skia

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import javax.swing.JPanel

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.Java2DRenderSurface.DeviceScale
import com.serenity.ui.renderer.{
  FrameSurfaceFactory,
  FrameSurfaceRequest,
  Java2DRenderSurface,
  PresentationSite,
  ScreenIdentity,
  SurfaceGeometry
}
import com.serenity.ui.terminal.SwingPresentationSite
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The frame request names where a frame is presented as a [[PresentationSite]], which has no window-system type in it.
  * The layer factory hosts a GPU layer only on the one kind of site it can: a Swing canvas (#2380, phase 2).
  */
class LayerSurfaceFactorySiteSpec extends AnyFlatSpec with Matchers:

  private val geometry = SurfaceGeometry(100, 60, DeviceScale(1.0, 1.0), ScreenIdentity(new Object))
  private val caches   = RenderCaches.create()

  private def request(site: PresentationSite): FrameSurfaceRequest =
    FrameSurfaceRequest(
      CellMetrics(8, 16, 12),
      new Font(Font.MONOSPACED, Font.PLAIN, 12),
      geometry,
      site,
      _ => (),
      (w, h, t) => new BufferedImage(w, h, t),
      caches.surfaceText,
      caches.backendCaches,
      () => None
    )

  "A layer factory" should "draw on Java2D, and build no host, when the site is not a Swing canvas" in {
    val built   = new AtomicInteger(0)
    val factory = new LayerSurfaceFactory(
      _ =>
        val _ = built.incrementAndGet()
        throw new IllegalStateException("no host is built for a site that is not a canvas")
      ,
      FrameSurfaceFactory.Java2D
    )

    val surface = factory.create(request(PresentationSite.None))

    surface shouldBe a[Java2DRenderSurface]
    built.get shouldBe 0
  }

  it should "ask for a host on the panel of a Swing canvas site" in {
    val panel = new JPanel
    panel.setSize(100, 60)
    val seen = new AtomicReference[Option[JPanel]](None)
    val factory = new LayerSurfaceFactory(
      canvas =>
        seen.set(Some(canvas))
        throw new IllegalStateException("a host that cannot be built falls back to Java2D")
      ,
      FrameSurfaceFactory.Java2D
    )

    val surface = factory.create(request(SwingPresentationSite(panel)))

    seen.get.exists(_ eq panel) shouldBe true
    surface shouldBe a[Java2DRenderSurface]
  }

  "A Swing presentation site" should "hold the panel it stands for" in {
    val panel = new JPanel

    SwingPresentationSite(panel).panel should be theSameInstanceAs panel
  }

  "The absence of a site" should "be a site, so a target with no window system can still be asked for one" in {
    PresentationSite.None should not be a[SwingPresentationSite]
  }
