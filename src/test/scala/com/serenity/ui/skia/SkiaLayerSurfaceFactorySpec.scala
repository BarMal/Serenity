package com.serenity.ui.skia

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import javax.swing.JPanel

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.FrameSurfaceRequest
import com.serenity.ui.terminal.{LayerPresenter, SwingWindow}
import org.jetbrains.skia.Picture
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaLayerSurfaceFactorySpec extends AnyFlatSpec with Matchers:

  private val red = RenderColor.fromRgba(200, 30, 30)

  final private class FakeHost extends LayerHost:
    val published              = new AtomicInteger(0)
    val disposed               = new AtomicInteger(0)
    val pictures               = new AtomicReference(List.empty[Picture])
    def baseAvailable: Boolean = published.get > 0

    def publish(picture: Picture): Unit =
      published.incrementAndGet()
      pictures.updateAndGet(picture :: _)

    def showCarets(carets: List[SwingWindow.CaretPaint]): Unit = ()
    def setCursor(cursor: java.awt.Cursor): Unit               = ()
    def dispose(): Unit                                        = disposed.incrementAndGet()

  final private class Harness(buildHost: JPanel => LayerHost):
    val canvas = new JPanel
    canvas.setSize(100, 60)
    val caches   = RenderCaches.create()
    val shown    = new AtomicReference(List.empty[BufferedImage])
    val attached = new AtomicReference(List.empty[LayerPresenter])
    val factory  = new SkiaLayerSurfaceFactory(buildHost)

    private def request =
      FrameSurfaceRequest(
        CellMetrics(8, 16, 12),
        new Font(Font.MONOSPACED, Font.PLAIN, 12),
        canvas,
        image =>
          val _ = shown.updateAndGet(image :: _)
        ,
        (w, h, t) => new BufferedImage(w, h, t),
        caches.surfaceText,
        caches.backendCaches,
        () => None,
        attachLayer = presenter =>
          val _ = attached.updateAndGet(presenter :: _)
      )

    def frame(): Unit =
      val surface = factory.create(request)
      surface.pixels.fillPixelRect(0, 0, 20, 20, red)
      surface.flush()

  "SkiaLayerSurfaceFactory" should "publish each frame as a picture to the window's host and attach the host once" in {
    val host   = new FakeHost
    val builds = new AtomicInteger(0)
    val harness = new Harness(_ =>
      builds.incrementAndGet(); host
    )
    harness.frame()
    harness.frame()
    harness.frame()
    host.published.get shouldBe 3
    builds.get shouldBe 1
    harness.attached.get should have size 1
    harness.attached.get.head shouldBe theSameInstanceAs(host)
    harness.shown.get shouldBe empty
  }

  it should "fall back to Java2D, and show frames, when the layer cannot be created" in {
    val builds = new AtomicInteger(0)
    val harness = new Harness(_ =>
      builds.incrementAndGet(); throw new IllegalStateException("no GL context")
    )
    harness.frame()
    harness.frame()
    harness.shown.get should have size 2
    harness.attached.get shouldBe empty
    builds.get shouldBe 1
  }

  it should "keep its text caches in the owner's backend slot" in {
    val harness = new Harness(_ => new FakeHost)
    harness.frame()
    SkiaTextCaches.heldBy(harness.caches.backendCaches) shouldBe true
  }
