package com.serenity.ui.skia

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import javax.swing.JPanel

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FrameSurfaceFactory, FrameSurfaceRequest, Java2DRenderSurface}
import com.serenity.ui.terminal.{LayerPresenter, SwingCanvasGeometry, SwingPresentationSite, SwingWindow}
import org.jetbrains.skia.{Bitmap, Image, Picture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaLayerSurfaceFactorySpec extends AnyFlatSpec with Matchers:

  private val red = RenderColor.fromRgba(200, 30, 30)

  final private class FakeHost extends LayerHost:
    val published              = new AtomicInteger(0)
    val disposed               = new AtomicInteger(0)
    val pictures               = new AtomicReference(List.empty[Picture])
    val images                 = new AtomicReference(List.empty[Image])
    def baseAvailable: Boolean = published.get > 0

    def publish(picture: Picture): Unit =
      published.incrementAndGet()
      pictures.updateAndGet(picture :: _)

    def publishImage(image: Image): Unit =
      published.incrementAndGet()
      images.updateAndGet(image :: _)

    def showCarets(carets: List[SwingWindow.CaretPaint]): Unit = ()
    def setCursor(cursor: java.awt.Cursor): Unit               = ()
    def dispose(): Unit                                        = disposed.incrementAndGet()
    def onRendered(callback: () => Unit): Unit                 = ()
    def renderApi: String                                      = "FAKE"

  final private class Harness(
      buildHost: JPanel => LayerHost,
      fallback: FrameSurfaceFactory = FrameSurfaceFactory.Java2D,
      frames: LayerFrames = LayerFrames.Picture
  ):
    val canvas = new JPanel
    canvas.setSize(100, 60)
    val caches   = RenderCaches.create()
    val shown    = new AtomicReference(List.empty[BufferedImage])
    val attached = new AtomicReference(List.empty[LayerPresenter])
    val factory  = new LayerSurfaceFactory(buildHost, fallback, frames)

    private def request(caches: RenderCaches) =
      FrameSurfaceRequest(
        CellMetrics(8, 16, 12),
        new Font(Font.MONOSPACED, Font.PLAIN, 12),
        SwingCanvasGeometry.of(canvas),
        SwingPresentationSite(canvas),
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

    def rawRequest: FrameSurfaceRequest = request(caches)

    def frame(): Unit = frameWith(caches)

    /** A frame drawn with `frameCaches`, as the render benchmark's cold frames are: new caches, the same window. */
    def frameWith(frameCaches: RenderCaches): Unit =
      val surface = factory.create(request(frameCaches))
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

  it should "keep one host for a window whatever caches its frames come with" in {
    val host   = new FakeHost
    val builds = new AtomicInteger(0)
    val harness = new Harness(_ =>
      builds.incrementAndGet(); host
    )
    List.fill(5)(RenderCaches.create()).foreach(harness.frameWith)
    builds.get shouldBe 1
    harness.attached.get should have size 1
    host.published.get shouldBe 5
  }

  it should "not try to build a layer that failed again because the caches changed" in {
    val builds = new AtomicInteger(0)
    val harness = new Harness(_ =>
      builds.incrementAndGet(); throw new IllegalStateException("no GL context")
    )
    List.fill(3)(RenderCaches.create()).foreach(harness.frameWith)
    builds.get shouldBe 1
    harness.shown.get should have size 3
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

  it should "draw with the fallback factory it was given, and attach no layer, when the layer cannot be created" in {
    val created = new AtomicInteger(0)
    val counting: FrameSurfaceFactory = request =>
      created.incrementAndGet()
      FrameSurfaceFactory.Java2D.create(request)
    val harness = new Harness(_ => throw new IllegalStateException("no GL context"), counting)
    harness.frame()
    harness.frame()
    harness.frame()
    created.get shouldBe 3
    harness.shown.get should have size 3
    harness.attached.get shouldBe empty
  }

  "LayerFrames" should "read the frame form from the property, and draw pictures when it says nothing usable" in {
    LayerFrames.parse(Some("image")) shouldBe LayerFrames.Image
    LayerFrames.parse(Some(" IMAGE ")) shouldBe LayerFrames.Image
    LayerFrames.parse(Some("picture")) shouldBe LayerFrames.Picture
    LayerFrames.parse(Some("raster")) shouldBe LayerFrames.Picture
    LayerFrames.parse(None) shouldBe LayerFrames.Picture
  }

  "SkiaLayerSurfaceFactory in image mode" should "publish each frame as an image of the window's device size, not a picture" in {
    val host    = new FakeHost
    val harness = new Harness(_ => host, frames = LayerFrames.Image)
    harness.frame()
    harness.frame()
    host.pictures.get shouldBe empty
    host.images.get should have size 2
    host.images.get.foreach { image =>
      image.getWidth shouldBe 100
      image.getHeight shouldBe 60
    }
    harness.attached.get should have size 1
    harness.shown.get shouldBe empty
  }

  it should "publish an image holding what the frame painted, still readable after the frame ended" in {
    val host    = new FakeHost
    val harness = new Harness(_ => host, frames = LayerFrames.Image)
    harness.frame()
    val bitmap = Bitmap.Companion.makeFromImage(host.images.get.head)
    bitmap.getColor(5, 5) shouldBe 0xffc81e1e
  }

  it should "give each frame an image of its own, so a later frame cannot change an earlier one" in {
    val host    = new FakeHost
    val harness = new Harness(_ => host, frames = LayerFrames.Image)
    harness.frame()
    harness.frame()
    val List(second, first) = host.images.get
    first should not be theSameInstanceAs(second)
    Bitmap.Companion.makeFromImage(first).getColor(5, 5) shouldBe 0xffc81e1e
  }

  "RasterThenJava2D" should "paint with the Skia raster surface when it can" in {
    val harness =
      new Harness(_ => throw new IllegalStateException("no GL context"), RasterThenJava2D(SkiaRasterSurfaceFactory))
    harness.frame()
    harness.shown.get should have size 1
    val surface = RasterThenJava2D(SkiaRasterSurfaceFactory).create(harness.rawRequest)
    surface match
      case _: SkiaRenderSurface => succeed
      case other                => fail(s"expected a Skia surface, got ${other.getClass.getName}")
  }

  it should "paint with Java2D when the raster surface cannot be made" in {
    val failing: FrameSurfaceFactory = _ => throw new IllegalStateException("no raster surface")
    val harness                      = new Harness(_ => new FakeHost)
    RasterThenJava2D(failing).create(harness.rawRequest) match
      case _: Java2DRenderSurface => succeed
      case other                  => fail(s"expected a Java2D surface, got ${other.getClass.getName}")
  }

  it should "keep its text caches in the owner's backend slot" in {
    val harness = new Harness(_ => new FakeHost)
    harness.frame()
    SkiaTextCaches.heldBy(harness.caches.backendCaches) shouldBe true
  }
