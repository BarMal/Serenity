package com.serenity.ui.skia

import java.awt.image.BufferedImage
import java.awt.{Dimension, GraphicsEnvironment}
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO
import javax.swing.SwingUtilities

import com.serenity.config.WindowChromeMode
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.testkit.DisplayExclusive
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FontSpec, FrameSurfaceFactory, RendererEntryPoints}
import com.serenity.ui.terminal.SwingWindow
import com.serenity.{FrameBackend, Java2DFrames, PersistedFrameFixture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The raster factory behind a real `SwingWindow` on a display (Xvfb in CI): frames go through the window's pooled
  * images and its canvas paints them. Needs a display, so it is cancelled on a headless runner.
  */
class SkiaRasterWindowSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  override protected def frames: FrameBackend = Java2DFrames

  private val evidence  = Path.of("target", "test-reports", "skia-raster-window")
  private val uiMetrics = CellMetrics.fromFont(uiFont)

  private def settle(): Unit =
    SwingUtilities.invokeAndWait(() => ())
    SwingUtilities.invokeAndWait(() => ())

  private def withWindow[A](body: SwingWindow => A): A =
    assume(!GraphicsEnvironment.isHeadless, "Presenting frames needs a real window, which needs a display")
    DisplayExclusive:
      val window = new SwingWindow(
        new Dimension(logicalW, logicalH),
        cellMetrics,
        WindowChromeMode.Custom,
        cellMetrics,
        env = Map("XDG_SESSION_TYPE" -> "x11")
      )
      try
        window.start()
        settle()
        body(window)
      finally
        window.stop()
        settle()

  private def present(
    window: SwingWindow,
    state: AppState,
    damage: Damage,
    caches: RenderCaches,
    surfaces: FrameSurfaceFactory
  ): BufferedImage =
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      window,
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(textFont),
      FontSpec.fromAwt(uiFont),
      uiMetrics,
      None,
      repaintOnFlush = true,
      damage,
      caches,
      surfaces
    )
    settle()
    shown(window)

  private def shown(window: SwingWindow): BufferedImage =
    val canvas = window.canvas
    val image  = new BufferedImage(canvas.getWidth.max(1), canvas.getHeight.max(1), BufferedImage.TYPE_INT_RGB)
    SwingUtilities.invokeAndWait { () =>
      val g = image.createGraphics()
      try canvas.paint(g)
      finally g.dispose()
    }
    image

  private def keep(name: String, image: BufferedImage): Unit =
    Files.createDirectories(evidence)
    val _ = ImageIO.write(image, "png", evidence.resolve(s"$name.png").toFile)

  private def pixels(image: BufferedImage): Array[Int] =
    image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth).map(_ & 0xffffff)

  /** A gutter boundary that falls between device pixels is blended by coverage, and where a repainted row meets a kept
    * one the two frames can blend the one pixel they share differently (the rule `ScrollReusePixelsSpec` applies to the
    * same artefact, seen on macOS there and on a Windows runner here). Only that pixel kind is forgiven: at most
    * [[MostEdgePixelsDiffering]] of them, each within [[EdgeChannelTolerance]] per channel of the other frame's and, in
    * both frames, between the colours of the pixel to its left and right on every channel.
    */
  private val EdgeChannelTolerance    = 0x18
  private val MostEdgePixelsDiffering = 2

  private def differingPixels(a: BufferedImage, b: BufferedImage): Seq[(Int, Int)] =
    val (left, right) = (pixels(a), pixels(b))
    for
      y <- 0 until a.getHeight
      x <- 0 until a.getWidth
      if left(y * a.getWidth + x) != right(y * b.getWidth + x)
    yield (x, y)

  private def isEdgeCoverage(a: BufferedImage, b: BufferedImage, x: Int, y: Int): Boolean =
    def channels(image: BufferedImage, px: Int): Seq[Int] =
      val rgb = image.getRGB(px, y)
      Seq((rgb >> 16) & 0xff, (rgb >> 8) & 0xff, rgb & 0xff)
    def between(image: BufferedImage): Boolean =
      channels(image, x - 1).zip(channels(image, x)).zip(channels(image, x + 1)).forall {
        case ((left, here), right) => here >= math.min(left, right) && here <= math.max(left, right)
      }
    x > 0 && x < a.getWidth - 1 &&
    channels(a, x).zip(channels(b, x)).forall((first, second) => math.abs(first - second) <= EdgeChannelTolerance) &&
    between(a) && between(b)

  /** Whether each cell-sized block holds anything but the colour that fills most of the frame. */
  private def inkMap(image: BufferedImage): Vector[Boolean] =
    val all        = pixels(image)
    val background = all.groupBy(identity).maxBy(_._2.length)._1
    val blockW     = cellMetrics.charWidth
    val blockH     = cellMetrics.lineHeight
    val columns    = image.getWidth / blockW
    val rows       = image.getHeight / blockH
    Vector.tabulate(rows * columns) { index =>
      val left = index % columns * blockW
      val top  = index / columns * blockH
      (for
        y <- top until top + blockH
        x <- left until left + blockW
      yield all(y * image.getWidth + x)).exists(_ != background)
    }

  "A window presenting through the raster factory" should "show the same cells as Java2D for prose and for code" in
    withWindow { window =>
      List("prose" -> proseLines, "code" -> codeLines).foreach {
        case (name, lines) =>
          val state  = stateOf(lines.mkString("\n"), CursorPosition(2, 3))
          val java2d = present(window, state, Damage.Everything, RenderCaches.create(), FrameSurfaceFactory.Java2D)
          val skia   = present(window, state, Damage.Everything, RenderCaches.create(), SkiaRasterSurfaceFactory)
          keep(s"$name-java2d", java2d)
          keep(s"$name-skia-raster", skia)

          val (left, right) = (inkMap(java2d), inkMap(skia))
          left.length shouldBe right.length
          withClue(s"$name: share of cells where Java2D and Skia agree on ink: ") {
            left.zip(right).count(_ == _).toDouble / left.length should be >= 0.95
          }
          left.count(identity) should be > 20
          right.count(identity) should be > 20
      }
    }

  it should "present a typing and undo sequence exactly as a window drawing each frame from scratch does" in
    withWindow { incremental =>
      withWindow { scratch =>
        val first   = stateOf(proseLines.mkString("\n"), CursorPosition(4, 5))
        val second  = typedAt(first, proseLines, line = 4, text = "x")
        val third   = typedAt(second, proseLines, line = 20, text = "yz")
        val caches  = RenderCaches.create()
        val factory = SkiaRasterSurfaceFactory

        val steps = List(
          "open"   -> (first, Damage.Everything),
          "type"   -> (second, DamageProducer.forTransition(first, second)),
          "type-2" -> (third, DamageProducer.forTransition(second, third)),
          "undo"   -> (second, DamageProducer.forTransition(third, second)),
          "undo-2" -> (first, DamageProducer.forTransition(second, first))
        )
        steps.foreach {
          case (name, (state, damage)) =>
            val shownIncrementally = present(incremental, state, damage, caches, factory)
            val drawnFromScratch   = present(scratch, state, Damage.Everything, RenderCaches.create(), factory)
            keep(s"sequence-$name", shownIncrementally)
            val (edges, others) = differingPixels(shownIncrementally, drawnFromScratch)
              .partition((x, y) => isEdgeCoverage(shownIncrementally, drawnFromScratch, x, y))
            withClue(s"after $name, pixels differing beyond a boundary's antialiasing: ")(others shouldBe empty)
            withClue(s"after $name, boundary pixels differing in antialiasing: ")(
              edges.size should be <= MostEdgePixelsDiffering
            )
        }
        SkiaRasterSurfaceFactory.lastCopiedPixels(caches.backendCaches).getOrElse(0L) should be < (
          incremental.canvas.getWidth.toLong * incremental.canvas.getHeight
        )
      }
    }
