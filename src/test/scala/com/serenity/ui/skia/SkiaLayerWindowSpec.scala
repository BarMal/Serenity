package com.serenity.ui.skia

import java.awt.image.BufferedImage
import java.awt.{Dimension, GraphicsEnvironment, Rectangle, Robot}
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO
import scala.annotation.tailrec

import javax.swing.SwingUtilities

import com.serenity.config.WindowChromeMode
import com.serenity.input.ImeRequestsHost
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.testkit.DisplayExclusive
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FontSpec, FrameSurfaceFactory, RendererCursorOverlay, RendererEntryPoints}
import com.serenity.ui.terminal.SwingWindow
import com.serenity.{FrameBackend, Java2DFrames, PersistedFrameFixture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The GPU layer behind a real `SwingWindow` on a display (Xvfb in CI): frames and carets go through the renderer's
  * entry points. Skiko falls back to its software redrawers where there is no GL context, so no GPU API is exercised.
  * Cancelled without a display.
  */
class SkiaLayerWindowSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  override protected def frames: FrameBackend = Java2DFrames

  private val evidence  = Path.of("target", "test-reports", "skia-layer-window")
  private val uiMetrics = CellMetrics.fromFont(uiFont)

  private def settle(): Unit =
    SwingUtilities.invokeAndWait(() => ())
    SwingUtilities.invokeAndWait(() => ())

  private def withWindow[A](body: SwingWindow => A): A =
    assume(!GraphicsEnvironment.isHeadless, "Presenting frames needs a real window, which needs a display")
    assume(
      System.getProperty("os.name", "").toLowerCase.contains("linux"),
      "reads the screen back from a layer Skiko draws with its software redrawer; other systems use their GPU APIs and need a real display, which the owner checks"
    )
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

  private def render(window: SwingWindow, state: AppState, caches: RenderCaches, surfaces: FrameSurfaceFactory): Unit =
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
      Damage.Everything,
      caches,
      surfaces
    )
    settle()

  /** What is on the screen over the canvas: the layer's pixels, which a Swing paint of the canvas does not hold. */
  private def onScreen(window: SwingWindow): BufferedImage =
    val origin = new java.util.concurrent.atomic.AtomicReference[java.awt.Point]()
    SwingUtilities.invokeAndWait(() => origin.set(window.canvas.getLocationOnScreen))
    new Robot().createScreenCapture(new Rectangle(origin.get(), window.canvas.getSize))

  private def keep(name: String, image: BufferedImage): Unit =
    Files.createDirectories(evidence)
    val _ = ImageIO.write(image, "png", evidence.resolve(s"$name.png").toFile)

  private def canvasImage(window: SwingWindow): BufferedImage =
    val canvas = window.canvas
    val image  = new BufferedImage(canvas.getWidth.max(1), canvas.getHeight.max(1), BufferedImage.TYPE_INT_RGB)
    SwingUtilities.invokeAndWait { () =>
      val g = image.createGraphics()
      try canvas.paint(g)
      finally g.dispose()
    }
    image

  private def inkMap(image: BufferedImage): Vector[Boolean] =
    val all        = image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth).map(_ & 0xffffff)
    val background = all.groupBy(identity).maxBy(_._2.length)._1
    val (w, h)     = (cellMetrics.charWidth, cellMetrics.lineHeight)
    val columns    = image.getWidth / w
    val rows       = image.getHeight / h
    Vector.tabulate(rows * columns) { index =>
      val (left, top) = (index % columns * w, index / columns * h)
      (for
        y <- top until top + h
        x <- left until left + w
      yield all(y * image.getWidth + x)).exists(_ != background)
    }

  @tailrec
  private def awaited(condition: => Boolean, deadlineNanos: Long): Boolean =
    if condition then true
    else if System.nanoTime() >= deadlineNanos then false
    else
      Thread.sleep(25)
      awaited(condition, deadlineNanos)

  private def eventually(what: String)(condition: => Boolean): Unit =
    withClue(s"timed out waiting for $what: ")(awaited(condition, System.nanoTime() + 15_000_000_000L) shouldBe true)

  "A window presenting through the Skia layer" should "show the same cells as Java2D for prose and for code" in
    withWindow { window =>
      List("prose" -> proseLines, "code" -> codeLines).foreach {
        case (name, lines) =>
          val state = stateOf(lines.mkString("\n"), CursorPosition(2, 3))
          render(window, state, RenderCaches.create(), FrameSurfaceFactory.Java2D)
          val java2d = canvasImage(window)
          // The layer is added over the canvas by the first layer frame, so the Java2D frame is taken first.
          render(window, state, RenderCaches.create(), SkiaLayerSurfaceFactory.Default)
          val layerHost = window.canvas.getComponents.length
          layerHost should be >= 1
          eventually("the layer's frame on screen")(inkMap(onScreen(window)).count(identity) > 20)
          val skia = onScreen(window)
          keep(s"$name-java2d", java2d)
          keep(s"$name-skia-layer", skia)
          val (left, right) = (inkMap(java2d), inkMap(skia))
          left.length shouldBe right.length
          withClue(s"$name: share of cells where Java2D and the layer agree on ink: ") {
            left.zip(right).count(_ == _).toDouble / left.length should be >= 0.95
          }
      }
    }

  it should "draw the caret over the retained frame, and decline the caret-only path only until a frame exists" in
    withWindow { window =>
      val state  = stateOf(proseLines.mkString("\n"), CursorPosition(2, 3))
      val caches = RenderCaches.create()
      RendererCursorOverlay.renderCursorOnly(
        state,
        cursorVisible = true,
        window,
        FontSpec.fromAwt(codeFont),
        FontSpec.fromAwt(textFont),
        FontSpec.fromAwt(uiFont),
        uiMetrics,
        Some(RenderColor.fromRgba(255, 255, 255)),
        caches
      ) shouldBe false

      RendererCursorOverlay.renderWithCursorOverlay(
        state,
        window,
        FontSpec.fromAwt(codeFont),
        FontSpec.fromAwt(textFont),
        FontSpec.fromAwt(uiFont),
        uiMetrics,
        Some(RenderColor.fromRgba(255, 255, 255)),
        Damage.Everything,
        caches,
        SkiaLayerSurfaceFactory.Default
      )
      settle()
      val bounds = window.canvas.asInstanceOf[ImeRequestsHost].caretBounds
      withClue("the caret bounds the input method uses: ")(bounds should not be empty)
      val caret = bounds.getOrElse(new Rectangle(0, 0, 1, 1))
      eventually("the caret on screen") {
        val shown = onScreen(window)
        val x     = caret.x + caret.width / 2
        val y     = caret.y + caret.height / 2
        (shown.getRGB(x, y) & 0xffffff) == 0xffffff
      }
      keep("caret", onScreen(window))
    }
