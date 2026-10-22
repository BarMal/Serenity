package com.serenity.ui.skia

import java.awt.event.{InputEvent, KeyAdapter, KeyEvent, MouseAdapter, MouseEvent}
import java.awt.image.BufferedImage
import java.awt.{Color, Dimension, GraphicsEnvironment, Point, Rectangle, Robot}
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

import javax.swing.{JFrame, JPanel, SwingUtilities}

import com.serenity.SkiaPixels
import com.serenity.testkit.DisplayExclusive
import com.serenity.ui.terminal.SwingWindow
import org.jetbrains.skia.{Paint, Picture, PictureRecorder, Rect, Surface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Skia layer hosted in a window canvas, on a display (Xvfb in CI). Skiko falls back to its software redrawers
  * where there is no GL context, so what is exercised is the hosting, the replay and the input routing; no GPU API is.
  */
class SkiaLayerHostSpec extends AnyFlatSpec with Matchers:

  private val width  = 400
  private val height = 300

  private def onEdt[A](body: => A): A =
    val result = new AtomicReference[Option[A]](None)
    SwingUtilities.invokeAndWait(() => result.set(Some(body)))
    result.get().getOrElse(fail("the event thread produced nothing"))

  @tailrec
  private def awaited(condition: => Boolean, deadlineNanos: Long): Boolean =
    if condition then true
    else if System.nanoTime() >= deadlineNanos then false
    else
      Thread.sleep(25)
      awaited(condition, deadlineNanos)

  private def eventually(what: String)(condition: => Boolean): Unit =
    withClue(s"timed out waiting for $what: ")(awaited(condition, System.nanoTime() + 15_000_000_000L) shouldBe true)

  final private class Window(val frame: JFrame, val canvas: JPanel, val host: SkiaLayerHost):
    def screenOrigin: Point = onEdt(canvas.getLocationOnScreen)

    def capture(): BufferedImage =
      new Robot().createScreenCapture(new Rectangle(screenOrigin, new Dimension(canvas.getWidth, canvas.getHeight)))

  private def withWindow[A](body: Window => A): A =
    assume(!GraphicsEnvironment.isHeadless, "A hosted layer needs a display")
    assume(
      System.getProperty("os.name", "").toLowerCase.contains("linux"),
      "reads the screen back from a layer Skiko draws with its software redrawer; other systems use their GPU APIs and need a real display, which the owner checks"
    )
    DisplayExclusive:
      val canvas = new JPanel(null)
      canvas.setFocusable(true)
      canvas.setPreferredSize(new Dimension(width, height))
      val frame = onEdt {
        val f = new JFrame("layer host")
        f.setContentPane(canvas)
        f.pack()
        f.setLocation(0, 0)
        f.setVisible(true)
        f
      }
      val host = SkiaLayerHost.create(canvas)
      try
        val _ = onEdt(canvas.requestFocusInWindow())
        body(new Window(frame, canvas, host))
      finally
        host.dispose()
        onEdt(frame.dispose())

  private def record(draw: org.jetbrains.skia.Canvas => Unit): Picture =
    val recorder = new PictureRecorder()
    draw(PictureRecording.begin(recorder, width, height))
    val picture = recorder.finishRecordingAsPicture()
    recorder.close()
    picture

  private def scene(canvas: org.jetbrains.skia.Canvas): Unit =
    canvas.clear(0xff203040)
    val ink = new Paint()
    ink.setColor(0xffff8000)
    canvas.drawRect(Rect.Companion.makeXYWH(20f, 20f, 100f, 50f), ink)
    ink.setColor(0xff30c060)
    canvas.drawRect(Rect.Companion.makeXYWH(200f, 120f, 80f, 90f), ink)

  private def rgb(image: BufferedImage, x: Int, y: Int): Int = image.getRGB(x, y) & 0xffffff

  "A hosted Skia layer" should "show a published picture exactly as the raster rasteriser draws it" in
    withWindow { window =>
      window.host.publish(record(scene))
      eventually("the first frame")(window.host.renderCount > 0)
      val expected = Surface.Companion.makeRasterN32Premul(width, height)
      scene(expected.getCanvas)
      val raster = SkiaPixels.read(expected)
      eventually("the picture on screen")(rgb(window.capture(), 300, 20) == 0x203040)
      val shown = window.capture()
      val differing = (for
        y <- 0 until height
        x <- 0 until width
        if (raster(y * width + x) & 0xffffff) != rgb(shown, x, y)
      yield (x, y)).take(5)
      withClue(s"first differing pixels (x,y): $differing; ")(differing shouldBe empty)
    }

  it should "draw the carets over the retained picture without recording the picture again" in
    withWindow { window =>
      window.host.publish(record(scene))
      eventually("the first frame")(window.host.renderCount > 0)
      val published = window.host.publishCount
      val caret     = SwingWindow.CaretPaint(new Rectangle(300, 40, 4, 20), Color.WHITE)
      List(List(caret), Nil, List(caret)).foreach { carets =>
        val before = window.host.renderCount
        window.host.showCarets(carets)
        eventually("a redraw")(window.host.renderCount > before)
      }
      window.host.publishCount shouldBe published
      eventually("the caret on screen")(rgb(window.capture(), 301, 50) == 0xffffff)
      rgb(window.capture(), 250, 160) shouldBe 0x30c060
    }

  it should "report a base frame only once a picture has been published" in
    withWindow { window =>
      window.host.baseAvailable shouldBe false
      window.host.publish(record(scene))
      window.host.baseAvailable shouldBe true
    }

  it should "pass mouse and key input to the canvas, which keeps the focus" in
    withWindow { window =>
      window.host.publish(record(scene))
      eventually("the first frame")(window.host.renderCount > 0)
      val events = new AtomicReference(List.empty[String])
      def note(kind: String, e: MouseEvent): Unit =
        val _ = events.updateAndGet(s"$kind@${e.getX},${e.getY}" :: _)
      onEdt {
        window.canvas.addMouseListener(new MouseAdapter:
          override def mousePressed(e: MouseEvent): Unit  = note("press", e)
          override def mouseReleased(e: MouseEvent): Unit = note("release", e))
        window.canvas.addMouseMotionListener(new MouseAdapter:
          override def mouseMoved(e: MouseEvent): Unit   = note("move", e)
          override def mouseDragged(e: MouseEvent): Unit = note("drag", e))
        window.canvas.addKeyListener(
          new KeyAdapter:
            override def keyPressed(e: KeyEvent): Unit =
              val _ = events.updateAndGet(s"key${e.getKeyCode}" :: _)
        )
      }
      val origin = window.screenOrigin
      val robot  = new Robot()
      robot.setAutoDelay(60)
      robot.mouseMove(origin.x + 30, origin.y + 40)
      robot.mouseMove(origin.x + 60, origin.y + 80)
      robot.mousePress(InputEvent.BUTTON1_DOWN_MASK)
      robot.mouseMove(origin.x + 90, origin.y + 100)
      robot.mouseRelease(InputEvent.BUTTON1_DOWN_MASK)
      robot.keyPress(KeyEvent.VK_A)
      robot.keyRelease(KeyEvent.VK_A)
      eventually("the input")(events.get.exists(_.startsWith("key")) && events.get.exists(_.startsWith("release")))
      val seen = events.get.reverse
      withClue(s"events seen by the canvas: $seen; ") {
        seen.exists(_.startsWith("move@")) shouldBe true
        seen should contain("press@60,80")
        seen.exists(_.startsWith("drag@")) shouldBe true
        seen should contain("release@90,100")
        seen should contain(s"key${KeyEvent.VK_A}")
      }
      onEdt(java.awt.KeyboardFocusManager.getCurrentKeyboardFocusManager.getFocusOwner) shouldBe window.canvas
    }

  it should "follow the canvas when it is resized" in
    withWindow { window =>
      window.host.layerBounds shouldBe new Rectangle(0, 0, width, height)
      onEdt {
        window.canvas.setPreferredSize(new Dimension(500, 380))
        window.frame.pack()
      }
      eventually("the layer to follow")(window.host.layerBounds == new Rectangle(0, 0, 500, 380))
    }

  it should "remove its layer and close its pictures when disposed, and let a second host be made" in
    withWindow { window =>
      window.host.publish(record(scene))
      window.host.publish(record(scene))
      eventually("a frame")(window.host.renderCount > 0)
      window.host.dispose()
      onEdt(window.canvas.getComponentCount) shouldBe 0
      window.host.closedPictures should be >= 1
      val second = SkiaLayerHost.create(window.canvas)
      try
        second.publish(record(scene))
        eventually("the second host's frame")(second.renderCount > 0)
      finally second.dispose()
    }

  it should "show a pointer cursor set on the host on its layer" in
    withWindow { window =>
      window.host.setCursor(java.awt.Cursor.getPredefinedCursor(java.awt.Cursor.HAND_CURSOR))
      window.host.layerCursorType shouldBe java.awt.Cursor.HAND_CURSOR
    }
