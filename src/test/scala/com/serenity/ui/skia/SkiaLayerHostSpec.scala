package com.serenity.ui.skia

import java.awt.event.{InputEvent, KeyAdapter, KeyEvent, MouseAdapter, MouseEvent}
import java.awt.image.BufferedImage
import java.awt.{Color, Dimension, GraphicsEnvironment, Point, Rectangle, Robot, Toolkit}
import java.util.concurrent.atomic.AtomicReference
import javax.swing.{JFrame, JPanel, SwingUtilities}

import scala.annotation.tailrec

import com.serenity.SkiaPixels
import com.serenity.testkit.{DisplayExclusive, ScreenLocation}
import com.serenity.ui.terminal.SwingWindow
import org.jetbrains.skia.{Paint, Picture, PictureRecorder, Rect, Surface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Skia layer hosted in a window canvas, on a display: Xvfb on Linux, where Skiko falls back to its software
  * redrawer, and the Windows and macOS runners, where it uses Direct3D and Metal. What is exercised is the hosting, the
  * drawing of a picture and the input routing.
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

  /** macOS composites the window's edge over the first rows of the canvas: on the CI runner the top rows read as a
    * darker gradient (`000a1a` where the picture has `203040`) whatever the layer drew. So on macOS the outer band is
    * left out of the exact comparison; everywhere else every pixel is compared.
    */
  private val chromeBand: Int =
    if System.getProperty("os.name", "").toLowerCase.contains("mac") then 8 else 0

  /** Waits for the screen to show `expected` exactly, with the picture's first pixel being `marker`. The window is
    * looked for where the toolkit says it is and around the first pixel of the picture's colour, since a window system
    * may put it a pixel or two away (see [[ScreenLocation]]); the best of those alignments is judged, and it must have
    * no mismatches outside [[chromeBand]]. What is reported on a failure is the closest alignment's first differences
    * and where they lie.
    */
  private def showsExactly(window: Window, expected: Array[Int], marker: Int): Unit =
    val closest = new AtomicReference("nothing was captured")
    def attempt(): Boolean =
      val screen = new Robot().createScreenCapture(new Rectangle(Toolkit.getDefaultToolkit.getScreenSize))
      def onScreen(x: Int, y: Int): Int =
        if x < 0 || y < 0 || x >= screen.getWidth || y >= screen.getHeight then -1 else screen.getRGB(x, y) & 0xffffff
      def differing(corner: (Int, Int)) =
        for
          y <- chromeBand until height - chromeBand
          x <- chromeBand until width - chromeBand
          if onScreen(corner._1 + x, corner._2 + y) != (expected(y * width + x) & 0xffffff)
        yield (x, y)
      val reported = window.screenOrigin
      val found    = ScreenLocation.firstPixelOf(screen, marker)
      val (best, corner) = ScreenLocation.bestAlignment(
        ScreenLocation.candidateCorners((reported.x, reported.y), found, slack = 2)
      )(differing(_).size)
      val all = differing(corner)
      val first = all
        .take(5)
        .map((x, y) =>
          f"($x,$y) screen ${onScreen(corner._1 + x, corner._2 + y)}%06x expected ${expected(y * width + x) & 0xffffff}%06x"
        )
      val bounds =
        Option.when(all.nonEmpty)((all.map(_._1).min, all.map(_._2).min, all.map(_._1).max, all.map(_._2).max))
      closest.set(
        s"$best pixels differ at the best corner $corner (toolkit says $reported, first marker pixel $found), within (minX,minY,maxX,maxY) $bounds: $first"
      )
      best == 0
    val shown = awaited(attempt(), System.nanoTime() + 15_000_000_000L)
    withClue(s"the screen never showed exactly the picture; ${closest.get}; ")(shown shouldBe true)

  "A hosted Skia layer" should "show a published picture exactly as the raster rasteriser draws it" in
    withWindow { window =>
      window.host.publish(record(scene))
      eventually("the first frame")(window.host.renderCount > 0)
      val expected = Surface.Companion.makeRasterN32Premul(width, height)
      scene(expected.getCanvas)
      val raster = SkiaPixels.read(expected)
      showsExactly(window, raster, marker = 0x203040)
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

  it should "count its draws and publishes and time the wait from publish to draw" in
    withWindow { window =>
      onEdt(window.canvas.repaint())
      eventually("a draw before any picture")(window.host.renderCount > 0)
      window.host.drawStats.publishes shouldBe 0
      window.host.drawStats.publishToRenderMaxMs shouldBe 0.0
      window.host.publish(record(scene))
      eventually("the picture drawn and timed")(window.host.drawStats.publishToRenderMaxMs > 0.0)
      val stats = window.host.drawStats
      stats.publishes shouldBe 1
      stats.renders should be >= 1
      stats.publishToRenderMeanMs should be > 0.0
      stats.replayMeanMs should be >= 0.0
    }

  it should "say how many published frames its latest draw showed" in
    withWindow { window =>
      window.host.publishedFrames shouldBe 0
      window.host.publish(record(scene))
      eventually("the first publish shown")(window.host.shownFrames == 1)
      window.host.publish(record(scene))
      eventually("the second publish shown")(window.host.shownFrames == 2)
      val seen = window.host.renderCount
      window.host.showCarets(Nil)
      eventually("a redraw")(window.host.renderCount > seen)
      window.host.shownFrames shouldBe 2
      window.host.publishedFrames shouldBe 2
    }

  it should "report how large the pictures it was given were" in
    withWindow { window =>
      window.host.drawStats.pictureOpsMean shouldBe 0.0
      window.host.publish(record(scene))
      window.host.publish(record(scene))
      val stats = window.host.drawStats
      stats.pictureOpsMean should be > 0.0
      stats.pictureBytesMean should be > 0.0
    }

  it should "record the published picture as one op in the canvas it draws into" in
    withWindow { window =>
      val ink = new Paint()
      ink.setColor(0xffff8000)
      val busy = record(canvas =>
        (0 until 300).foreach(i => canvas.drawRect(Rect.Companion.makeXYWH(i % 20 * 5f, i / 20 * 5f, 4f, 4f), ink))
      )
      busy.getApproximateOpCount should be >= 300
      window.host.publish(busy)
      val recorder = new PictureRecorder()
      val target   = PictureRecording.begin(recorder, width, height)
      onEdt(window.host.renderInto(target))
      val drawn = recorder.finishRecordingAsPicture()
      try drawn.getApproximateOpCount should be < 20
      finally
        drawn.close()
        recorder.close()
    }

  it should "show the carets over the picture exactly as a full replay would" in
    withWindow { window =>
      window.host.publish(record(scene))
      eventually("the first frame")(window.host.renderCount > 0)
      val caret = SwingWindow.CaretPaint(new Rectangle(300, 40, 4, 20), Color.WHITE)
      window.host.showCarets(List(caret))
      val expected = Surface.Companion.makeRasterN32Premul(width, height)
      scene(expected.getCanvas)
      val ink = new Paint()
      ink.setColor(Color.WHITE.getRGB)
      expected.getCanvas.drawRect(Rect.Companion.makeXYWH(300f, 40f, 4f, 20f), ink)
      val raster = SkiaPixels.read(expected)
      showsExactly(window, raster, marker = 0x203040)
    }

  it should "keep showing the picture when the canvas is resized" in
    withWindow { window =>
      window.host.publish(record(scene))
      eventually("the first frame")(window.host.renderCount > 0)
      val drawn = window.host.renderCount
      onEdt {
        window.canvas.setPreferredSize(new Dimension(width + 100, height + 50))
        window.canvas.setSize(width + 100, height + 50)
        window.canvas.doLayout()
      }
      eventually("a frame at the new size")(window.host.renderCount > drawn)
      eventually("the picture still on screen")(rgb(window.capture(), 300, 20) == 0x203040)
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

  it should "tell its listener after each frame it draws" in
    withWindow { window =>
      val drawn = new java.util.concurrent.atomic.AtomicInteger(0)
      window.host.onRendered(() =>
        val _ = drawn.incrementAndGet()
      )
      window.host.publish(record(scene))
      eventually("a frame to be drawn")(drawn.get > 0)
      val before = drawn.get
      window.host.showCarets(Nil)
      eventually("the caret frame to be drawn")(drawn.get > before)
    }

  it should "ask its redraw once for each published picture and once for each change of carets" in
    withWindow { window =>
      val publishes = new java.util.concurrent.atomic.AtomicInteger(0)
      val changes   = new java.util.concurrent.atomic.AtomicInteger(0)
      val second = SkiaLayerHost.createWith(
        window.canvas,
        syncGpu = false,
        layer =>
          val real = LayerRedraw.repainting(layer)
          new LayerRedraw:
            def afterPublish(): Unit =
              val _ = publishes.incrementAndGet()
              real.afterPublish()
            def afterCaretsChanged(): Unit =
              val _ = changes.incrementAndGet()
              real.afterCaretsChanged()
      )
      try
        second.publish(record(scene))
        (publishes.get, changes.get) shouldBe (1, 0)
        second.showCarets(Nil)
        second.showCarets(Nil)
        (publishes.get, changes.get) shouldBe (1, 2)
      finally second.dispose()
    }

  it should "name the render API Skiko chose" in
    withWindow { window =>
      eventually("a frame")(window.host.renderCount >= 0)
      window.host.publish(record(scene))
      eventually("the first frame")(window.host.renderCount > 0)
      List("OPENGL", "SOFTWARE_FAST", "SOFTWARE_COMPAT", "DIRECT3D", "ANGLE", "METAL", "VULKAN") should contain(
        window.host.renderApi
      )
    }

  it should "draw frames when told to wait for the GPU after each, as the benchmark does" in {
    assume(!GraphicsEnvironment.isHeadless, "A hosted layer needs a display")
    DisplayExclusive:
      val canvas = new JPanel(null)
      canvas.setPreferredSize(new Dimension(width, height))
      val frame = onEdt {
        val f = new JFrame("synchronous layer")
        f.setContentPane(canvas)
        f.pack()
        f.setLocation(0, 0)
        f.setVisible(true)
        f
      }
      val host = SkiaLayerHost.create(canvas, syncGpu = true)
      try
        host.publish(record(scene))
        eventually("a synchronous frame")(host.renderCount > 0)
        host.drawStats.syncedDraws shouldBe 0
      finally
        host.dispose()
        onEdt(frame.dispose())
  }
