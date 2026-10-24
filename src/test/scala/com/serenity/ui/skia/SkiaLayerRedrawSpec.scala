package com.serenity.ui.skia

import java.awt.image.BufferedImage
import java.awt.{Color, Dimension, Font, GraphicsEnvironment, Rectangle}
import java.util.concurrent.atomic.AtomicReference
import javax.swing.{JFrame, JPanel, SwingUtilities}

import scala.annotation.tailrec

import com.serenity.state.manager.RenderCaches
import com.serenity.testkit.DisplayExclusive
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.FrameSurfaceRequest
import com.serenity.ui.terminal.{LayerPresenter, SwingWindow}
import org.jetbrains.skiko.SkiaLayer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the Skia layer costs per frame in a window: how many layers there are, how often they draw, and whether every
  * picture is closed. Counted, not timed.
  *
  * The render benchmark draws its cold frames with caches of their own. Each used to get a layer of its own on the same
  * canvas, and every publish repainted all of them (a heavyweight child's `repaint` goes through the parent, which
  * paints every sibling), so memory grew with every cold frame and each frame waited behind the others' redraws.
  */
class SkiaLayerRedrawSpec extends AnyFlatSpec with Matchers:

  private val width  = 400
  private val height = 300
  private val frames = 20
  private val red    = RenderColor.fromRgba(200, 30, 30)

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

  /** True once `value` has not changed for `quietMillis`. */
  private def settled(value: => Int, quietMillis: Long = 500): Boolean =
    @tailrec
    def loop(last: Int, since: Long, deadline: Long): Boolean =
      Thread.sleep(25)
      val now = value
      val at  = System.nanoTime()
      if now != last then loop(now, at, deadline)
      else if (at - since) / 1_000_000L >= quietMillis then true
      else at < deadline && loop(last, since, deadline)
    loop(value, System.nanoTime(), System.nanoTime() + 20_000_000_000L)

  final private class Session(val canvas: JPanel):
    val presenters      = new AtomicReference(List.empty[LayerPresenter])
    private val factory = SkiaLayerSurfaceFactory

    def host: SkiaLayerHost =
      presenters.get().collectFirst { case host: SkiaLayerHost => host }.getOrElse(fail("no layer was attached"))

    def layers: Int = onEdt(canvas.getComponents.count(_.isInstanceOf[SkiaLayer]))

    /** A frame with caches of its own, as a cold frame has. */
    def coldFrame(): Unit =
      val caches = RenderCaches.create()
      val request = FrameSurfaceRequest(
        CellMetrics(8, 16, 12),
        new Font(Font.MONOSPACED, Font.PLAIN, 12),
        canvas,
        _ => (),
        (w, h, t) => new BufferedImage(w, h, t),
        caches.surfaceText,
        caches.backendCaches,
        () => None,
        attachLayer = presenter =>
          val _ = presenters.updateAndGet(_ :+ presenter)
      )
      val surface = factory.create(request)
      surface.pixels.fillPixelRect(0, 0, 40, 40, red)
      surface.flush()

  private def withSession[A](body: Session => A): A =
    assume(!GraphicsEnvironment.isHeadless, "A hosted layer needs a display")
    DisplayExclusive:
      val canvas = new JPanel(null)
      canvas.setPreferredSize(new Dimension(width, height))
      val frame = onEdt {
        val f = new JFrame("layer redraw")
        f.setContentPane(canvas)
        f.pack()
        f.setLocation(0, 0)
        f.setVisible(true)
        f
      }
      val session = new Session(canvas)
      try body(session)
      finally
        session.presenters.get().foreach(_.dispose())
        onEdt(frame.dispose())

  "Frames drawn with caches of their own" should "share the window's one layer" in
    withSession { session =>
      (1 to frames).foreach(_ => session.coldFrame())
      session.presenters.get() should have size 1
      session.layers shouldBe 1
    }

  it should "draw about once each, not once for every layer a window ever had" in
    withSession { session =>
      (1 to frames).foreach(_ => session.coldFrame())
      settled(session.host.renderCount) shouldBe true
      session.host.publishCount shouldBe frames
      session.host.renderCount should be <= (frames + 2)
    }

  it should "close every picture but the one on show, and that one when the layer goes" in
    withSession { session =>
      (1 to frames).foreach(_ => session.coldFrame())
      settled(session.host.renderCount) shouldBe true
      session.host.closedPictures shouldBe (frames - 1)
      val drawn = session.host.renderCount
      session.host.dispose()
      session.host.closedPictures shouldBe frames
      Thread.sleep(300)
      session.host.renderCount shouldBe drawn
    }

  "Caret blinks" should "draw once each and publish no picture" in
    withSession { session =>
      session.coldFrame()
      settled(session.host.renderCount) shouldBe true
      val caret   = SwingWindow.CaretPaint(new Rectangle(30, 30, 4, 16), Color.WHITE)
      val before  = session.host.renderCount
      val painted = session.host.publishCount
      val blinks  = 10
      (1 to blinks).foreach(i => session.host.showCarets(if i % 2 == 0 then Nil else List(caret)))
      settled(session.host.renderCount) shouldBe true
      session.host.publishCount shouldBe painted
      session.host.renderCount should be <= (before + blinks)
    }
