package com.serenity.ui.terminal

import java.awt.{Color, Cursor, Dimension, GraphicsEnvironment, Rectangle}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import javax.swing.SwingUtilities

import com.serenity.config.WindowChromeMode
import com.serenity.input.ImeRequestsHost
import com.serenity.testkit.DisplayExclusive
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SwingWindowLayerPresenterSpec extends AnyFlatSpec with Matchers:

  final private class FakePresenter(hasBase: Boolean) extends LayerPresenter:
    val carets                                            = new AtomicReference(List.empty[SwingWindow.CaretPaint])
    val cursors                                           = new AtomicReference(List.empty[Cursor])
    val disposed                                          = new AtomicInteger(0)
    def baseAvailable: Boolean                            = hasBase
    def showCarets(c: List[SwingWindow.CaretPaint]): Unit = carets.set(c)
    def setCursor(cursor: Cursor): Unit =
      val _ = cursors.updateAndGet(cursor :: _)
    def dispose(): Unit =
      val _ = disposed.incrementAndGet()
    val rendered                               = new AtomicReference[Option[() => Unit]](None)
    def onRendered(callback: () => Unit): Unit = rendered.set(Some(callback))
    def renderApi: String                      = "FAKE"

  private def withWindow[A](body: SwingWindow => A): A =
    assume(!GraphicsEnvironment.isHeadless, "A window needs a display")
    DisplayExclusive:
      val window = new SwingWindow(
        new Dimension(640, 480),
        CellMetrics(8, 16, 13),
        WindowChromeMode.Custom,
        CellMetrics(8, 16, 13),
        env = Map("XDG_SESSION_TYPE" -> "x11")
      )
      try
        window.start()
        SwingUtilities.invokeAndWait(() => ())
        body(window)
      finally
        window.stop()
        SwingUtilities.invokeAndWait(() => ())

  private val caret = SwingWindow.CaretPaint(new Rectangle(10, 20, 2, 16), Color.WHITE)

  "SwingWindow with a layer presenter" should "decline the caret overlay until the presenter has a base frame" in
    withWindow { window =>
      val presenter = new FakePresenter(hasBase = false)
      window.attachLayerPresenter(presenter)
      window.onCursorOverlayReady(None)(List(caret)) shouldBe false
      presenter.carets.get shouldBe Nil
    }

  it should "hand the carets to the presenter once it has a base, and keep the caret bounds for the input method" in
    withWindow { window =>
      val presenter = new FakePresenter(hasBase = true)
      window.attachLayerPresenter(presenter)
      window.onCursorOverlayReady(None)(List(caret)) shouldBe true
      presenter.carets.get shouldBe List(caret)
      window.canvas.asInstanceOf[ImeRequestsHost].caretBounds shouldBe Some(caret.rect)
    }

  it should "dispose a presenter that another replaces, and leave the same one attached alone" in
    withWindow { window =>
      val first  = new FakePresenter(hasBase = true)
      val second = new FakePresenter(hasBase = true)
      window.attachLayerPresenter(first)
      window.attachLayerPresenter(first)
      first.disposed.get shouldBe 0
      window.attachLayerPresenter(second)
      first.disposed.get shouldBe 1
      second.disposed.get shouldBe 0
      window.layerPresenter.exists(_ eq second) shouldBe true
    }

  it should "mirror the pointer cursor onto the presenter" in
    withWindow { window =>
      val presenter = new FakePresenter(hasBase = true)
      window.attachLayerPresenter(presenter)
      window.updatePointerShape(com.serenity.state.models.PointerShape.Text)
      SwingUtilities.invokeAndWait(() => ())
      presenter.cursors.get.map(_.getType) should contain(Cursor.TEXT_CURSOR)
    }

  it should "dispose the presenter when the window stops" in {
    val presenter = new FakePresenter(hasBase = true)
    withWindow(window => window.attachLayerPresenter(presenter))
    presenter.disposed.get shouldBe 1
  }

  it should "give the presenter a way to tell the window a frame was drawn" in
    withWindow { window =>
      val presenter = new FakePresenter(hasBase = true)
      val presented = new AtomicInteger(0)
      window.setOnFramePresented(() =>
        val _ = presented.incrementAndGet()
      )
      window.attachLayerPresenter(presenter)
      SwingUtilities.invokeAndWait(() => ())
      // The window's own paints also report a frame, so only a rise can be asked of the presenter's callback.
      val before = presented.get
      presenter.rendered.get().fold(fail("the presenter was given no callback"))(_.apply())
      presented.get should be > before
    }

  it should "expose the attached presenter" in
    withWindow { window =>
      window.layerPresenter shouldBe None
      val presenter = new FakePresenter(hasBase = true)
      window.attachLayerPresenter(presenter)
      window.layerPresenter.exists(_ eq presenter) shouldBe true
    }

  "SwingWindow without a presenter" should "decline the caret overlay until a base image is published, as before" in
    withWindow(window => window.onCursorOverlayReady(None)(List(caret)) shouldBe false)

  it should "tell a listener each time the canvas paints" in
    withWindow { window =>
      val presented = new AtomicInteger(0)
      window.setOnFramePresented(() =>
        val _ = presented.incrementAndGet()
      )
      SwingUtilities.invokeAndWait(() => window.canvas.paintImmediately(window.canvas.getBounds))
      presented.get should be >= 1
    }
