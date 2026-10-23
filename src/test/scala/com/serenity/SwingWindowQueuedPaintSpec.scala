package com.serenity

import java.awt.Rectangle
import javax.swing.JPanel

import scala.collection.mutable.ListBuffer

import com.serenity.ui.terminal.SwingWindow
import com.serenity.ui.terminal.SwingWindow.{CanvasRepaint, PaintRequest}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** WLToolkit never commits what `paintImmediately` draws, so under it a published frame has to reach the screen through
  * `repaint(rect)` and the `RepaintManager`; every other toolkit keeps painting each rect at once.
  */
class SwingWindowQueuedPaintSpec extends AnyFlatSpec with Matchers:

  private val caret = new Rectangle(10, 20, 2, 18)
  private val line  = new Rectangle(0, 40, 300, 18)

  private val NativeWayland = "sun.awt.wl.WLToolkit"
  private val OtherToolkits =
    scala.List("sun.awt.X11.XToolkit", "sun.lwawt.macosx.LWCToolkit", "sun.awt.windows.WToolkit")

  "SwingWindow.paintRequests" should "queue every bounded rect on the native Wayland toolkit" in {
    SwingWindow.paintRequests(NativeWayland, CanvasRepaint.Rects(scala.List(caret, line))) shouldBe
      scala.List(PaintRequest.Queued(caret), PaintRequest.Queued(line))
  }

  it should "paint every bounded rect at once on any other toolkit" in
    OtherToolkits.foreach { toolkit =>
      withClue(toolkit)(
        SwingWindow.paintRequests(toolkit, CanvasRepaint.Rects(scala.List(caret, line))) shouldBe
          scala.List(PaintRequest.Immediate(caret), PaintRequest.Immediate(line))
      )
    }

  it should "repaint the whole canvas for a whole-canvas repaint on every toolkit" in
    (NativeWayland :: OtherToolkits).foreach { toolkit =>
      withClue(toolkit)(
        SwingWindow.paintRequests(toolkit, CanvasRepaint.Whole) shouldBe scala.List(PaintRequest.WholeCanvas)
      )
    }

  it should "request nothing for a repaint with no rects" in
    (NativeWayland :: OtherToolkits).foreach { toolkit =>
      withClue(toolkit)(SwingWindow.paintRequests(toolkit, CanvasRepaint.Rects(Nil)) shouldBe Nil)
    }

  "SwingWindow.applyPaintRequests" should "queue a Queued rect with repaint and paint nothing at once" in {
    val canvas = recordingCanvas()
    SwingWindow.applyPaintRequests(canvas, scala.List(PaintRequest.Queued(caret), PaintRequest.Queued(line)))

    canvas.calls.toList shouldBe scala.List(Call.Repaint(caret), Call.Repaint(line))
  }

  it should "paint an Immediate rect at once and queue nothing" in {
    val canvas = recordingCanvas()
    SwingWindow.applyPaintRequests(canvas, scala.List(PaintRequest.Immediate(caret), PaintRequest.Immediate(line)))

    canvas.calls.toList shouldBe scala.List(Call.PaintImmediately(caret), Call.PaintImmediately(line))
  }

  it should "repaint the whole canvas for a WholeCanvas request" in {
    val canvas = recordingCanvas()
    SwingWindow.applyPaintRequests(canvas, scala.List(PaintRequest.WholeCanvas))

    canvas.calls.toList shouldBe scala.List(Call.RepaintWhole)
  }

  private enum Call:
    case Repaint(rect: Rectangle)
    case PaintImmediately(rect: Rectangle)
    case RepaintWhole

  private class RecordingCanvas extends JPanel:
    val calls = ListBuffer.empty[Call]

    override def repaint(rect: Rectangle): Unit =
      calls += Call.Repaint(new Rectangle(rect))

    override def paintImmediately(rect: Rectangle): Unit =
      calls += Call.PaintImmediately(new Rectangle(rect))

    override def repaint(): Unit =
      calls += Call.RepaintWhole

  private def recordingCanvas(): RecordingCanvas = new RecordingCanvas
