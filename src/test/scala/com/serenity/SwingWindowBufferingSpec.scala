package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Component, GraphicsEnvironment, Image}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import javax.swing.{JPanel, RepaintManager, SwingUtilities}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.testkit.DisplayExclusive
import com.serenity.ui.terminal.SwingWindow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Swing paints a component through the nearest double-buffered ancestor's `RepaintManager` back buffer, so the canvas
  * only presents straight to the screen when none of its ancestors is buffered either.
  */
class SwingWindowBufferingSpec extends AnyFlatSpec with Matchers:

  /** The current `RepaintManager` is JVM-wide, so while it is installed any other spec that paints a component, and any
    * part of the window beside the canvas, asks it for buffers too. Only a request for the canvas or one of its
    * ancestors says how the canvas was presented, so only those are counted.
    */
  final private class CountingRepaintManager(presents: Component => Boolean) extends RepaintManager:
    val buffersRequested = new AtomicInteger(0)

    def noteRequest(c: Component): Unit =
      if presents(c) then buffersRequested.incrementAndGet(): Unit

    override def getOffscreenBuffer(c: Component, proposedWidth: Int, proposedHeight: Int): Image =
      noteRequest(c)
      super.getOffscreenBuffer(c, proposedWidth, proposedHeight)

    override def getVolatileOffscreenBuffer(c: Component, proposedWidth: Int, proposedHeight: Int): Image =
      noteRequest(c)
      super.getVolatileOffscreenBuffer(c, proposedWidth, proposedHeight)

  private def onEventThread(action: => Unit): Unit = SwingUtilities.invokeAndWait(() => action)

  "A shown SwingWindow" should "present frames without Swing requesting a back buffer" in {
    assume(!GraphicsEnvironment.isHeadless, "Painting through RepaintManager needs a display")

    DisplayExclusive {
      val canvasUnderTest = new AtomicReference[Option[Component]](None)
      val counting = new CountingRepaintManager(c => canvasUnderTest.get.exists(SwingUtilities.isDescendingFrom(_, c)))
      val previous = RepaintManager.currentManager(new JPanel)
      RepaintManager.setCurrentManager(counting)
      try
        val _ = SwingWindow
          .resource()
          .use { window =>
            IO.blocking {
              val canvas = window.canvas
              canvasUnderTest.set(Some(canvas))
              onEventThread(())
              canvas.isShowing shouldBe true

              val frame =
                window.acquireBaseImage(canvas.getWidth.max(1), canvas.getHeight.max(1), BufferedImage.TYPE_INT_RGB)
              counting.buffersRequested.set(0)
              onEventThread {
                window.onBaseImageReady(frame)
                canvas.paintImmediately(0, 0, canvas.getWidth, canvas.getHeight)
                canvas.paintImmediately(0, 0, 4, 4)
              }
              onEventThread(())
              onEventThread(())

              counting.buffersRequested.get() shouldBe 0
            }
          }
          .unsafeRunSync()
      finally RepaintManager.setCurrentManager(previous)
    }
  }

  it should "not count buffers requested for components outside the canvas's hierarchy" in {
    val canvas   = new JPanel
    val sibling  = new JPanel
    val counting = new CountingRepaintManager(SwingUtilities.isDescendingFrom(canvas, _))

    val _ = counting.noteRequest(sibling)
    counting.buffersRequested.get() shouldBe 0

    val _ = counting.noteRequest(canvas)
    counting.buffersRequested.get() shouldBe 1
  }
