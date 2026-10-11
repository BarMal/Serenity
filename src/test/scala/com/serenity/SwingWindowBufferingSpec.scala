package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Component, GraphicsEnvironment, Image}
import java.util.concurrent.atomic.AtomicInteger
import javax.swing.{JPanel, RepaintManager, SwingUtilities}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.ui.terminal.SwingWindow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Swing paints a component through the nearest double-buffered ancestor's `RepaintManager` back buffer, so the canvas
  * only presents straight to the screen when none of its ancestors is buffered either.
  */
class SwingWindowBufferingSpec extends AnyFlatSpec with Matchers:

  final private class CountingRepaintManager extends RepaintManager:
    val buffersRequested = new AtomicInteger(0)

    override def getOffscreenBuffer(c: Component, proposedWidth: Int, proposedHeight: Int): Image =
      val _ = buffersRequested.incrementAndGet()
      super.getOffscreenBuffer(c, proposedWidth, proposedHeight)

    override def getVolatileOffscreenBuffer(c: Component, proposedWidth: Int, proposedHeight: Int): Image =
      val _ = buffersRequested.incrementAndGet()
      super.getVolatileOffscreenBuffer(c, proposedWidth, proposedHeight)

  private def onEventThread(action: => Unit): Unit = SwingUtilities.invokeAndWait(() => action)

  "A shown SwingWindow" should "present frames without Swing requesting a back buffer" in {
    assume(!GraphicsEnvironment.isHeadless, "Painting through RepaintManager needs a display")

    val counting = new CountingRepaintManager
    val previous = RepaintManager.currentManager(new JPanel)
    RepaintManager.setCurrentManager(counting)
    try
      val _ = SwingWindow
        .resource()
        .use { window =>
          IO.blocking {
            val canvas = window.canvas
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
