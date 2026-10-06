package com.serenity.perf

import java.awt.image.BufferedImage

import com.serenity.ui.renderer.Java2DRenderSurface.DeviceScale
import com.serenity.ui.terminal.SwingWindow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The present benchmarks once re-published one unchanged image, which Java2D caches as an X pixmap after its first
  * draw: each iteration then only queued X requests, timing them bimodally (0.02 vs 0.4 ms) and never the upload a real
  * frame costs. These pin the frames those benchmarks present to what the renderer hands the window.
  */
class PresentedFramesSpec extends AnyFlatSpec with Matchers:

  private def presentedThroughPool(count: Int, scale: DeviceScale): List[BufferedImage] =
    val pool   = new SwingWindow.ReusableImagePool
    val frames = PresentedFrames(pool.acquire, logicalWidthPx = 1500, logicalHeightPx = 970, scale)
    List.fill(count) {
      val frame = frames.next()
      pool.publish(frame)
      frame
    }

  "a presented frame" should "be an opaque image at the window's device size, as Java2DRenderSurface.forFrame draws" in {
    val frames = presentedThroughPool(1, DeviceScale(2.0, 2.0))
    frames.map(frame => (frame.getWidth, frame.getHeight, frame.getType)) shouldBe
      List((3000, 1940, BufferedImage.TYPE_INT_RGB))
  }

  it should "come from the window's two-image pool, as the renderer's frames do, rather than a fresh allocation" in {
    val frames                             = presentedThroughPool(4, DeviceScale(1.0, 1.0))
    val List(first, second, third, fourth) = frames: @unchecked
    (first eq second) shouldBe false
    (third eq first) shouldBe true
    (fourth eq second) shouldBe true
  }

  it should "differ from what the same image held when it was last presented, so Java2D re-uploads it" in {
    val pool        = new SwingWindow.ReusableImagePool
    val frames      = PresentedFrames(pool.acquire, logicalWidthPx = 1500, logicalHeightPx = 970, DeviceScale(1.0, 1.0))
    val first       = frames.next()
    val firstPixels = first.getRGB(0, 0, first.getWidth, 1, null, 0, first.getWidth).toVector
    pool.publish(first)
    pool.publish(frames.next())
    val reused = frames.next()
    (reused eq first) shouldBe true
    reused.getRGB(0, 0, reused.getWidth, 1, null, 0, reused.getWidth).toVector should not be firstPixels
  }
