package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D, Rectangle}

import com.serenity.ui.terminal.SwingWindow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How `SwingWindow.canvas` presents a published frame: only the repaint clip is copied out of the base image, and
  * carets are filled straight over it instead of compositing a full-window overlay image.
  */
class SwingWindowPresentSpec extends AnyFlatSpec with Matchers:

  "SwingWindow.imageSourceRegion" should "map a clip one-to-one when the image matches the panel" in {
    SwingWindow.imageSourceRegion(new Rectangle(10, 20, 30, 40), 200, 100, 200, 100) shouldBe
      Some(new Rectangle(10, 20, 30, 40))
  }

  it should "scale a clip into device pixels for a 2x image" in {
    SwingWindow.imageSourceRegion(new Rectangle(10, 20, 30, 40), 200, 100, 400, 200) shouldBe
      Some(new Rectangle(20, 40, 60, 80))
  }

  it should "round an odd clip outwards so every covered device pixel is copied" in {
    SwingWindow.imageSourceRegion(new Rectangle(3, 5, 7, 9), 100, 100, 150, 150) shouldBe
      Some(new Rectangle(4, 7, 11, 14))
  }

  it should "clamp a clip that extends past the panel to the image bounds" in {
    SwingWindow.imageSourceRegion(new Rectangle(-5, 90, 20, 30), 100, 100, 200, 200) shouldBe
      Some(new Rectangle(0, 180, 30, 20))
  }

  it should "report nothing to copy for an empty, off-image or degenerate clip" in {
    SwingWindow.imageSourceRegion(new Rectangle(10, 10, 0, 5), 100, 100, 100, 100) shouldBe None
    SwingWindow.imageSourceRegion(new Rectangle(150, 10, 10, 5), 100, 100, 100, 100) shouldBe None
    SwingWindow.imageSourceRegion(new Rectangle(0, 0, 10, 10), 0, 100, 100, 100) shouldBe None
  }

  "SwingWindow.paintPresentedFrame" should "copy only the clipped part of the base image" in {
    val base   = filled(20, 20, Color.RED)
    val target = filled(20, 20, Color.BLUE)
    paintInto(target, new Rectangle(5, 5, 4, 4))(g => SwingWindow.paintPresentedFrame(g, Some(base), Nil, 20, 20))

    new Color(target.getRGB(6, 6), true) shouldBe Color.RED
    new Color(target.getRGB(1, 1), true) shouldBe Color.BLUE
    new Color(target.getRGB(12, 12), true) shouldBe Color.BLUE
  }

  it should "map a 2x base image onto the panel's logical pixels" in {
    val base = new BufferedImage(40, 40, BufferedImage.TYPE_INT_ARGB)
    val bg   = base.createGraphics()
    try
      bg.setColor(Color.GREEN)
      bg.fillRect(20, 20, 20, 20)
    finally bg.dispose()
    val target = filled(20, 20, Color.BLUE)
    paintInto(target, new Rectangle(0, 0, 20, 20))(g => SwingWindow.paintPresentedFrame(g, Some(base), Nil, 20, 20))

    new Color(target.getRGB(15, 15), true) shouldBe Color.GREEN
    new Color(target.getRGB(5, 5), true) shouldBe Color.BLACK
  }

  it should "fill carets over the base image, blending translucent caret colours" in {
    val base   = filled(20, 20, Color.BLACK)
    val target = filled(20, 20, Color.BLUE)
    val carets = List(
      SwingWindow.CaretPaint(new Rectangle(2, 2, 2, 6), Color.WHITE),
      SwingWindow.CaretPaint(new Rectangle(10, 2, 2, 6), new Color(255, 255, 255, 128))
    )
    paintInto(target, new Rectangle(0, 0, 20, 20))(g => SwingWindow.paintPresentedFrame(g, Some(base), carets, 20, 20))

    new Color(target.getRGB(3, 4), true) shouldBe Color.WHITE
    val blended = new Color(target.getRGB(11, 4), true)
    blended.getRed should (be > 100 and be < 160)
    new Color(target.getRGB(6, 4), true) shouldBe Color.BLACK
  }

  it should "keep the background fill inside the clip" in {
    val target = filled(20, 20, Color.RED)
    paintInto(target, new Rectangle(0, 0, 5, 5))(g => SwingWindow.paintPresentedFrame(g, None, Nil, 20, 20))

    new Color(target.getRGB(2, 2), true) shouldBe Color.BLACK
    new Color(target.getRGB(10, 10), true) shouldBe Color.RED
  }

  it should "present a frame with a fully transparent background as opaque black" in {
    val target = filled(20, 20, Color.RED)
    val base   = filled(20, 20, new Color(0, 0, 0, 0))
    paintInto(target, new Rectangle(0, 0, 20, 20))(g => SwingWindow.paintPresentedFrame(g, Some(base), Nil, 20, 20))

    new Color(target.getRGB(10, 10), true) shouldBe Color.BLACK
  }

  "SwingWindow.ReusableImagePool" should "not hand back an image the EDT is still painting" in {
    val pool    = new SwingWindow.ReusableImagePool
    val initial = pool.acquire(width = 64, height = 48, imageType = BufferedImage.TYPE_INT_ARGB)
    pool.publish(initial)
    pool.leasePublished() shouldBe Some(initial)

    val next = pool.acquire(width = 64, height = 48, imageType = BufferedImage.TYPE_INT_ARGB)
    pool.publish(next)

    pool.acquire(width = 64, height = 48, imageType = BufferedImage.TYPE_INT_ARGB) should not be theSameInstanceAs(
      initial
    )
  }

  it should "wait for a paint in progress and reuse its image rather than allocate a third" in {
    val pool    = new SwingWindow.ReusableImagePool
    val initial = pool.acquire(width = 64, height = 48, imageType = BufferedImage.TYPE_INT_ARGB)
    pool.publish(initial)
    pool.leasePublished() shouldBe Some(initial)
    pool.publish(pool.acquire(width = 64, height = 48, imageType = BufferedImage.TYPE_INT_ARGB))

    val paint = new Thread(() =>
      Thread.sleep(30)
      pool.releaseLease()
    )
    paint.start()

    pool.acquire(width = 64, height = 48, imageType = BufferedImage.TYPE_INT_ARGB) should be theSameInstanceAs initial
    paint.join()
  }

  it should "not wait forever on a paint that never finishes" in {
    val pool    = new SwingWindow.ReusableImagePool
    val initial = pool.acquire(width = 64, height = 48, imageType = BufferedImage.TYPE_INT_ARGB)
    pool.publish(initial)
    val _ = pool.leasePublished()
    pool.publish(pool.acquire(width = 64, height = 48, imageType = BufferedImage.TYPE_INT_ARGB))

    pool.acquire(width = 64, height = 48, imageType = BufferedImage.TYPE_INT_ARGB) should not be theSameInstanceAs(
      initial
    )
  }

  it should "reuse a painted image again once the paint has finished" in {
    val pool    = new SwingWindow.ReusableImagePool
    val initial = pool.acquire(width = 64, height = 48, imageType = BufferedImage.TYPE_INT_ARGB)
    pool.publish(initial)
    pool.leasePublished() shouldBe Some(initial)
    pool.releaseLease()

    val next = pool.acquire(width = 64, height = 48, imageType = BufferedImage.TYPE_INT_ARGB)
    pool.publish(next)

    pool.acquire(width = 64, height = 48, imageType = BufferedImage.TYPE_INT_ARGB) should be theSameInstanceAs initial
  }

  it should "lease nothing before a frame has been published" in {
    new SwingWindow.ReusableImagePool().leasePublished() shouldBe None
  }

  private def filled(width: Int, height: Int, color: Color): BufferedImage =
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g     = image.createGraphics()
    try
      g.setColor(color)
      g.fillRect(0, 0, width, height)
    finally g.dispose()
    image

  private def paintInto(target: BufferedImage, clip: Rectangle)(paint: Graphics2D => Unit): Unit =
    val g = target.createGraphics()
    try
      g.setClip(clip)
      paint(g)
    finally g.dispose()

end SwingWindowPresentSpec
