package com.serenity

import java.awt.geom.RoundRectangle2D
import java.awt.image.BufferedImage
import java.awt.{Color, Rectangle, RenderingHints}

import com.serenity.ui.terminal.SwingWindow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rounded-corner mask only ever touches the four arc-sized corner tiles of the window. */
class SwingWindowRoundedCornerSpec extends AnyFlatSpec with Matchers:

  "SwingWindow.cornerTileBounds" should "place one arc-sized tile in each corner" in {
    SwingWindow.cornerTileBounds(200, 100, 12) shouldBe List(
      new Rectangle(0, 0, 12, 12),
      new Rectangle(188, 0, 12, 12),
      new Rectangle(0, 88, 12, 12),
      new Rectangle(188, 88, 12, 12)
    )
  }

  it should "shrink the tiles to fit a window narrower than the arc" in {
    SwingWindow.cornerTileBounds(8, 30, 12) shouldBe List(
      new Rectangle(0, 0, 8, 12),
      new Rectangle(0, 0, 8, 12),
      new Rectangle(0, 18, 8, 12),
      new Rectangle(0, 18, 8, 12)
    )
  }

  it should "have no tiles without a corner arc" in {
    SwingWindow.cornerTileBounds(200, 100, 0) shouldBe Nil
  }

  "SwingWindow.RoundedCornerMaskBuffers" should "select only the corner tiles a repaint clip touches" in {
    val buffers = new SwingWindow.RoundedCornerMaskBufferCache().acquire(200, 100, cornerArc = 12)

    buffers.cornersTouching(new Rectangle(50, 40, 20, 10)) shouldBe Nil
    buffers.cornersTouching(new Rectangle(190, 95, 4, 4)).map(_.bounds) shouldBe List(new Rectangle(188, 88, 12, 12))
    buffers.cornersTouching(new Rectangle(0, 0, 200, 100)).size shouldBe 4
  }

  it should "mask each corner exactly as the whole-window antialiased mask would" in {
    val (width, height, arc) = (200, 120, 24)
    val reference            = wholeWindowMask(width, height, arc)
    val buffers              = new SwingWindow.RoundedCornerMaskBufferCache().acquire(width, height, arc)

    buffers.corners.foreach { tile =>
      val masked = buffers.render(
        tile,
        g =>
          g.setColor(Color.WHITE)
          g.fillRect(0, 0, width, height)
      )
      for
        x <- 0 until tile.bounds.width
        y <- 0 until tile.bounds.height
      do
        withClue(s"corner ${tile.bounds} pixel ($x, $y): ") {
          alpha(masked.getRGB(x, y)) shouldBe alpha(reference.getRGB(tile.bounds.x + x, tile.bounds.y + y))
        }
    }
  }

  it should "paint each tile from the window's own coordinates" in {
    val buffers     = new SwingWindow.RoundedCornerMaskBufferCache().acquire(100, 80, cornerArc = 10)
    val bottomRight = buffers.corners.find(_.bounds == new Rectangle(90, 70, 10, 10)).getOrElse(fail("no corner"))

    val masked = buffers.render(
      bottomRight,
      g =>
        g.setColor(Color.GREEN)
        g.fillRect(90, 70, 2, 2)
    )

    new Color(masked.getRGB(0, 0), true) shouldBe Color.GREEN
    alpha(masked.getRGB(5, 5)) shouldBe 0
  }

  private def alpha(argb: Int): Int = (argb >>> 24) & 0xff

  /** The mask the window used before it was split into tiles: the whole window supersampled 2x, then downsampled. */
  private def wholeWindowMask(width: Int, height: Int, arc: Int): BufferedImage =
    val supersampled = new BufferedImage(width * 2, height * 2, BufferedImage.TYPE_INT_ARGB)
    val sg           = supersampled.createGraphics()
    try
      sg.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      sg.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      sg.setColor(Color.WHITE)
      sg.fill(new RoundRectangle2D.Double(0, 0, width * 2.0, height * 2.0, arc * 2.0, arc * 2.0))
    finally sg.dispose()
    val mask = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val mg   = mask.createGraphics()
    try
      mg.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      mg.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
      mg.drawImage(supersampled, 0, 0, width, height, null)
    finally mg.dispose()
    mask

end SwingWindowRoundedCornerSpec
