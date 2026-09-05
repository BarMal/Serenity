package com.serenity.ui.tui

import java.awt.Color
import java.awt.image.BufferedImage

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The half-block Unicode conversion at the core of a real TUI `drawImage`: downsample the source image to twice the
  * target cell grid's vertical resolution, then paint one cell as an upper-half-block glyph (`▀`) whose foreground is
  * the top sub-pixel and whose background is the bottom one -- the notcurses/ratatui-image/Jexer technique.
  */
class HalfBlockImageRendererSpec extends AnyFlatSpec with Matchers:

  private def solid(width: Int, height: Int)(colorAt: (Int, Int) => Int): BufferedImage =
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    for
      x <- 0 until width
      y <- 0 until height
    do image.setRGB(x, y, colorAt(x, y))
    image

  "HalfBlockImageRenderer.render" should "map each cell to an upper-half-block glyph with fg=top pixel, bg=bottom pixel" in {
    // A 1x2 image, one column, two rows: red on top, blue on the bottom -- exactly one cell's worth of sub-pixels.
    val image = solid(1, 2) { case (_, y) => if y == 0 then 0xffff0000 else 0xff0000ff }

    val grid = HalfBlockImageRenderer.render(image, cellsWide = 1, cellsHigh = 1, fallback = Color.BLACK)

    grid should have length 1
    grid.head should have length 1
    grid.head.head shouldBe Some(HalfBlockImageRenderer.HalfBlockCell(new Color(0xffff0000, true), new Color(0xff0000ff, true)))
  }

  it should "produce one row of cells per two source pixel rows, and one column per source pixel column" in {
    val image = solid(2, 4)((_, _) => 0xffffffff)
    val grid  = HalfBlockImageRenderer.render(image, cellsWide = 2, cellsHigh = 2, fallback = Color.BLACK)

    grid should have length 2
    grid.foreach(_ should have length 2)
  }

  it should "leave a fully transparent cell as None so the caller can skip painting it" in {
    val image = solid(1, 2)((_, _) => 0x00000000)
    val grid  = HalfBlockImageRenderer.render(image, cellsWide = 1, cellsHigh = 1, fallback = Color.BLACK)

    grid.head.head shouldBe None
  }

  it should "substitute the fallback color for a transparent half of an otherwise opaque cell" in {
    val image = solid(1, 2) { case (_, y) => if y == 0 then 0xffff0000 else 0x00000000 }
    val fallback = new Color(10, 20, 30)

    val grid = HalfBlockImageRenderer.render(image, cellsWide = 1, cellsHigh = 1, fallback = fallback)

    grid.head.head shouldBe Some(HalfBlockImageRenderer.HalfBlockCell(new Color(0xffff0000, true), fallback))
  }

  it should "downsample a larger image to a smaller cell grid using nearest-neighbor sampling" in {
    // Left half of the image is red, right half is green; downsampling to 2 columns should keep them distinguishable.
    val image = solid(4, 2) { case (x, _) => if x < 2 then 0xffff0000 else 0xff00ff00 }

    val grid = HalfBlockImageRenderer.render(image, cellsWide = 2, cellsHigh = 1, fallback = Color.BLACK)

    grid.head(0).map(_.foreground) shouldBe Some(new Color(0xffff0000, true))
    grid.head(1).map(_.foreground) shouldBe Some(new Color(0xff00ff00, true))
  }
