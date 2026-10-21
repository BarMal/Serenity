package com.serenity.testkit

import java.awt.image.BufferedImage

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScreenLocationSpec extends AnyFlatSpec with Matchers:

  private val blue = 0x40a0ff

  private def screenWith(paint: BufferedImage => Unit): BufferedImage =
    val screen = new BufferedImage(200, 100, BufferedImage.TYPE_INT_RGB)
    paint(screen)
    screen

  "firstPixelOf" should "find a window where it really is, however far from where the toolkit says" in {
    val screen = screenWith(image => (0 until 30).foreach(x => image.setRGB(60 + x, 40, blue)))

    ScreenLocation.firstPixelOf(screen, blue) shouldBe Some((60, 40))
  }

  it should "read rows from the top and each from the left" in {
    val screen = screenWith { image =>
      image.setRGB(150, 10, blue)
      image.setRGB(20, 10, blue)
      image.setRGB(5, 11, blue)
    }

    ScreenLocation.firstPixelOf(screen, blue) shouldBe Some((20, 10))
  }

  it should "ignore the alpha byte of the colour asked for" in {
    val screen = screenWith(_.setRGB(7, 3, blue))

    ScreenLocation.firstPixelOf(screen, 0xff000000 | blue) shouldBe Some((7, 3))
  }

  it should "find nothing on a screen that never showed the window" in {
    ScreenLocation.firstPixelOf(screenWith(_ => ()), blue) shouldBe None
  }

  "candidateCorners" should "try the toolkit's position first, then the found pixel and its neighbours" in {
    val corners = ScreenLocation.candidateCorners((0, 59), Some((1, 60)), slack = 1)

    corners.head shouldBe ((0, 59))
    corners should contain((1, 60))
    corners should contain allOf ((0, 59), (2, 61), (0, 61), (2, 59))
    corners.distinct shouldBe corners
    corners.length shouldBe 9
  }

  it should "offer only the toolkit's position when nothing was found on the screen" in {
    ScreenLocation.candidateCorners((5, 6), None, slack = 2) shouldBe List((5, 6))
  }

  "bestAlignment" should "find zero mismatches where the window really is, off from where the toolkit said" in {
    val (screen, expected) = stripedWindowAt(60, 40)

    val (count, corner) = ScreenLocation.bestAlignment(candidates(screen))(mismatchesAt(screen, expected))

    count shouldBe 0
    corner shouldBe ((60, 40))
  }

  it should "leave a corrupted region mismatching at every candidate, so it still fails" in {
    val (screen, expected) = stripedWindowAt(60, 40)
    (66 until 90).foreach(x => (50 until 70).foreach(y => screen.setRGB(x, y, 0xffffff)))

    val corners    = candidates(screen)
    val (count, _) = ScreenLocation.bestAlignment(corners)(mismatchesAt(screen, expected))

    corners.length should be > 20
    corners.foreach(c => mismatchesAt(screen, expected)(c) should be > 0)
    count should be > 0
  }

  private val frameWidth  = 100
  private val frameHeight = 60

  /** A screen with a striped frame painted at (x, y), and the frame it was painted from. */
  private def stripedWindowAt(x: Int, y: Int): (BufferedImage, BufferedImage) =
    val frame = new BufferedImage(frameWidth, frameHeight, BufferedImage.TYPE_INT_RGB)
    (0 until frameHeight).foreach(row =>
      (0 until frameWidth).foreach(col => frame.setRGB(col, row, if row % 6 < 2 then blue else 0x202020))
    )
    val screen = new BufferedImage(300, 200, BufferedImage.TYPE_INT_RGB)
    (0 until frameHeight).foreach(row =>
      (0 until frameWidth).foreach(col => screen.setRGB(x + col, y + row, frame.getRGB(col, row)))
    )
    (screen, frame)

  private def candidates(screen: BufferedImage): List[(Int, Int)] =
    ScreenLocation.candidateCorners((0, 0), ScreenLocation.firstPixelOf(screen, blue), slack = 2)

  /** Pixels of the frame's inner region that differ from the screen when the frame's corner is taken to be `corner`. */
  private def mismatchesAt(screen: BufferedImage, frame: BufferedImage)(corner: (Int, Int)): Int =
    (for
      row <- 10 until 50
      col <- 10 until 90
      if screen.getRGB(corner._1 + col, corner._2 + row) != frame.getRGB(col, row)
    yield 1).size
