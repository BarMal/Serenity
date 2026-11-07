package com.serenity.testkit

import java.awt.image.BufferedImage

import scala.util.Random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FrameDiffSpec extends AnyFlatSpec with Matchers:

  private def filled(width: Int, height: Int, rgb: Int, imageType: Int = BufferedImage.TYPE_INT_RGB): BufferedImage =
    val image = BufferedImage(width, height, imageType)
    (0 until height).foreach(y => (0 until width).foreach(x => image.setRGB(x, y, rgb)))
    image

  /** What the pixel specs computed before this helper: every column in turn, the pixels of each top to bottom. */
  private def reference(drawn: BufferedImage, expected: BufferedImage): Seq[(Int, Int)] =
    for
      x <- 0 until drawn.getWidth
      y <- 0 until drawn.getHeight
      if (drawn.getRGB(x, y) & 0xffffff) != (expected.getRGB(x, y) & 0xffffff)
    yield (x, y)

  "Two frames with the same colours" should "have no differing pixels" in {
    FrameDiff.differing(filled(7, 5, 0x336699), filled(7, 5, 0x336699)) shouldBe empty
  }

  "A frame with one pixel changed" should "name exactly that pixel" in {
    val drawn = filled(7, 5, 0x336699)
    drawn.setRGB(4, 3, 0x336698)
    FrameDiff.differing(drawn, filled(7, 5, 0x336699)) shouldBe Seq((4, 3))
  }

  "The last pixel of the last row" should "be compared as well" in {
    val drawn = filled(7, 5, 0x000000)
    drawn.setRGB(6, 4, 0x010000)
    FrameDiff.differing(drawn, filled(7, 5, 0x000000)) shouldBe Seq((6, 4))
  }

  "Differing pixels" should "come back column by column, top to bottom, as the pixel specs listed them" in {
    val drawn = filled(6, 6, 0x202020)
    List((3, 1), (1, 4), (1, 2), (3, 0)).foreach((x, y) => drawn.setRGB(x, y, 0xffffff))
    FrameDiff.differing(drawn, filled(6, 6, 0x202020)) shouldBe Seq((1, 2), (1, 4), (3, 0), (3, 1))
  }

  "A difference in the alpha byte alone" should "not count, as the colour mask never counted it" in {
    val translucent = filled(4, 4, 0x80123456, BufferedImage.TYPE_INT_ARGB)
    val opaque      = filled(4, 4, 0xff123456, BufferedImage.TYPE_INT_ARGB)
    FrameDiff.differing(translucent, opaque) shouldBe empty
  }

  "Frames of different image types with the same colours" should "have no differing pixels" in {
    FrameDiff.differing(filled(5, 5, 0x123456), filled(5, 5, 0x123456, BufferedImage.TYPE_INT_ARGB)) shouldBe empty
  }

  "A view into part of a larger image" should "be compared by what it shows, with coordinates inside the view" in {
    val large = filled(10, 10, 0x000000)
    large.setRGB(5, 6, 0xabcdef)
    val view = large.getSubimage(4, 5, 4, 4)
    val same = filled(4, 4, 0x000000)
    same.setRGB(1, 1, 0xabcdef)
    FrameDiff.differing(view, same) shouldBe empty
    same.setRGB(2, 3, 0x000001)
    FrameDiff.differing(view, same) shouldBe Seq((2, 3))
  }

  "A byte-per-pixel image" should "still be compared by colour" in {
    val grey  = BufferedImage(4, 4, BufferedImage.TYPE_BYTE_GRAY)
    val other = BufferedImage(4, 4, BufferedImage.TYPE_BYTE_GRAY)
    other.setRGB(2, 1, 0xffffff)
    FrameDiff.differing(grey, other) shouldBe Seq((2, 1))
  }

  "Frames of different sizes" should "be refused, not compared over the part they share" in {
    val error = intercept[IllegalArgumentException](FrameDiff.differing(filled(4, 4, 0), filled(4, 5, 0)))
    error.getMessage should include("4x4")
    error.getMessage should include("4x5")
  }

  "Random frames" should "give the pixels the earlier loop gave, whether or not they differ" in {
    val random = Random(7)
    (1 to 40).foreach { round =>
      val (width, height) = (1 + random.nextInt(24), 1 + random.nextInt(24))
      val drawn           = filled(width, height, random.nextInt(0x1000000))
      val expected        = filled(width, height, drawn.getRGB(0, 0))
      (0 until random.nextInt(6)).foreach { _ =>
        drawn.setRGB(random.nextInt(width), random.nextInt(height), random.nextInt(0x1000000))
      }
      withClue(s"round $round ${width}x$height: ") {
        FrameDiff.differing(drawn, expected) shouldBe reference(drawn, expected)
      }
    }
  }
