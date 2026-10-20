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
