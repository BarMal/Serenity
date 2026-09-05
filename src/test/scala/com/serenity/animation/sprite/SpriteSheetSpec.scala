package com.serenity.animation.sprite

import java.awt.image.BufferedImage

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpriteSheetSpec extends AnyFlatSpec with Matchers:

  /** A 4-frame, 2x2-pixel-per-frame sheet where each frame is a single solid color, so slicing can be checked pixel-
    * for-pixel rather than just by dimensions.
    */
  private def testSheet(colors: Vector[Int]): BufferedImage =
    val frameSize = 2
    val sheet     = new BufferedImage(frameSize * colors.length, frameSize, BufferedImage.TYPE_INT_ARGB)
    colors.zipWithIndex.foreach {
      case (color, index) =>
        for
          x <- 0 until frameSize
          y <- 0 until frameSize
        do sheet.setRGB(index * frameSize + x, y, color)
    }
    sheet

  "SpriteSheetLayout.horizontalStrip" should "place equally sized frames left to right with no gap" in {
    val layout = SpriteSheetLayout.horizontalStrip("idle", frameWidth = 16, frameHeight = 16, frameCount = 4)

    layout.clip("idle") shouldBe Some(
      SpriteClip(
        "idle",
        Vector(
          SpriteFrameRect(0, 0, 16, 16),
          SpriteFrameRect(16, 0, 16, 16),
          SpriteFrameRect(32, 0, 16, 16),
          SpriteFrameRect(48, 0, 16, 16)
        )
      )
    )
  }

  it should "return None for a clip that was never declared" in {
    val layout = SpriteSheetLayout.horizontalStrip("idle", 16, 16, 4)
    layout.clip("walk") shouldBe None
  }

  "SpriteSheetSlicer.frames" should "slice each frame rect into a correctly sized, correctly positioned sub-image" in {
    val colors = Vector(0xffff0000, 0xff00ff00, 0xff0000ff, 0xffffff00)
    val sheet  = testSheet(colors)
    val clip   = SpriteClip("idle", Vector.tabulate(4)(i => SpriteFrameRect(i * 2, 0, 2, 2)))

    val frames = SpriteSheetSlicer.frames(sheet, clip)

    frames should have length 4
    frames.zip(colors).foreach {
      case (frame, color) =>
        frame.getWidth shouldBe 2
        frame.getHeight shouldBe 2
        frame.getRGB(0, 0) shouldBe color
        frame.getRGB(1, 1) shouldBe color
    }
  }

  "SpriteSheetSlicer.slice" should "slice every declared clip, keyed by clip name" in {
    val colors = Vector(0xffff0000, 0xff00ff00, 0xff0000ff, 0xffffff00)
    val sheet  = testSheet(colors)
    val layout = SpriteSheetLayout.horizontalStrip("idle", frameWidth = 2, frameHeight = 2, frameCount = 4)

    val sliced = SpriteSheetSlicer.slice(sheet, layout)

    sliced.keySet shouldBe Set("idle")
    sliced("idle") should have length 4
  }

  "SpriteClip" should "play back its frame list in exactly the order declared, including a ping-pong repeat" in {
    // Three distinct source frames (red, green, blue) but a clip that plays them back red, green, blue, green --
    // a repeated frame, not just "one pass over the sheet in file order". The clip's own frame list is what decides
    // playback order; sheet position alone would never produce this sequence.
    val colors  = Vector(0xffff0000, 0xff00ff00, 0xff0000ff)
    val sheet   = testSheet(colors)
    val red     = SpriteFrameRect(0, 0, 2, 2)
    val green   = SpriteFrameRect(2, 0, 2, 2)
    val blue    = SpriteFrameRect(4, 0, 2, 2)
    val clip    = SpriteClip("blink", Vector(red, green, blue, green))
    val frames  = SpriteSheetSlicer.frames(sheet, clip)
    val topLeft = frames.map(_.getRGB(0, 0))

    topLeft shouldBe Vector(0xffff0000, 0xff00ff00, 0xff0000ff, 0xff00ff00)
  }
