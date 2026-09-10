package com.serenity.ui.renderer

import java.awt.image.BufferedImage

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated unit coverage for `RenderSurfaceCapabilities` (issue #1421). The file is mostly capability-trait
  * declarations with no logic of their own to test; its one piece of independently-checkable behavior is
  * [[PixelDrawing.compositeFullSurfaceLayer]]'s default implementation, which every surface without its own
  * backing-resolution override (every headless test double, and every real surface but `Java2DRenderSurface`) falls
  * back to.
  *
  * `HardwareCursorStyle.decscusrParam` -- this file's other piece of pure logic -- already has dedicated coverage in
  * [[HardwareCursorSpec]], so it is not duplicated here.
  */
class RenderSurfaceCapabilitiesSpec extends AnyFlatSpec with Matchers:

  private class RecordingPixelDrawing extends PixelDrawing:
    private val calls = scala.collection.mutable.ListBuffer.empty[(BufferedImage, Int, Int, Int, Int)]
    def drawImageCalls: List[(BufferedImage, Int, Int, Int, Int)]                                   = calls.toList
    def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: java.awt.Color): Unit = ()
    def drawImage(image: BufferedImage, x: Int, y: Int, width: Int, height: Int): Unit =
      calls += ((image, x, y, width, height))
    def withPixelTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit = render

  "compositeFullSurfaceLayer" should "default to a full-surface drawImage at the origin, sized to the image itself" in {
    val pixels = new RecordingPixelDrawing
    val layer  = new BufferedImage(37, 21, BufferedImage.TYPE_INT_ARGB)

    pixels.compositeFullSurfaceLayer(layer)

    pixels.drawImageCalls shouldBe List((layer, 0, 0, 37, 21))
  }

  it should "read the image's own width/height rather than a fixed size, for every image it is given" in {
    val pixels     = new RecordingPixelDrawing
    val smallLayer = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB)
    val bigLayer   = new BufferedImage(200, 5, BufferedImage.TYPE_INT_ARGB)

    pixels.compositeFullSurfaceLayer(smallLayer)
    pixels.compositeFullSurfaceLayer(bigLayer)

    pixels.drawImageCalls shouldBe List(
      (smallLayer, 0, 0, 4, 4),
      (bigLayer, 0, 0, 200, 5)
    )
  }
