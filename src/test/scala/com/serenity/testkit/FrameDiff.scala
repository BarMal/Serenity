package com.serenity.testkit

import java.awt.image.{BufferedImage, DataBufferInt, SinglePixelPackedSampleModel}

import scala.annotation.tailrec

/** Which pixels of two equally sized frames differ in colour.
  *
  * The pixel specs compare a frame with a cold one after every step, and a frame that matches is the common case. A
  * match is found by one pass over the pixel arrays in memory order; the coordinates are listed only when something
  * differs, in the order the specs' own loops produced them (column by column, each top to bottom), so a failure reads
  * as it did.
  */
object FrameDiff:

  private val colour = 0xffffff

  def differing(drawn: BufferedImage, expected: BufferedImage): Seq[(Int, Int)] =
    val (width, height) = (drawn.getWidth, drawn.getHeight)
    require(
      width == expected.getWidth && height == expected.getHeight,
      s"frames differ in size: ${width}x$height against ${expected.getWidth}x${expected.getHeight}"
    )
    differing(pixels(drawn), pixels(expected), width, height)

  /** The same for frames already read as `0xAARRGGBB` pixels, one row after another. */
  def differing(actual: Array[Int], expected: Array[Int], width: Int, height: Int): Seq[(Int, Int)] =
    val count = width * height
    require(
      actual.length >= count && expected.length >= count,
      s"a ${width}x$height frame needs $count pixels, got ${actual.length} and ${expected.length}"
    )
    if firstDifference(actual, expected, 0, count) < 0 then Vector.empty
    else
      for
        x <- 0 until width
        y <- 0 until height
        if ((actual(y * width + x) ^ expected(y * width + x)) & colour) != 0
      yield (x, y)

  @tailrec
  private def firstDifference(actual: Array[Int], expected: Array[Int], from: Int, until: Int): Int =
    if from == until then -1
    else if ((actual(from) ^ expected(from)) & colour) != 0 then from
    else firstDifference(actual, expected, from + 1, until)

  private def pixels(image: BufferedImage): Array[Int] =
    ownPixels(image).getOrElse(image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth))

  /** The image's own pixel array, when it is one row after another from its first element and holds the colour as
    * `getRGB` returns it; a view into part of a larger image, or any other layout, is read through `getRGB`.
    */
  private def ownPixels(image: BufferedImage): Option[Array[Int]] =
    val raster = image.getRaster
    (image.getType, raster.getDataBuffer, raster.getSampleModel) match
      case (
            BufferedImage.TYPE_INT_RGB | BufferedImage.TYPE_INT_ARGB,
            buffer: DataBufferInt,
            model: SinglePixelPackedSampleModel
          )
          if buffer.getNumBanks == 1 && buffer.getOffset == 0 && model.getScanlineStride == image.getWidth &&
            raster.getSampleModelTranslateX == 0 && raster.getSampleModelTranslateY == 0 =>
        Some(buffer.getData)
      case _ => None
