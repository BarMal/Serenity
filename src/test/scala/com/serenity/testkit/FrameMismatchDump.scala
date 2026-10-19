package com.serenity.testkit

import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger
import javax.imageio.ImageIO

/** Writes the frames of a failed pixel comparison next to the JUnit reports, so the CI artifact of a Windows or macOS
  * run shows which pixels differ and how, instead of only how many.
  */
object FrameMismatchDump:

  private val directory = Path.of("target", "test-reports", "frame-mismatches")
  private val counter   = new AtomicInteger(0)

  /** Writes `actual`, `expected` and a mask of the differing pixels as PNGs, and returns a line naming them and the
    * colours at the first differing pixels.
    */
  def write(label: String, actual: BufferedImage, expected: BufferedImage, differing: Seq[(Int, Int)]): String =
    val stem = f"${counter.incrementAndGet()}%03d-${label.replaceAll("[^A-Za-z0-9]+", "-").take(80)}"
    try
      Files.createDirectories(directory)
      ImageIO.write(actual, "png", directory.resolve(s"$stem-actual.png").toFile)
      ImageIO.write(expected, "png", directory.resolve(s"$stem-expected.png").toFile)
      ImageIO.write(mask(actual, differing), "png", directory.resolve(s"$stem-diff.png").toFile)
    catch case _: java.io.IOException => ()
    val samples = differing
      .take(8)
      .map((x, y) => f"($x,$y) actual=${rgb(actual, x, y)}%06x expected=${rgb(expected, x, y)}%06x")
    val line = s"$stem: ${differing.size} px differ; ${samples.mkString("; ")}"
    try Files.writeString(directory.resolve(s"$stem.txt"), line + "\n")
    catch case _: java.io.IOException => ()
    line

  private def rgb(image: BufferedImage, x: Int, y: Int): Int = image.getRGB(x, y) & 0xffffff

  private def mask(like: BufferedImage, differing: Seq[(Int, Int)]): BufferedImage =
    val image = new BufferedImage(like.getWidth, like.getHeight, BufferedImage.TYPE_INT_RGB)
    differing.foreach((x, y) => image.setRGB(x, y, 0xff0000))
    image
