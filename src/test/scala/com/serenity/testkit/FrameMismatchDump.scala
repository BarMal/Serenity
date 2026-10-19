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
    val neighbourhoods = clusters(differing).take(3).map((x, y) => neighbourhood(actual, expected, x, y))
    val line = (s"$stem: ${differing.size} px differ; ${samples.mkString("; ")}" +: neighbourhoods).mkString("\n")
    try Files.writeString(directory.resolve(s"$stem.txt"), line + "\n")
    catch case _: java.io.IOException => ()
    line

  private def clusters(differing: Seq[(Int, Int)]): Seq[(Int, Int)] =
    differing.foldLeft(Vector.empty[(Int, Int)]) { (found, pixel) =>
      if found.exists((x, y) => math.abs(x - pixel._1) <= 8 && math.abs(y - pixel._2) <= 5) then found
      else found :+ pixel
    }

  /** The green channel of a 17 by 11 window around (x, y), actual beside expected, '..' marking the same value in both.
    */
  private def neighbourhood(actual: BufferedImage, expected: BufferedImage, x: Int, y: Int): String =
    val rows = (y - 5 to y + 5).filter(row => row >= 0 && row < actual.getHeight).map { row =>
      val cols = (x - 8 to x + 8).filter(col => col >= 0 && col < actual.getWidth)
      def cell(image: BufferedImage, col: Int): String = f"${(image.getRGB(col, row) >> 8) & 0xff}%02x"
      val left                                         = cols.map(col => cell(actual, col)).mkString(" ")
      val right                                        = cols.map(col => cell(expected, col)).mkString(" ")
      f"y=$row%5d  $left  |  $right"
    }
    s"around ($x,$y), x from ${x - 8}, actual | expected:\n${rows.mkString("\n")}"

  private def rgb(image: BufferedImage, x: Int, y: Int): Int = image.getRGB(x, y) & 0xffffff

  private def mask(like: BufferedImage, differing: Seq[(Int, Int)]): BufferedImage =
    val image = new BufferedImage(like.getWidth, like.getHeight, BufferedImage.TYPE_INT_RGB)
    differing.foreach((x, y) => image.setRGB(x, y, 0xff0000))
    image
