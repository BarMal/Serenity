package com.serenity.perf

import java.awt.image.BufferedImage

/** What a failing pixel comparison saw around its first differing pixel: each frame's brightness there (0 dark to 9
  * light), side by side, so a log says whether the damaged frame is missing ink or keeps stale ink.
  */
private[perf] object FrameDifferencePicture:

  private val Reach = 6

  def of(label: String, drawn: BufferedImage, cold: BufferedImage, differences: Seq[(Int, Int)]): String =
    differences.headOption.fold("") { (cx, cy) =>
      def digit(image: BufferedImage, x: Int, y: Int): Char =
        if x < 0 || y < 0 || x >= image.getWidth || y >= image.getHeight then '.'
        else
          val rgb  = image.getRGB(x, y)
          val luma = (((rgb >> 16) & 0xff) * 3 + ((rgb >> 8) & 0xff) * 6 + (rgb & 0xff)) / 10
          ('0' + luma * 10 / 256).toChar
      val rows = (cy - Reach to cy + Reach).map { y =>
        val columns = cx - Reach to cx + Reach
        val left    = columns.map(digit(drawn, _, y)).mkString
        val right   = columns.map(digit(cold, _, y)).mkString
        val marks   = columns.map(x => if differences.contains((x, y)) then '^' else ' ').mkString
        f"$y%5d  $left  $right  $marks"
      }
      s"$label: first difference at ($cx,$cy); drawn | cold | differing\n${rows.mkString("\n")}"
    }
