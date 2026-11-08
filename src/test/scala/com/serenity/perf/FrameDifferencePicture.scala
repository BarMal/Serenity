package com.serenity.perf

import java.awt.image.BufferedImage

import com.serenity.ui.layout.{CellMetrics, LayoutRect}
import com.serenity.ui.renderer.EditorPaneRenderPlan

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
      val samples = differences
        .take(6)
        .map((x, y) => f"($x,$y) drawn=${drawn.getRGB(x, y) & 0xffffff}%06x cold=${cold.getRGB(x, y) & 0xffffff}%06x")
      s"$label: first difference at ($cx,$cy); ${samples.mkString("; ")}; drawn | cold | differing\n${rows.mkString("\n")}"
    }

  /** Every rect of the plan's layout contract and pane layouts, in device pixels, that touches the first differing
    * pixel: which element of the frame the difference belongs to.
    */
  def elementsAt(
    plan: Option[EditorPaneRenderPlan],
    cells: CellMetrics,
    deviceScale: Double,
    differences: Seq[(Int, Int)]
  ): String =
    (for
      (cx, cy)  <- differences.headOption.toList
      framePlan <- plan.toList
      (name, rect) <- namedRects("contract", framePlan.layoutContract) ++
        framePlan.paneLayouts.toList.flatMap((id, layout) => namedRects(s"pane ${id.value}", layout))
      left   = (rect.x * cells.charWidth * deviceScale).toInt
      top    = (rect.y * cells.lineHeight * deviceScale).toInt
      right  = ((rect.x + rect.width) * cells.charWidth * deviceScale).toInt
      bottom = ((rect.y + rect.height) * cells.lineHeight * deviceScale).toInt
      if cx >= left - Reach && cx < right + Reach && cy >= top - Reach && cy < bottom + Reach
    yield s"  $name: x $left-$right, y $top-$bottom").mkString("\nelements around the first difference:\n", "\n", "")

  private def namedRects(prefix: String, owner: Product): List[(String, LayoutRect)] =
    owner.productElementNames.zip(owner.productIterator).toList.flatMap {
      case (name, rect: LayoutRect)       => List(s"$prefix.$name" -> rect)
      case (name, Some(rect: LayoutRect)) => List(s"$prefix.$name" -> rect)
      case _                              => Nil
    }
