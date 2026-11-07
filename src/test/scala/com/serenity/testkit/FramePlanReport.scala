package com.serenity.testkit

import java.awt.Rectangle

import com.serenity.state.models.PaneId.value
import com.serenity.ui.layout.PixelRect
import com.serenity.ui.renderer.FramePlan

/** One line saying what the planner drew and kept for a frame, for the message of a pixel comparison that failed: the
  * rows it drew per pane, the vertical bands it kept from earlier frames, and what it moved and repainted on screen.
  */
object FramePlanReport:

  def describe(plan: Option[FramePlan]): String =
    plan.fold("frame plan: no plan, the frame drew everything") { planned =>
      val panes = planned.dirtyRowsByPane.toList
        .sortBy((pane, _) => pane.value)
        .map((pane, rows) =>
          s"pane ${pane.value}: ${if rows.isEmpty then "none" else rows.toList.sorted.mkString(", ")}"
        )
      val repaint = planned.repaintRegion.fold("the whole canvas")(rects =>
        s"${rects.size} rect(s) (x,y wxh, logical px): ${rects.map(r => s"${r.xPx},${r.yPx} ${r.widthPx}x${r.heightPx}").mkString("; ")}"
      )
      s"frame plan: dirty rows {${panes.mkString("; ")}}; preserved y ${bands(planned.preserved)}; " +
        s"chrome ${planned.chrome}; blits ${planned.blits.size}; repaint $repaint"
    }

  private def bands(preserved: List[PixelRect]): String =
    val merged = preserved.map(rect => (rect.yPx, rect.bottomPx)).sorted.foldLeft(List.empty[(Int, Int)]) {
      case ((from, to) :: earlier, (nextFrom, nextTo)) if nextFrom <= to => (from, math.max(to, nextTo)) :: earlier
      case (bands, band)                                                 => band :: bands
    }
    if merged.isEmpty then "none" else merged.reverse.map((from, to) => s"$from-$to").mkString(", ")

  /** The device pixels a window shows for `rect` when it presents a frame `widthPx` by `heightPx`: the same floor,
    * ceiling and clip as the specs' `present`, so that "outside" here means the screen would not have been given it.
    */
  def deviceRect(rect: PixelRect, scale: Double, widthPx: Int, heightPx: Int): Option[Rectangle] =
    val area = Rectangle(
      math.floor(rect.xPx * scale).toInt,
      math.floor(rect.yPx * scale).toInt,
      math.ceil(rect.widthPx * scale).toInt,
      math.ceil(rect.heightPx * scale).toInt
    ).intersection(Rectangle(0, 0, widthPx, heightPx))
    Option.unless(area.isEmpty)(area)

  private val ShownPixels = 6

  /** Which of the differing pixels (device coordinates) lie in a rect the frame presented and which outside all of
    * them: a pixel outside was never given to the screen, whatever the frame drew there.
    */
  def coverage(
    plan: Option[FramePlan],
    differing: Seq[(Int, Int)],
    scale: Double,
    widthPx: Int,
    heightPx: Int
  ): String =
    plan.fold("repaint coverage: no plan") { planned =>
      planned.repaintRegion.fold("repaint coverage: the whole frame was presented") { rects =>
        val areas = rects.map(deviceRect(_, scale, widthPx, heightPx))
        def where(pixel: (Int, Int)): Option[Int] =
          areas.indexWhere(_.exists(_.contains(pixel._1, pixel._2))) match
            case -1    => None
            case index => Some(index + 1)
        val outside = differing.count(where(_).isEmpty)
        val shown = differing.take(ShownPixels).map { pixel =>
          val place = where(pixel).fold("outside")(index => s"inside rect $index")
          s"(${pixel._1},${pixel._2}) $place"
        }
        val more = if differing.size > ShownPixels then s"; and ${differing.size - ShownPixels} more" else ""
        s"repaint coverage: $outside of ${differing.size} differing pixels lie outside every repaint rect; " +
          shown.mkString("; ") + more
      }
    }
