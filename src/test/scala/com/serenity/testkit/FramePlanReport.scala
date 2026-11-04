package com.serenity.testkit

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
      val repaint = planned.repaintRegion.fold("the whole canvas")(rects => s"${rects.size} rect(s)")
      s"frame plan: dirty rows {${panes.mkString("; ")}}; preserved y ${bands(planned.preserved)}; " +
        s"chrome ${planned.chrome}; blits ${planned.blits.size}; repaint $repaint"
    }

  private def bands(preserved: List[PixelRect]): String =
    val merged = preserved.map(rect => (rect.yPx, rect.bottomPx)).sorted.foldLeft(List.empty[(Int, Int)]) {
      case ((from, to) :: earlier, (nextFrom, nextTo)) if nextFrom <= to => (from, math.max(to, nextTo)) :: earlier
      case (bands, band)                                                 => band :: bands
    }
    if merged.isEmpty then "none" else merged.reverse.map((from, to) => s"$from-$to").mkString(", ")
