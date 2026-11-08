package com.serenity.testkit

import java.awt.Rectangle

import com.serenity.ui.layout.PixelRect

/** What the specs' model of a window copies from the frame it drew to the screen, as `SkiaRasterSurfaceFactory` does:
  * the rects the planner names plus every area the surface recorded painting, so ink that passes its row's rect still
  * reaches the screen. `None` is the whole frame.
  */
object PresentAreas:

  def of(
    planner: Option[List[PixelRect]],
    recordedPaint: List[Rectangle],
    scale: Double,
    widthPx: Int,
    heightPx: Int
  ): Option[List[Rectangle]] =
    val frame = Rectangle(0, 0, widthPx, heightPx)
    planner.map { rects =>
      val named    = rects.flatMap(FramePlanReport.deviceRect(_, scale, widthPx, heightPx))
      val recorded = recordedPaint.map(_.intersection(frame)).filterNot(_.isEmpty)
      named ++ recorded
    }
