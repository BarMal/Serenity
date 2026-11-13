package com.serenity.ui.skia

import java.awt.Rectangle

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FontSpec, ScreenIdentity, StyledFontCache}
import org.jetbrains.skia.Surface

/** The paint a retaining Skia surface recorded for one frame, which `SkiaRasterSurfaceFactory` copies to the screen
  * beside the planner's rects. Specs reach the recorder through here because it is private to this package.
  */
final class PaintLog private (recorder: ChangeRecorder):

  def rects: List[Rectangle] =
    recorder.paintRecords.map(area => Rectangle(area.left, area.top, area.width, area.height))

object PaintLog:

  def retaining(
    surface: Surface,
    metrics: CellMetrics,
    font: FontSpec,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    scale: Double,
    caches: SkiaTextCaches,
    styledFonts: StyledFontCache,
    screen: ScreenIdentity
  ): (SkiaRenderSurface, PaintLog) =
    val recorder = ChangeRecorder(scale, scale, surface.getWidth, surface.getHeight, new PresentPhases)
    val painted = SkiaRenderSurface.retaining(
      surface,
      metrics,
      font,
      logicalWidthPx,
      logicalHeightPx,
      scale,
      caches,
      styledFonts,
      screen = Some(screen),
      changes = Some(recorder)
    )
    (painted, new PaintLog(recorder))
