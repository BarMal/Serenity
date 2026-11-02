package com.serenity.ui.skia

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FontSpec, RenderSurface, StyledFontCache}
import org.jetbrains.skia.{Font, PictureRecorder}

/** Which `(text, font)` stretches a frame will ask the shaped-text cache for, found by drawing it once to a recording
  * canvas on a surface whose cache shapes nothing and only notes what it is asked. The keys are those of the renderer's
  * own line code, so they cannot drift from it.
  */
object ShapePlan:

  /** `draw` is handed the surface to draw the frame on. The surface uses `real`'s fonts, so the keys name the faces a
    * real frame will use; `real`'s shaped text and images are not touched.
    */
  def keysOf(
    real: SkiaTextCaches,
    styledFonts: StyledFontCache,
    metrics: CellMetrics,
    font: FontSpec,
    widthPx: Int,
    heightPx: Int,
    deviceScale: Double
  )(draw: RenderSurface => Unit): Vector[(String, Font)] =
    val recording = new RecordingShapedTextCache
    val caches    = SkiaTextCaches(real.fonts, recording, new SkiaImageCache(), real.inks, real.warmup)
    val recorder  = new PictureRecorder()
    try
      val canvas = PictureRecording.begin(recorder, widthPx, heightPx)
      val surface =
        SkiaRenderSurface.onCanvas(canvas, metrics, font, widthPx, heightPx, deviceScale, caches, styledFonts)
      draw(surface)
      recording.keys
    finally recorder.close()
