package com.serenity.ui.renderer

import com.serenity.ui.layout.CellMetrics

/** Where the cursor path gets the surface a caret recorder measures against. The target's backend supplies it, so the
  * renderer names no backend, as it names none when it asks a [[FrameSurfaceFactory]] for a frame. A canvas may be kept
  * from one frame to the next, so nothing may close it after a frame.
  */
trait CaretCanvases:
  def forGeometry(metrics: CellMetrics, font: FontSpec, geometry: SurfaceGeometry): RenderSurface
