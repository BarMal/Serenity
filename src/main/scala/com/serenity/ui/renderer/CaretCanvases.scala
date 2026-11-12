package com.serenity.ui.renderer

import com.serenity.ui.layout.CellMetrics

/** The surface a caret recorder measures against, and how to free what backs it. */
final case class CaretCanvas(surface: RenderSurface, release: () => Unit)

/** Where the cursor path gets that surface. The target's backend supplies it, so the renderer names no backend, as it
  * names none when it asks a [[FrameSurfaceFactory]] for a frame.
  */
trait CaretCanvases:
  def forGeometry(metrics: CellMetrics, font: FontSpec, geometry: SurfaceGeometry): CaretCanvas
