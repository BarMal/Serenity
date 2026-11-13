package com.serenity.ui.renderer

import java.awt.Font
import java.awt.image.BufferedImage

import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.terminal.LayerPresenter

/** What the Swing frontend knows when it needs a surface for one frame of the window canvas.
  *
  * `backendCaches` is the owner's slot for what a backend keeps between frames. `changedRegion` is read when the frame
  * ends: `None` for the whole canvas, otherwise the logical-pixel rects outside which the frame matches what the
  * previous frame left on screen. `attachLayer` hands the window the GPU layer a backend made, so the window can give
  * it the carets and the pointer cursor.
  */
final case class FrameSurfaceRequest(
    metrics: CellMetrics,
    font: Font,
    geometry: SurfaceGeometry,
    site: PresentationSite,
    onFlush: BufferedImage => Unit,
    acquireImage: (Int, Int, Int) => BufferedImage,
    textCaches: SurfaceTextCaches,
    backendCaches: BackendCaches,
    changedRegion: () => Option[List[PixelRect]],
    contentPersists: Boolean = true,
    attachLayer: LayerPresenter => Unit = _ => ()
)

/** The seam the frontend draws a frame through. The backend chosen at startup supplies the implementation. */
trait FrameSurfaceFactory:
  def create(request: FrameSurfaceRequest): RenderSurface
