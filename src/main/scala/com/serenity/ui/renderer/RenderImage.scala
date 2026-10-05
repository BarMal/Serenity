package com.serenity.ui.renderer

import java.awt.image.BufferedImage

/** A raster image a [[RenderSurface]] draws or composites: Markdown preview images and the layer buffers
  * [[LayerBufferSupport]] hands back. Owned by the render seam so renderers pass images around without naming
  * `java.awt` (#1812).
  *
  * Every image is still produced by Java2D (ImageIO decoding, preview rasterising, Java2D layer buffers), so the handle
  * is the `BufferedImage` itself: wrapping and unwrapping cost nothing per frame. A backend that rasterises elsewhere
  * changes only this representation and the `Awt` conversions.
  */
opaque type RenderImage = BufferedImage

object RenderImage:

  def fromAwt(image: BufferedImage): RenderImage = image

  extension (image: RenderImage)
    def widthPx: Int  = image.getWidth
    def heightPx: Int = image.getHeight

    def toAwt: BufferedImage = image
