package com.serenity.ui.renderer

import java.awt.Image
import java.awt.image.ImageObserver

/** Sizes shared by every surface and by the Swing canvas that presents them, whichever renderer paints. */
object SurfaceSizing:

  /** `Graphics2D.drawImage`'s `ImageObserver` callback exists for images that may still be loading asynchronously (e.g.
    * from a URL); every image drawn here is an already-fully-materialised `BufferedImage`, so the callback can never
    * fire and a `null` observer (Java's usual shorthand for "don't call me back") is equivalent to this no-op.
    * Returning `false` tells the (never-invoked) caller not to bother scheduling further notifications.
    */
  private[serenity] val NoOpImageObserver: ImageObserver =
    (_: Image, _: Int, _: Int, _: Int, _: Int, _: Int) => false

  private[serenity] def deviceImageDimension(logicalDimensionPx: Int, deviceScale: Double): Int =
    math.ceil(logicalDimensionPx.max(1) * deviceScale.max(1.0)).toInt.max(1)

  private[serenity] def logicalCanvasDimension(currentPx: Int, preferredPx: Int): Int =
    if currentPx > 0 then currentPx
    else preferredPx.max(1)
