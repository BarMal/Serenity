package com.serenity.ui.skia

import java.awt.image.{BufferedImage, DataBufferInt}

import com.serenity.ui.renderer.DeviceArea

/** [[PixelMove]] for the pooled image, so it can follow a scrolled surface without a copy out of Skia. */
private[skia] object ImageMove:

  /** `area` of `image` moved down by `dyPx` rows (up if negative), as [[PixelMove.inPlace]] moves a surface's: rows
    * that would land outside the image are dropped, and nothing outside the destination changes. Rows are copied from
    * the first to the last when moving up and from the last to the first when moving down, so no row is read after it
    * was overwritten.
    */
  def apply(image: BufferedImage, area: DeviceArea, dyPx: Int): Unit =
    val width = image.getWidth
    val left  = area.left.max(0)
    val right = (area.left + area.width).min(width)
    val first = area.top.max(0).max(-dyPx)
    val last  = (area.top + area.height).min(image.getHeight).min(image.getHeight - dyPx)
    if dyPx != 0 && left < right && first < last then
      val rows = if dyPx < 0 then first until last else (first until last).reverse
      image.getRaster.getDataBuffer match
        case target: DataBufferInt =>
          val pixels = target.getData
          rows.foreach(row =>
            System.arraycopy(pixels, row * width + left, pixels, (row + dyPx) * width + left, right - left)
          )
        case _ =>
          rows.foreach { row =>
            val line = image.getRGB(left, row, right - left, 1, new Array[Int](right - left), 0, right - left)
            image.setRGB(left, row + dyPx, right - left, 1, line, 0, right - left)
          }
