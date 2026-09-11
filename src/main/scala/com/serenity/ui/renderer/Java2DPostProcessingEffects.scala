package com.serenity.ui.renderer

import java.awt.*
import java.awt.image.*

/** Scanline and glow post-processing, operating directly on a [[Java2DRenderSurface]]'s backing `BufferedImage` via
  * raw `Graphics2D`/pixel-array operations. Split out of `Java2DRenderSurface` purely to keep that class within the
  * architecture line-count ratchet; every method here is stateless and takes the target `image` as a parameter,
  * reaching into `Java2DRenderSurface`'s companion helpers (`defaultRenderingHints`, `compatibleDestImage`) since both
  * live in the same `com.serenity.ui.renderer` package.
  */
private[renderer] object Java2DPostProcessingEffects:

  def applyScanlines(image: BufferedImage, animationPhase: Long): Unit =
    val rawGraphics = image.createGraphics()
    try
      val phase = (animationPhase % 97L).toInt
      drawScanlines(image, rawGraphics, 1 + phase % 3, phase)
      rawGraphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.025f))
      (0 until image.getWidth).foreach { x =>
        rawGraphics.setColor(
          x % 3 match
            case 0 => Color.RED
            case 1 => Color.GREEN
            case _ => Color.BLUE
        )
        rawGraphics.drawLine(x, 0, x, image.getHeight - 1)
      }
    finally rawGraphics.dispose()

  private def drawScanlines(image: BufferedImage, rawGraphics: Graphics2D, y: Int, phase: Int): Unit =
    if y < image.getHeight then
      val thickness = if (y + phase) % 11 <= 1 then 2 else 1
      val alpha     = if thickness == 2 then 0.18f else 0.13f
      rawGraphics.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha))
      rawGraphics.setColor(Color.BLACK)
      (y until math.min(image.getHeight, y + thickness)).foreach(row =>
        rawGraphics.drawLine(0, row, image.getWidth - 1, row)
      )
      val spacing = 3 + math.floorMod(y + phase, 4)
      drawScanlines(image, rawGraphics, y + thickness + spacing, phase)

  /** Spreads bright (or, on a light background, dark) pixels into a soft halo.
    *
    * Operates on raw ARGB `int[]` pixel arrays end to end -- one bulk `getRGB`/`setRGB` transfer in, one out -- and
    * blends with [[blendSrcOver]] instead of per-pixel `Graphics2D` calls: the previous implementation issued up to 24
    * `setColor`/`fillRect`/`drawImage` calls per masked source pixel, which is a well-known Java2D anti-pattern for
    * full-image compositing.
    */
  def applyGlow(image: BufferedImage): Unit =
    val width       = image.getWidth
    val height      = image.getHeight
    val background  = estimatedBackgroundColor(image)
    val backgroundR = background.getRed
    val backgroundG = background.getGreen
    val backgroundB = background.getBlue

    val basePixels   = image.getRGB(0, 0, width, height, new Array[Int](width * height), 0, width)
    val sourcePixels = new Array[Int](width * height)
    val sourceMask   = new Array[Boolean](width * height)

    (0 until basePixels.length).foreach { index =>
      val argb     = basePixels(index)
      val r        = (argb >>> 16) & 0xff
      val g        = (argb >>> 8) & 0xff
      val b        = argb & 0xff
      val contrast = math.abs(r - backgroundR) + math.abs(g - backgroundG) + math.abs(b - backgroundB)
      if contrast >= 96 then
        sourcePixels(index) = argb
        sourceMask(index) = true
    }

    val source = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    source.setRGB(0, 0, width, height, sourcePixels, 0, width)

    val glowOp = new ConvolveOp(
      new Kernel(
        5,
        5,
        Array(1f, 4f, 6f, 4f, 1f, 4f, 16f, 24f, 16f, 4f, 6f, 24f, 36f, 24f, 6f, 4f, 16f, 24f, 16f, 4f, 1f, 4f, 6f, 4f,
          1f).map(_ / 128f)
      ),
      ConvolveOp.EDGE_NO_OP,
      Java2DRenderSurface.defaultRenderingHints
    )
    val blurred       = glowOp.filter(source, Java2DRenderSurface.compatibleDestImage(glowOp, source))
    val blurredPixels = blurred.getRGB(0, 0, width, height, new Array[Int](width * height), 0, width)

    val result = basePixels.clone()

    // Spread each masked source pixel into a soft halo across its 5x5 neighborhood.
    (0 until height).foreach { y =>
      (0 until width).foreach { x =>
        val index = y * width + x
        if sourceMask(index) then
          val srcColor = sourcePixels(index)
          val srcR     = (srcColor >>> 16) & 0xff
          val srcG     = (srcColor >>> 8) & 0xff
          val srcB     = srcColor & 0xff
          (-2 to 2).foreach { yOffset =>
            (-2 to 2).foreach { xOffset =>
              val distance = math.max(math.abs(xOffset), math.abs(yOffset))
              val nx       = x + xOffset
              val ny       = y + yOffset
              if distance > 0 && nx >= 0 && nx < width && ny >= 0 && ny < height then
                val alpha       = if distance == 1 then 14 else 6
                val targetIndex = ny * width + nx
                result(targetIndex) = blendSrcOver(result(targetIndex), srcR, srcG, srcB, alpha)
            }
          }
      }
    }

    // Composite the Gaussian-blurred glow on top, with per-pixel alpha derived from its brightest channel.
    (0 until blurredPixels.length).foreach { index =>
      val bc        = blurredPixels(index)
      val ba        = (bc >>> 24) & 0xff
      val br        = (bc >>> 16) & 0xff
      val bg        = (bc >>> 8) & 0xff
      val bb        = bc & 0xff
      val intensity = ba.max(br).max(bg).max(bb)
      if intensity > 0 then
        val alpha = math.max(1, (intensity * 0.8f).toInt)
        result(index) = blendSrcOver(result(index), br, bg, bb, alpha)
    }

    // Restore the sharp original source pixels exactly where masked, undoing any halo/blur bleed on top of them.
    (0 until sourcePixels.length).foreach { index =>
      if sourceMask(index) then
        val srcColor = sourcePixels(index)
        val srcA     = (srcColor >>> 24) & 0xff
        val srcR     = (srcColor >>> 16) & 0xff
        val srcG     = (srcColor >>> 8) & 0xff
        val srcB     = srcColor & 0xff
        result(index) = blendSrcOver(result(index), srcR, srcG, srcB, srcA)
    }

    image.setRGB(0, 0, width, height, result, 0, width)

  private def estimatedBackgroundColor(image: BufferedImage): Color =
    val horizontalStep = (image.getWidth / 64).max(1)
    val verticalStep   = (image.getHeight / 64).max(1)
    val sampledColors =
      (0 until image.getWidth by horizontalStep).iterator
        .flatMap(x => (0 until image.getHeight by verticalStep).iterator.map(y => image.getRGB(x, y)))
    val counts = sampledColors.foldLeft(Map.empty[Int, Int]) { (accumulator, color) =>
      accumulator.updated(color, accumulator.getOrElse(color, 0) + 1)
    }
    // `image` is always at least 1x1 (`deviceImageDimension` floors both dimensions to 1), so `counts` is never
    // empty in practice; the (0, 0) starting pair only matters as a total fallback for that impossible case, in
    // place of the exception a bare `.maxBy` would throw on an empty map.
    val (dominantColor, _) = counts.foldLeft((0, 0)) {
      case (best @ (_, bestCount), (color, count)) =>
        if count > bestCount then (color, count) else best
    }
    new Color(dominantColor, true)

  /** Standard Porter-Duff SRC_OVER, matching what `Graphics2D`'s default composite computes for opaque-source draws
    * onto a `TYPE_INT_ARGB` destination -- reimplemented here so `applyGlow` can blend directly on a raw pixel array
    * instead of issuing one `Graphics2D` draw call per pixel.
    */
  private def blendSrcOver(destArgb: Int, srcR: Int, srcG: Int, srcB: Int, srcA: Int): Int =
    if srcA <= 0 then destArgb
    else
      val dstA = (destArgb >>> 24) & 0xff
      val dstR = (destArgb >>> 16) & 0xff
      val dstG = (destArgb >>> 8) & 0xff
      val dstB = destArgb & 0xff
      val outA = srcA + dstA * (255 - srcA) / 255
      def blendChannel(srcC: Int, dstC: Int): Int =
        if outA == 0 then 0
        else ((srcC * srcA + dstC * dstA * (255 - srcA) / 255) / outA).min(255).max(0)
      (outA << 24) | (blendChannel(srcR, dstR) << 16) | (blendChannel(srcG, dstG) << 8) | blendChannel(srcB, dstB)
