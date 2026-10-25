package com.serenity

import java.awt.Font
import java.awt.image.BufferedImage

import com.serenity.state.manager.RenderCaches
import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FontSpec, Java2DRenderSurface, RenderSurface, ScreenIdentity}
import com.serenity.ui.skia.{SkiaRenderSurface, SkiaTextCaches}
import org.jetbrains.skia.{Bitmap, BlendMode, IRect, ImageInfo, Paint, Rect, Surface}

/** The pixels of one pooled frame, as the pixel specs read and stamp them whatever renders into them. */
trait FramePixels:
  def widthPx: Int
  def heightPx: Int

  /** The colour of one device pixel; the alpha byte is whatever the backing store holds. */
  def rgbAt(x: Int, y: Int): Int

  /** Overwrites one device pixel, as a sentinel that survives only if nothing paints it afterwards. */
  def stamp(x: Int, y: Int, rgb: Int): Unit

  /** Every pixel, row by row. */
  def rgbPixels: Array[Int]

  /** Paints every pixel with `rgb`. */
  def fill(rgb: Int): Unit

  /** Copies the `width` x `height` pixels at (`x`, `y`) of `source`, a frame of the same renderer, to the same place.
    */
  def blit(source: FramePixels, x: Int, y: Int, width: Int, height: Int): Unit

  /** The frame as an image, for the dump a failed comparison writes. */
  final def toImage: BufferedImage =
    val image = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)
    image.setRGB(0, 0, widthPx, heightPx, rgbPixels, 0, widthPx)
    image

/** A renderer the persisted-frame specs can run against: a pooled pixel store and a surface painting into it. */
trait FrameBackend:
  def newFrame(widthPx: Int, heightPx: Int): FramePixels

  def surfaceOver(
    frame: FramePixels,
    metrics: CellMetrics,
    font: Font,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    scale: Double,
    persists: Boolean,
    caches: RenderCaches,
    screen: Option[ScreenIdentity] = None
  ): RenderSurface

object Java2DFrames extends FrameBackend:

  final private class Java2DFrame(val image: BufferedImage) extends FramePixels:
    def widthPx: Int                          = image.getWidth
    def heightPx: Int                         = image.getHeight
    def rgbAt(x: Int, y: Int): Int            = image.getRGB(x, y)
    def stamp(x: Int, y: Int, rgb: Int): Unit = image.setRGB(x, y, rgb)

    def rgbPixels: Array[Int] =
      image.getRGB(0, 0, widthPx, heightPx, new Array[Int](widthPx * heightPx), 0, widthPx)

    def fill(rgb: Int): Unit =
      val graphics = image.createGraphics()
      try
        graphics.setComposite(java.awt.AlphaComposite.Src)
        graphics.setColor(new java.awt.Color(rgb))
        graphics.fillRect(0, 0, widthPx, heightPx)
      finally graphics.dispose()

    def blit(source: FramePixels, x: Int, y: Int, width: Int, height: Int): Unit = source match
      case from: Java2DFrame =>
        val graphics = image.createGraphics()
        try graphics.drawImage(from.image.getSubimage(x, y, width, height), x, y, null)
        finally graphics.dispose()
      case other => throw new IllegalArgumentException(s"not a Java2D frame: $other")

  def newFrame(widthPx: Int, heightPx: Int): FramePixels =
    new Java2DFrame(new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB))

  def surfaceOver(
    frame: FramePixels,
    metrics: CellMetrics,
    font: Font,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    scale: Double,
    persists: Boolean,
    caches: RenderCaches,
    screen: Option[ScreenIdentity]
  ): RenderSurface =
    frame match
      case java2d: Java2DFrame =>
        new Java2DRenderSurface(
          java2d.image,
          metrics,
          font,
          _ => (),
          logicalWidthPx = logicalWidthPx,
          logicalHeightPx = logicalHeightPx,
          deviceScaleX = scale,
          deviceScaleY = scale,
          contentPersists = persists,
          layerCacheOwnerOverride = screen.orElse(Some(ScreenIdentity(java2d.image))),
          textCaches = caches.surfaceText
        )
      case other => throw new IllegalArgumentException(s"not a Java2D frame: $other")

/** The renderer the pixel specs paint with: a frame and a surface painting into it, read back as an image. */
object ReferenceFrames:

  final case class Painted(frame: FramePixels, surface: RenderSurface):
    def image: BufferedImage = frame.toImage

  def over(
    widthPx: Int,
    heightPx: Int,
    metrics: CellMetrics,
    font: Font,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    scale: Double = 1.0
  ): Painted =
    val frame = SkiaFrames.newFrame(widthPx, heightPx)
    Painted(
      frame,
      SkiaFrames.surfaceOver(
        frame,
        metrics,
        font,
        logicalWidthPx,
        logicalHeightPx,
        scale,
        persists = false,
        RenderCaches.create()
      )
    )

/** Reading a raster `Surface` back: the CPU pixels, as `0xAARRGGBB` ints, row by row. */
object SkiaPixels:

  def read(surface: Surface): Array[Int] =
    val info  = ImageInfo.Companion.makeN32Premul(surface.getWidth, surface.getHeight)
    val whole = Bitmap()
    require(whole.allocPixels(info) && surface.readPixels(whole, 0, 0), "cannot read the surface")
    val bytes  = whole.readPixels(info, surface.getWidth * 4, 0, 0)
    val pixels = new Array[Int](surface.getWidth * surface.getHeight)
    java.nio.ByteBuffer.wrap(bytes).order(java.nio.ByteOrder.LITTLE_ENDIAN).asIntBuffer().get(pixels)
    pixels

/** Skia painting into an off-screen raster `Surface`: the CPU rasteriser, with no window, so it runs in CI. */
object SkiaFrames extends FrameBackend:

  final private class SkiaFrame(val surface: Surface) extends FramePixels:
    def widthPx: Int  = surface.getWidth
    def heightPx: Int = surface.getHeight

    def rgbAt(x: Int, y: Int): Int =
      val pixel = Bitmap()
      require(
        pixel.allocPixels(ImageInfo.Companion.makeN32Premul(1, 1)) && surface.readPixels(pixel, x, y),
        s"cannot read pixel $x,$y"
      )
      pixel.getColor(0, 0)

    def stamp(x: Int, y: Int, rgb: Int): Unit =
      val ink = Paint()
      ink.setBlendMode(BlendMode.SRC)
      ink.setColor(0xff000000 | rgb)
      val _ = surface.getCanvas.drawRect(Rect.Companion.makeXYWH(x.toFloat, y.toFloat, 1f, 1f), ink)

    def rgbPixels: Array[Int] = SkiaPixels.read(surface)

    def fill(rgb: Int): Unit = surface.getCanvas.clear(0xff000000 | rgb)

    def blit(source: FramePixels, x: Int, y: Int, width: Int, height: Int): Unit = source match
      case from: SkiaFrame =>
        val copy = Paint()
        copy.setBlendMode(BlendMode.SRC)
        val image = from.surface.makeImageSnapshot(IRect.Companion.makeXYWH(x, y, width, height))
        try surface.getCanvas.drawImage(image, x.toFloat, y.toFloat, copy)
        finally image.close()
      case other => throw new IllegalArgumentException(s"not a Skia frame: $other")

  def newFrame(widthPx: Int, heightPx: Int): FramePixels =
    new SkiaFrame(Surface.Companion.makeRasterN32Premul(widthPx, heightPx))

  def surfaceOver(
    frame: FramePixels,
    metrics: CellMetrics,
    font: Font,
    logicalWidthPx: Int,
    logicalHeightPx: Int,
    scale: Double,
    persists: Boolean,
    caches: RenderCaches,
    screen: Option[ScreenIdentity]
  ): RenderSurface =
    frame match
      case skia: SkiaFrame =>
        if persists then
          SkiaRenderSurface.retaining(
            skia.surface,
            metrics,
            FontSpec.fromAwt(font),
            logicalWidthPx,
            logicalHeightPx,
            scale,
            SkiaTextCaches.of(caches.backendCaches),
            caches.styledFonts,
            layerCacheOwner = screen.orElse(Some(ScreenIdentity(skia)))
          )
        else
          SkiaRenderSurface.onCanvas(
            skia.surface.getCanvas,
            metrics,
            FontSpec.fromAwt(font),
            logicalWidthPx,
            logicalHeightPx,
            scale,
            SkiaTextCaches.of(caches.backendCaches),
            caches.styledFonts
          )
      case other => throw new IllegalArgumentException(s"not a Skia frame: $other")
