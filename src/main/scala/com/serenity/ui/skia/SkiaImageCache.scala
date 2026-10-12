package com.serenity.ui.skia

import java.awt.image.BufferedImage
import java.nio.ByteOrder
import java.util.{Collections, WeakHashMap}

import org.jetbrains.skia.{ColorAlphaType, Image, ImageInfo}

/** The Skia image behind each [[RenderImage]] a surface draws, converted once and kept for as long as the source
  * `BufferedImage` lives.
  *
  * A [[RenderImage]] is a `BufferedImage` (see its doc), and Skia cannot draw one: its pixels are copied into a Skia
  * raster image the first time it is drawn. Every image drawn here is a Markdown preview, rasterised once and never
  * drawn into afterwards, which is what makes keying on the source's identity safe; an image repainted in place would
  * keep showing its first pixels. The key is weak, so a closed preview takes its Skia copy with it.
  */
final class SkiaImageCache:
  private val images = Collections.synchronizedMap(new WeakHashMap[BufferedImage, Image]())

  def imageOf(source: BufferedImage): Image =
    Option(images.get(source)).getOrElse {
      val converted = SkiaImageCache.convert(source)
      val _         = images.put(source, converted)
      converted
    }

  def size: Int = images.size

object SkiaImageCache:

  /** Shared by every surface that is not handed its own. */
  val shared: SkiaImageCache = new SkiaImageCache()

  private val BytesPerPixel = 4

  /** Skia's native 32-bit order is BGRA on every platform Skiko ships for, which is an ARGB `int` written
    * little-endian; the pixels are copied as they are and Skia premultiplies them as it draws.
    */
  private[skia] def convert(source: BufferedImage): Image =
    val width  = source.getWidth
    val height = source.getHeight
    val argb   = source.getRGB(0, 0, width, height, new Array[Int](width * height), 0, width)
    val bytes  = new Array[Byte](argb.length * BytesPerPixel)
    java.nio.ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer().put(argb)
    Image.Companion.makeRaster(
      ImageInfo.Companion.makeN32(width, height, ColorAlphaType.UNPREMUL),
      bytes,
      width * BytesPerPixel
    )
