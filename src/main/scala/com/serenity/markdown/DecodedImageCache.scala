package com.serenity.markdown

import java.awt.image.BufferedImage
import java.nio.file.Path

import com.serenity.markdown.DecodedImageCache.DecodedImageKey

/** Byte-bounded LRU of decoded local preview images, so an unchanged image file referenced from a preview is decoded
  * once rather than on every render. Keyed on (path, mtime, size): a rewritten file gets a new key and replaces the
  * previous version of that path. An image larger than the whole cap is returned but never retained, so a single huge
  * decode cannot flush everything else.
  */
final class DecodedImageCache(
    maxBytes: Long,
    decode: Array[Byte] => Option[BufferedImage]
):

  private val store = new ByteBoundedLru[DecodedImageKey, BufferedImage](
    maxBytes,
    ImageBytes.of,
    (newKey, existing) => newKey.path == existing.path
  )

  /** `readBytes` only runs on a miss, so a hit costs the caller a stat call and nothing else. */
  def decoded(key: DecodedImageKey)(readBytes: => Option[Array[Byte]]): Option[BufferedImage] =
    store.get(key).orElse {
      val image = readBytes.flatMap(decode)
      image.filter(ImageBytes.of(_) <= maxBytes).foreach(store.put(key, _))
      image
    }

  def retainedBytes: Long = store.retainedBytes

  def entryCount: Int = store.size

object DecodedImageCache:

  val DefaultMaxBytes: Long = 32L * 1024L * 1024L

  def apply(): DecodedImageCache = apply(DefaultMaxBytes)

  def apply(maxBytes: Long): DecodedImageCache =
    new DecodedImageCache(maxBytes, MarkdownPreviewImageResources.decodeWithinLimits)

  def apply(maxBytes: Long, decode: Array[Byte] => Option[BufferedImage]): DecodedImageCache =
    new DecodedImageCache(maxBytes, decode)

  final case class DecodedImageKey(path: Path, lastModifiedMillis: Long, sizeBytes: Long)

private[markdown] object ImageBytes:

  def of(image: BufferedImage): Long =
    val bytesPerPixel = ((image.getColorModel.getPixelSize + 7) / 8).max(1)
    image.getWidth.toLong * image.getHeight.toLong * bytesPerPixel.toLong
