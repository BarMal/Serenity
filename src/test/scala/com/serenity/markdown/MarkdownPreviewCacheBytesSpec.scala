package com.serenity.markdown

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger
import javax.imageio.ImageIO

import com.serenity.TestTemp
import com.serenity.markdown.MarkdownPreviewCache.{ImageCacheKey, SourceFingerprint}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkdownPreviewCacheBytesSpec extends AnyFlatSpec with Matchers:

  private val font    = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val MiB     = 1024L * 1024L
  private val Hundred = 100L * MiB

  private def keyFor(title: String, width: Int, height: Int, source: String = "# doc"): ImageCacheKey =
    ImageCacheKey(
      source = SourceFingerprint.from(source),
      title = title,
      widthPx = width,
      heightPx = height,
      theme = Theme.default,
      font = font,
      baseUri = None,
      panelChrome = true,
      inlineLineHeightPx = None,
      inlineRows = false
    )

  private def argb(width: Int, height: Int): BufferedImage =
    new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

  private def store(cache: MarkdownPreviewCache, key: ImageCacheKey, image: BufferedImage): BufferedImage =
    cache.cachedImage(key, reuseLastRenderWhileEditing = false)(image)

  private def lookup(cache: MarkdownPreviewCache, key: ImageCacheKey): Option[BufferedImage] =
    val renders = AtomicInteger(0)
    val found = cache.cachedImage(key, reuseLastRenderWhileEditing = false) {
      renders.incrementAndGet()
      argb(1, 1)
    }
    Option.when(renders.get == 0)(found)

  "MarkdownPreviewCache image storage" should "stay within its byte cap across 30 distinct sizes of 2000x2000 renders" in {
    val cache  = MarkdownPreviewCache(maxImageBytes = Hundred)
    val keys   = (0 until 30).map(i => keyFor(s"slot-$i.md", 2000, 2000 + i))
    val images = keys.map(key => store(cache, key, argb(2000, 2000)))

    cache.retainedImageBytes should be <= Hundred
    lookup(cache, keys.last) shouldBe Some(images.last)
  }

  it should "evict by bytes, not by count, so large images displace many entries" in {
    val cache = MarkdownPreviewCache(maxImageBytes = Hundred)
    (0 until 30).foreach(i => store(cache, keyFor(s"slot-$i.md", 2000, 2000), argb(2000, 2000)))

    cache.retainedImageBytes should be <= Hundred
    cache.cachedImageCount shouldBe 6
  }

  it should "keep only the latest size when the same slot is resized repeatedly" in {
    val cache  = MarkdownPreviewCache(maxImageBytes = 1024L * MiB)
    val keys   = (0 until 10).map(i => keyFor("resized.md", 200 + i * 10, 150))
    val images = keys.map(key => store(cache, key, argb(key.widthPx, key.heightPx)))

    cache.cachedImageCount shouldBe 1
    lookup(cache, keys.last) shouldBe Some(images.last)
    lookup(cache, keys.head) shouldBe None
  }

  it should "keep same-size renders of different content for one slot" in {
    val cache = MarkdownPreviewCache(maxImageBytes = 1024L * MiB)
    val a     = keyFor("undo.md", 200, 150, source = "a")
    val b     = keyFor("undo.md", 200, 150, source = "b")
    val imgA  = store(cache, a, argb(200, 150))
    store(cache, b, argb(200, 150))

    lookup(cache, a) shouldBe Some(imgA)
  }

  it should "evict the least recently used entry first" in {
    val cache = MarkdownPreviewCache(maxImageBytes = 9L * MiB)
    val a     = keyFor("a.md", 1000, 1000)
    val b     = keyFor("b.md", 1000, 1000)
    val c     = keyFor("c.md", 1000, 1000)
    store(cache, a, argb(1000, 1000))
    store(cache, b, argb(1000, 1000))
    lookup(cache, a) should not be empty
    store(cache, c, argb(1000, 1000))

    lookup(cache, b) shouldBe None
    lookup(cache, a) should not be empty
  }

  it should "never evict the image just rendered, even when it alone exceeds the cap" in {
    val cache = MarkdownPreviewCache(maxImageBytes = MiB)
    val key   = keyFor("huge.md", 2000, 2000)
    val image = store(cache, key, argb(2000, 2000))

    cache.cachedImageCount shouldBe 1
    lookup(cache, key) shouldBe Some(image)
  }

  it should "weigh images by their actual pixel size rather than assuming four bytes per pixel" in {
    val cache = MarkdownPreviewCache(maxImageBytes = Hundred)
    store(cache, keyFor("gray.md", 1000, 1000), new BufferedImage(1000, 1000, BufferedImage.TYPE_BYTE_GRAY))

    cache.retainedImageBytes shouldBe 1000L * 1000L
  }

  it should "count a rendered image once even though it also serves edit-slot reuse" in {
    val cache = MarkdownPreviewCache(maxImageBytes = Hundred)
    store(cache, keyFor("shared.md", 1000, 1000), argb(1000, 1000))

    cache.retainedImageBytes shouldBe 4_000_000L
  }

  private def withDirectory[A](body: Path => A): A =
    val dir = TestTemp.directory("preview-cache-bytes").toRealPath()
    try body(dir)
    finally
      Files.list(dir).forEach(p => Files.deleteIfExists(p))
      Files.deleteIfExists(dir)

  private def countingDecoder(counter: AtomicInteger): Array[Byte] => Option[BufferedImage] =
    bytes =>
      counter.incrementAndGet()
      MarkdownPreviewImageResources.decodeWithinLimits(bytes)

  private def writePng(path: Path, width: Int, height: Int): Unit =
    val _ = ImageIO.write(argb(width, height), "png", path.toFile)

  private def touch(file: Path): Unit =
    val _ = Files.setLastModifiedTime(file, FileTime.fromMillis(Files.getLastModifiedTime(file).toMillis + 5000L))

  "DecodedImageCache" should "decode an unchanged local image once across renders of the same markdown" in
    withDirectory { dir =>
      writePng(dir.resolve("pic.png"), 20, 10)
      val decodes = AtomicInteger(0)
      val cache   = MarkdownPreviewCache(decodedImages = DecodedImageCache(32L * MiB, countingDecoder(decodes)))
      def render(width: Int): BufferedImage =
        MarkdownDocumentPreview.renderImage(
          source = "# Pic\n\n![pic](pic.png)",
          title = "pic.md",
          widthPx = width,
          heightPx = 200,
          theme = Theme.default,
          font = font,
          cache = cache,
          baseUri = Some(dir.toUri)
        )

      render(300)
      render(320)

      decodes.get shouldBe 1
    }

  it should "decode again when the file's modification time changes" in
    withDirectory { dir =>
      val file = dir.resolve("pic.png")
      writePng(file, 20, 10)
      val decodes = AtomicInteger(0)
      val decoded = DecodedImageCache(32L * MiB, countingDecoder(decodes))
      val policy  = new MarkdownPreviewImageResources.PreviewResourcePolicy(Some(dir.toUri), decoded)
      val uri     = file.toUri.toString

      policy.imageFor(uri).getWidth shouldBe 20
      policy.imageFor(uri).getWidth shouldBe 20
      decodes.get shouldBe 1

      touch(file)
      policy.imageFor(uri).getWidth shouldBe 20
      decodes.get shouldBe 2
    }

  it should "hold only the newest version of a path" in
    withDirectory { dir =>
      val file = dir.resolve("pic.png")
      writePng(file, 20, 10)
      val decoded = DecodedImageCache(32L * MiB, countingDecoder(AtomicInteger(0)))
      val policy  = new MarkdownPreviewImageResources.PreviewResourcePolicy(Some(dir.toUri), decoded)

      policy.imageFor(file.toUri.toString)
      touch(file)
      policy.imageFor(file.toUri.toString)

      decoded.entryCount shouldBe 1
    }

  it should "stay within its byte cap, evicting the least recently used image" in
    withDirectory { dir =>
      val names = (0 until 5).map(i => s"pic-$i.png")
      names.foreach(name => writePng(dir.resolve(name), 100, 100))
      val decoded = DecodedImageCache(100_000L, countingDecoder(AtomicInteger(0)))
      val policy  = new MarkdownPreviewImageResources.PreviewResourcePolicy(Some(dir.toUri), decoded)

      names.foreach(name => policy.imageFor(dir.resolve(name).toUri.toString))

      decoded.retainedBytes should be <= 100_000L
      decoded.entryCount shouldBe 2
    }

  it should "not retain an image larger than its whole cap" in
    withDirectory { dir =>
      writePng(dir.resolve("big.png"), 100, 100)
      val decoded = DecodedImageCache(1000L)
      val policy  = new MarkdownPreviewImageResources.PreviewResourcePolicy(Some(dir.toUri), decoded)

      policy.imageFor(dir.resolve("big.png").toUri.toString).getWidth shouldBe 100
      decoded.entryCount shouldBe 0
    }
end MarkdownPreviewCacheBytesSpec
