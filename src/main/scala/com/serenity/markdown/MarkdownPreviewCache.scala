package com.serenity.markdown

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.LinkedHashMap

import scala.util.hashing.MurmurHash3

import com.serenity.ui.theme.Theme

/** Bounded render caches backing [[MarkdownDocumentPreview]], split out so the rendering logic itself isn't buried
  * under cache bookkeeping. Every cache here is a plain size-bounded LRU (`LinkedHashMap` in access-order mode), keyed
  * on a fingerprint of its input rather than the input itself, so repeated renders of unchanged content are free.
  *
  * `LinkedHashMap` + `synchronized` rather than `Ref[IO, ...]`: every reader/writer of these maps
  * (`MarkdownDocumentPreview.renderHtmlFragment`/`renderImage`/`renderInlineImage`) is a plain synchronous `def` called
  * from the renderer's paint path (`RendererFloatingPanels`, `TuiRuntime`), not from inside an IO fiber, so a
  * `Ref`-backed cache would just have every call site force its `IO` via `unsafeRunSync` right back into the
  * synchronous signature these callers need -- hiding a plain mutable map behind an effect type nothing here ever
  * suspends on. This is the same tradeoff already settled for
  * [[com.serenity.ui.renderer.RendererFrameState.BoundedRefCache]] (#1431/#1434) and `ThemeManager`'s highlight/lex
  * caches (#1412/#1431/#1434). Unlike those two `AtomicReference`-backed caches, each map here is mutated in place
  * rather than swapped by reference (there is no immutable snapshot to compare-and-set between), so `synchronized`
  * around each `get`/`put` is the direct equivalent for a mutable `LinkedHashMap` -- the critical sections are short,
  * so contention is not a concern in this rendering hot path.
  *
  * Instance-scoped (issue #1677): one instance is created per render-owning entity (held on
  * [[com.serenity.state.manager.RenderCaches]], threaded through [[com.serenity.ui.renderer.RenderContext]] and
  * [[MarkdownDocumentPreview]]'s own entry points to every caller) rather than a JVM-wide singleton `object`, so two
  * independently constructed instances share no cache state.
  */
final class MarkdownPreviewCache:
  import MarkdownPreviewCache.*

  private val imageCache =
    new LinkedHashMap[ImageCacheKey, BufferedImage](MaxCachedImages, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[ImageCacheKey, BufferedImage]): Boolean =
        size() > MaxCachedImages

  private val editSlotCache =
    new LinkedHashMap[ImageSlotKey, SlotRender](MaxEditSlotCacheEntries, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[ImageSlotKey, SlotRender]): Boolean =
        size() > MaxEditSlotCacheEntries

  private val htmlFragmentCache =
    new LinkedHashMap[HtmlFragmentCacheKey, String](MaxCachedHtmlFragments, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[HtmlFragmentCacheKey, String]): Boolean =
        size() > MaxCachedHtmlFragments

  private val inlineDocumentCache =
    new LinkedHashMap[InlineDocumentCacheKey, MarkdownDocumentPreview.InlinePreviewIndex](
      MaxCachedInlineDocuments,
      0.75f,
      true
    ):
      override def removeEldestEntry(
        eldest: java.util.Map.Entry[InlineDocumentCacheKey, MarkdownDocumentPreview.InlinePreviewIndex]
      ): Boolean =
        size() > MaxCachedInlineDocuments

  private[markdown] def cachedHtmlFragment(key: HtmlFragmentCacheKey)(render: => String): String =
    htmlFragmentCache.synchronized(Option(htmlFragmentCache.get(key))).getOrElse {
      val rendered = render
      htmlFragmentCache.synchronized {
        val _ = htmlFragmentCache.put(key, rendered)
      }
      rendered
    }

  /** Exact-key image cache first, falling back to [[renderOrReuseCommitted]]'s edit-slot reuse on a miss -- the same
    * two-level lookup `renderImage`/`renderInlineRowsImage` always performed against the (formerly singleton) caches.
    */
  private[markdown] def cachedImage(key: ImageCacheKey, reuseLastRenderWhileEditing: Boolean)(
    render: => BufferedImage
  ): BufferedImage =
    imageCache.synchronized(Option(imageCache.get(key))).getOrElse {
      renderOrReuseCommitted(key, reuseLastRenderWhileEditing) {
        val rendered = render
        imageCache.synchronized {
          val _ = imageCache.put(key, rendered)
        }
        rendered
      }
    }

  private[markdown] def cachedInlineDocument(key: InlineDocumentCacheKey)(
    build: => MarkdownDocumentPreview.InlinePreviewIndex
  ): MarkdownDocumentPreview.InlinePreviewIndex =
    inlineDocumentCache.synchronized(Option(inlineDocumentCache.get(key))).getOrElse {
      val index = build
      inlineDocumentCache.synchronized {
        val _ = inlineDocumentCache.put(key, index)
      }
      index
    }

  /** While `reuseLastRenderWhileEditing` is true, reuses the last image rendered for this preview slot (same
    * title/size/theme/font/etc, only the markdown content differing) instead of paying for a fresh flying-saucer layout
    * pass. Callers set this from an explicit, event-driven signal decided upstream -- e.g. "an edit landed for this
    * buffer more recently than the last settled render" -- never from wall-clock proximity, so the result is fully
    * deterministic given the caller's inputs. `false` (every direct caller's default) always renders fresh, exactly as
    * if this cache didn't exist.
    */
  private def renderOrReuseCommitted(key: ImageCacheKey, reuseLastRenderWhileEditing: Boolean)(
    render: => BufferedImage
  ): BufferedImage =
    val slotKey = ImageSlotKey.from(key)
    val reused =
      if reuseLastRenderWhileEditing then editSlotCache.synchronized(Option(editSlotCache.get(slotKey))) else None
    reused match
      case Some(entry) => entry.image
      case None =>
        val rendered = render
        editSlotCache.synchronized {
          val _ = editSlotCache.put(slotKey, SlotRender(rendered))
        }
        rendered

object MarkdownPreviewCache:

  def apply(): MarkdownPreviewCache = new MarkdownPreviewCache

  private val MaxCachedImages          = 24
  private val MaxCachedHtmlFragments   = 48
  private val MaxCachedInlineDocuments = 32
  private val MaxEditSlotCacheEntries  = 24

  final case class SourceFingerprint(length: Int, hash: Int)

  object SourceFingerprint:
    def from(source: String): SourceFingerprint =
      SourceFingerprint(source.length, MurmurHash3.stringHash(source))

  final case class ImageCacheKey(
      source: SourceFingerprint,
      title: String,
      widthPx: Int,
      heightPx: Int,
      theme: Theme,
      font: Font,
      baseUri: Option[String],
      panelChrome: Boolean,
      inlineLineHeightPx: Option[Int],
      inlineRows: Boolean
  )

  /** Identifies an `ImageCacheKey` without its `source` fingerprint, so rapid successive renders of the same preview
    * slot (same title/size/theme/font/etc, only the markdown content changing keystroke to keystroke) can be recognised
    * as "the same thing being retyped" rather than unrelated cache entries.
    */
  final case class ImageSlotKey(
      title: String,
      widthPx: Int,
      heightPx: Int,
      theme: Theme,
      font: Font,
      baseUri: Option[String],
      panelChrome: Boolean,
      inlineLineHeightPx: Option[Int],
      inlineRows: Boolean
  )

  object ImageSlotKey:

    def from(key: ImageCacheKey): ImageSlotKey =
      ImageSlotKey(
        key.title,
        key.widthPx,
        key.heightPx,
        key.theme,
        key.font,
        key.baseUri,
        key.panelChrome,
        key.inlineLineHeightPx,
        key.inlineRows
      )

  final case class SlotRender(image: BufferedImage)

  final case class HtmlFragmentCacheKey(source: SourceFingerprint, title: String, baseUri: Option[String])

  final case class SourceLinesFingerprint(lineCount: Int, totalLength: Int, hash: Int)

  object SourceLinesFingerprint:

    def from(sourceLines: Vector[String]): SourceLinesFingerprint =
      SourceLinesFingerprint(
        lineCount = sourceLines.length,
        totalLength = sourceLines.map(_.length).sum,
        hash = MurmurHash3.orderedHash(sourceLines)
      )

  final case class InlineDocumentCacheKey(source: SourceLinesFingerprint)
