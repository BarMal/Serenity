package com.serenity.markdown

import java.awt.Font
import java.util.LinkedHashMap

import scala.util.hashing.MurmurHash3

import com.serenity.ui.theme.Theme

/** Bounded render caches backing [[MarkdownDocumentPreview]], split out so the rendering logic itself isn't buried
  * under cache bookkeeping. Every cache here is a plain size-bounded LRU (`LinkedHashMap` in access-order mode),
  * keyed on a fingerprint of its input rather than the input itself, so repeated renders of unchanged content are
  * free.
  */
private[markdown] object MarkdownPreviewCache:

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

  final case class SlotRender(image: java.awt.image.BufferedImage)

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

  val imageCache =
    new LinkedHashMap[ImageCacheKey, java.awt.image.BufferedImage](MaxCachedImages, 0.75f, true):
      override def removeEldestEntry(
        eldest: java.util.Map.Entry[ImageCacheKey, java.awt.image.BufferedImage]
      ): Boolean =
        size() > MaxCachedImages

  val editSlotCache =
    new LinkedHashMap[ImageSlotKey, SlotRender](MaxEditSlotCacheEntries, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[ImageSlotKey, SlotRender]): Boolean =
        size() > MaxEditSlotCacheEntries

  val htmlFragmentCache =
    new LinkedHashMap[HtmlFragmentCacheKey, String](MaxCachedHtmlFragments, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[HtmlFragmentCacheKey, String]): Boolean =
        size() > MaxCachedHtmlFragments

  val inlineDocumentCache =
    new LinkedHashMap[InlineDocumentCacheKey, MarkdownDocumentPreview.InlinePreviewIndex](
      MaxCachedInlineDocuments,
      0.75f,
      true
    ):
      override def removeEldestEntry(
        eldest: java.util.Map.Entry[InlineDocumentCacheKey, MarkdownDocumentPreview.InlinePreviewIndex]
      ): Boolean =
        size() > MaxCachedInlineDocuments

  /** While `reuseLastRenderWhileEditing` is true, reuses the last image rendered for this preview slot (same
    * title/size/theme/font/etc, only the markdown content differing) instead of paying for a fresh flying-saucer layout
    * pass. Callers set this from an explicit, event-driven signal decided upstream -- e.g. "an edit landed for this
    * buffer more recently than the last settled render" -- never from wall-clock proximity, so the result is fully
    * deterministic given the caller's inputs. `false` (every direct caller's default) always renders fresh, exactly as
    * if this cache didn't exist.
    */
  def renderOrReuseCommitted(key: ImageCacheKey, reuseLastRenderWhileEditing: Boolean)(
    render: => java.awt.image.BufferedImage
  ): java.awt.image.BufferedImage =
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
