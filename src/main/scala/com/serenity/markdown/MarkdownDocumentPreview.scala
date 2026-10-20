package com.serenity.markdown

import java.awt.Font
import java.awt.image.BufferedImage
import java.net.URI

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import com.serenity.rope.Rope
import com.serenity.ui.theme.Theme
import org.commonmark.Extension
import org.commonmark.ext.gfm.tables.TablesExtension
import org.commonmark.ext.task.list.items.TaskListItemsExtension
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.*
import org.xhtmlrenderer.swing.Java2DRenderer

object MarkdownDocumentPreview:

  final case class InlinePreviewLine(sourceLine: Option[Int], text: String)

  final case class PreviewWindow(firstSourceLine: Int, firstPreviewRow: Int, source: String)

  final private[markdown] case class InlinePreviewIndex(
      previewLines: Vector[InlinePreviewLine]
  )

  private val extensions: java.util.List[Extension] =
    List[Extension](TablesExtension.create(), TaskListItemsExtension.create()).asJava

  private val parser: Parser =
    Parser
      .builder()
      .extensions(extensions)
      .build()

  private val defaultHtmlRenderer: HtmlRenderer =
    HtmlRenderer
      .builder()
      .extensions(extensions)
      .escapeHtml(true)
      .build()

  def renderHtmlFragment(
    source: String,
    title: String,
    cache: MarkdownPreviewCache,
    baseUri: Option[URI] = None
  ): String =
    val key =
      MarkdownPreviewCache.HtmlFragmentCacheKey(
        MarkdownPreviewCache.SourceFingerprint.from(source),
        title,
        baseUri.map(_.toString)
      )
    cache.cachedHtmlFragment(key) {
      MarkdownPreviewXhtml.htmlRenderer(extensions, defaultHtmlRenderer, baseUri).render(parser.parse(source))
    }

  def renderImage(
    source: String,
    title: String,
    widthPx: Int,
    heightPx: Int,
    theme: Theme,
    font: Font,
    cache: MarkdownPreviewCache,
    baseUri: Option[URI] = None,
    panelChrome: Boolean = true,
    reuseLastRenderWhileEditing: Boolean = false
  ): BufferedImage =
    val safeWidth  = widthPx.max(1)
    val safeHeight = heightPx.max(1)
    val key = MarkdownPreviewCache.ImageCacheKey(
      source = MarkdownPreviewCache.SourceFingerprint.from(source),
      title = title,
      widthPx = safeWidth,
      heightPx = safeHeight,
      theme = theme,
      font = font,
      baseUri = baseUri.map(_.toString),
      panelChrome = panelChrome
    )
    cache.cachedImage(key, reuseLastRenderWhileEditing) {
      renderImageUncached(
        source,
        title,
        safeWidth,
        safeHeight,
        theme,
        font,
        baseUri,
        panelChrome,
        cache
      )
    }

  private def renderImageUncached(
    source: String,
    title: String,
    safeWidth: Int,
    safeHeight: Int,
    theme: Theme,
    font: Font,
    baseUri: Option[URI],
    panelChrome: Boolean,
    cache: MarkdownPreviewCache
  ): BufferedImage =
    try
      val renderer = Java2DRenderer(
        MarkdownPreviewImageResources.parseXhtml(
          renderXhtml(source, title, theme, font, baseUri, panelChrome, cache)
        ),
        safeWidth,
        safeHeight
      )
      val resourcePolicy = new MarkdownPreviewImageResources.PreviewResourcePolicy(baseUri, cache.decodedImages)
      renderer.getSharedContext.setReplacedElementFactory(
        MarkdownPreviewImageResources.previewReplacedElementFactory(resourcePolicy)
      )
      renderer.getImage()
    catch
      case NonFatal(error) =>
        MarkdownPreviewImageResources.fallbackImage(safeWidth, safeHeight, theme, font, error.getMessage)

  def renderInlineLine(source: String): String =
    val trimmed = source.trim
    val Heading = """^(#{1,6})\s+(.+)$""".r
    trimmed match
      case Heading(_, text) =>
        MarkdownInlineNormalizer.normalizeInline(text).trim
      case text if text.startsWith(">") =>
        s"| ${MarkdownInlineNormalizer.normalizeInline(text.drop(1)).trim}"
      case text =>
        MarkdownInlineNormalizer.normalizeInline(text)

  def renderInlineLines(sourceLines: Vector[String], cache: MarkdownPreviewCache): Vector[String] =
    renderInlineDocument(sourceLines, cache).map(_.text)

  /** The first `rowCount` preview lines of `content` that are not blank -- what a docked preview panel shows. The
    * document's text is read only when this `Rope` has not been resolved at this height before.
    */
  def panelPreviewRows(content: Rope, rowCount: Int, cache: MarkdownPreviewCache): Vector[String] =
    cache.cachedPreviewRows(content, rowCount) {
      renderInlineLines(content.collect().linesIterator.toVector, cache).take(rowCount).filter(_.trim.nonEmpty)
    }

  def renderInlineDocument(sourceLines: Vector[String], cache: MarkdownPreviewCache): Vector[InlinePreviewLine] =
    inlinePreviewIndex(sourceLines, cache).previewLines

  private def inlinePreviewIndex(sourceLines: Vector[String], cache: MarkdownPreviewCache): InlinePreviewIndex =
    val key = MarkdownPreviewCache.InlineDocumentCacheKey(MarkdownPreviewCache.SourceLinesFingerprint.from(sourceLines))
    cache.cachedInlineDocument(key) {
      buildInlinePreviewIndex(sourceLines)
    }

  private def buildInlinePreviewIndex(sourceLines: Vector[String]): InlinePreviewIndex =
    InlinePreviewIndex(renderInlineDocumentUncached(sourceLines))

  private def renderInlineDocumentUncached(sourceLines: Vector[String]): Vector[InlinePreviewLine] =
    @annotation.tailrec
    def loop(index: Int, acc: Vector[InlinePreviewLine]): Vector[InlinePreviewLine] =
      if index >= sourceLines.length then acc
      else
        MarkdownInlineTablePreview.tableBlockAt(sourceLines, index) match
          case Some(tableBlock) =>
            loop(tableBlock.endIndex + 1, acc ++ tableBlock.previewLines)
          case None =>
            loop(index + 1, acc :+ InlinePreviewLine(Some(index), renderInlineLine(sourceLines(index))))

    loop(0, Vector.empty)

  private def renderXhtml(
    source: String,
    title: String,
    theme: Theme,
    font: Font,
    baseUri: Option[URI],
    panelChrome: Boolean,
    cache: MarkdownPreviewCache
  ): String =
    val fragment = renderHtmlFragment(source, title, cache, baseUri)
    MarkdownPreviewXhtml.renderXhtmlFragment(fragment, title, theme, font, panelChrome)

end MarkdownDocumentPreview
