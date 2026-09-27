package com.serenity.markdown

import java.awt.Font
import java.awt.image.BufferedImage
import java.net.URI

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

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

  private[serenity] def fontForDeviceScale(font: Font, deviceScale: Double): Font =
    font.deriveFont((font.getSize2D * deviceScale.max(1.0)).max(1.0).toFloat)

  private[serenity] def lineHeightForDeviceScale(lineHeightPx: Int, deviceScale: Double): Int =
    math.ceil(lineHeightPx.max(1).toDouble * deviceScale.max(1.0)).toInt.max(1)

  /** Sizes inline lens text to remain readable within an editor row before rendering at device scale. */
  private[serenity] def inlineLensFont(font: Font, lineHeightPx: Int, deviceScale: Double): Font =
    val readableFontSize = math.max(font.getSize2D.toDouble, lineHeightPx.max(1).toDouble * 0.8).toFloat
    fontForDeviceScale(font.deriveFont(readableFontSize), deviceScale)

  final private[markdown] case class InlinePreviewIndex(
      previewLines: Vector[InlinePreviewLine],
      rowsBySourceLine: Map[Int, Vector[Int]],
      tableLineIndexes: Set[Int]
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
    inlineLineHeightPx: Option[Int] = None,
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
      panelChrome = panelChrome,
      inlineLineHeightPx = inlineLineHeightPx,
      inlineRows = false
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
        inlineLineHeightPx,
        cache
      )
    }

  /** Renders the inline lens directly from its preview rows without reparsing them as Markdown. */
  private[serenity] def renderInlineImage(
    sourceLines: Vector[String],
    firstSourceLine: Int,
    maxSourceLines: Int,
    title: String,
    widthPx: Int,
    heightPx: Int,
    theme: Theme,
    font: Font,
    inlineLineHeightPx: Int,
    cache: MarkdownPreviewCache,
    reuseLastRenderWhileEditing: Boolean = false
  ): BufferedImage =
    val rows = inlinePreviewRows(sourceLines, firstSourceLine, maxSourceLines)
    renderInlineRowsImage(
      rows,
      sourceLines,
      title,
      widthPx,
      heightPx,
      theme,
      font,
      inlineLineHeightPx,
      cache,
      reuseLastRenderWhileEditing
    )

  /** Renders a caller-composed inline Lens row sequence. */
  private[serenity] def renderInlineRowsImage(
    rows: Vector[InlinePreviewLine],
    sourceLines: Vector[String],
    title: String,
    widthPx: Int,
    heightPx: Int,
    theme: Theme,
    font: Font,
    inlineLineHeightPx: Int,
    cache: MarkdownPreviewCache,
    reuseLastRenderWhileEditing: Boolean = false
  ): BufferedImage =
    val safeWidth  = widthPx.max(1)
    val safeHeight = heightPx.max(1)
    val key = MarkdownPreviewCache.ImageCacheKey(
      source = MarkdownPreviewCache.SourceFingerprint.from(
        rows.map(row => s"${row.sourceLine}:${row.text}").mkString("\u0000")
      ),
      title = title,
      widthPx = safeWidth,
      heightPx = safeHeight,
      theme = theme,
      font = font,
      baseUri = None,
      panelChrome = false,
      inlineLineHeightPx = Some(inlineLineHeightPx.max(1)),
      inlineRows = true
    )
    cache.cachedImage(key, reuseLastRenderWhileEditing) {
      renderInlineImageUncached(
        rows,
        sourceLines,
        title,
        safeWidth,
        safeHeight,
        theme,
        font,
        inlineLineHeightPx.max(1)
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
    inlineLineHeightPx: Option[Int],
    cache: MarkdownPreviewCache
  ): BufferedImage =
    try
      val renderer = Java2DRenderer(
        MarkdownPreviewImageResources.parseXhtml(
          renderXhtml(source, title, theme, font, baseUri, panelChrome, inlineLineHeightPx, cache)
        ),
        safeWidth,
        safeHeight
      )
      val resourcePolicy = new MarkdownPreviewImageResources.PreviewResourcePolicy(baseUri)
      renderer.getSharedContext.setReplacedElementFactory(
        MarkdownPreviewImageResources.previewReplacedElementFactory(resourcePolicy)
      )
      renderer.getImage()
    catch
      case NonFatal(error) =>
        MarkdownPreviewImageResources.fallbackImage(safeWidth, safeHeight, theme, font, error.getMessage)

  private def renderInlineImageUncached(
    rows: Vector[InlinePreviewLine],
    sourceLines: Vector[String],
    title: String,
    safeWidth: Int,
    safeHeight: Int,
    theme: Theme,
    font: Font,
    inlineLineHeightPx: Int
  ): BufferedImage =
    try
      val renderer = Java2DRenderer(
        MarkdownPreviewImageResources.parseXhtml(
          renderInlineXhtml(rows, sourceLines, title, theme, font, inlineLineHeightPx)
        ),
        safeWidth,
        safeHeight
      )
      val resourcePolicy = new MarkdownPreviewImageResources.PreviewResourcePolicy(None)
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

  def renderInlineDocument(sourceLines: Vector[String], cache: MarkdownPreviewCache): Vector[InlinePreviewLine] =
    inlinePreviewIndex(sourceLines, cache).previewLines

  def inlineTableLineIndexes(sourceLines: Vector[String], cache: MarkdownPreviewCache): Set[Int] =
    inlinePreviewIndex(sourceLines, cache).tableLineIndexes

  def renderInlineLineAt(sourceLines: Vector[String], index: Int, cache: MarkdownPreviewCache): String =
    inlinePreviewIndex(sourceLines, cache).rowsBySourceLine
      .get(index)
      .flatMap(_.headOption)
      .flatMap(renderInlineDocument(sourceLines, cache).lift)
      .map(_.text)
      .orElse(sourceLines.lift(index).map(renderInlineLine))
      .getOrElse("")

  def previewRowForSourceLine(sourceLines: Vector[String], sourceLine: Int, cache: MarkdownPreviewCache): Option[Int] =
    inlinePreviewIndex(sourceLines, cache).rowsBySourceLine.get(sourceLine).flatMap(_.headOption)

  def previewRowsForSourceRange(
    sourceLines: Vector[String],
    sourceRange: Range.Inclusive,
    cache: MarkdownPreviewCache
  ): Option[Range.Inclusive] =
    val preview = renderInlineDocument(sourceLines, cache)
    val mappedRows = sourceRange
      .flatMap(line => inlinePreviewIndex(sourceLines, cache).rowsBySourceLine.getOrElse(line, Vector.empty))
      .distinct
    mappedRows match
      case rows if rows.nonEmpty =>
        // rows.nonEmpty is checked by the guard above, so these folds always see a real row.
        val firstMapped = rows.foldLeft(Int.MaxValue)(_ min _)
        val lastMapped  = rows.foldLeft(Int.MinValue)(_ max _)
        val first =
          Iterator
            .iterate(firstMapped - 1)(_ - 1)
            .takeWhile(row => row >= 0 && preview(row).sourceLine.isEmpty)
            .toVector
            .lastOption
            .getOrElse(firstMapped)
        val last =
          Iterator
            .iterate(lastMapped + 1)(_ + 1)
            .takeWhile(row => row < preview.length && preview(row).sourceLine.isEmpty)
            .toVector
            .lastOption
            .getOrElse(lastMapped)
        Some(first to last)
      case _ =>
        None

  /** Produces the inline preview rows corresponding to a source window.
    *
    * The lens image and raw-source placement both use this row representation, including expanded table chrome.
    */
  private[serenity] def inlinePreviewRows(
    sourceLines: Vector[String],
    firstSourceLine: Int,
    maxSourceLines: Int
  ): Vector[InlinePreviewLine] =
    if sourceLines.isEmpty then Vector.empty
    else
      val requestedStart = firstSourceLine.max(0).min(sourceLines.length - 1)
      val maxWindowLines = maxSourceLines.max(1)
      val start          = inlinePreviewWindowStart(sourceLines, requestedStart, maxWindowLines)
      val end            = (requestedStart + maxWindowLines).min(sourceLines.length)
      renderInlineDocumentUncached(sourceLines.slice(start, end)).map { row =>
        row.copy(sourceLine = row.sourceLine.map(_ + start))
      }

  private def inlinePreviewWindowStart(sourceLines: Vector[String], start: Int, maxWindowLines: Int): Int =
    val earliestContextLine = (start - maxWindowLines.max(2)).max(0)
    val tableStart = Iterator
      .iterate(start)(_ - 1)
      .takeWhile(index => index >= earliestContextLine && MarkdownInlineTablePreview.isTableRow(sourceLines(index)))
      .toVector
      .lastOption
      .filter(index => index + 1 <= start && MarkdownInlineTablePreview.isTableSeparator(sourceLines(index + 1)))
    tableStart.getOrElse(start)

  private def inlinePreviewIndex(sourceLines: Vector[String], cache: MarkdownPreviewCache): InlinePreviewIndex =
    val key = MarkdownPreviewCache.InlineDocumentCacheKey(MarkdownPreviewCache.SourceLinesFingerprint.from(sourceLines))
    cache.cachedInlineDocument(key) {
      buildInlinePreviewIndex(sourceLines)
    }

  private def buildInlinePreviewIndex(sourceLines: Vector[String]): InlinePreviewIndex =
    val previewLines = renderInlineDocumentUncached(sourceLines)
    InlinePreviewIndex(
      previewLines = previewLines,
      rowsBySourceLine = previewLines.zipWithIndex
        .flatMap { case (line, row) => line.sourceLine.map(_ -> row) }
        .groupMap(_._1)(_._2),
      tableLineIndexes = tableLineIndexesUncached(sourceLines)
    )

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

  private def tableLineIndexesUncached(sourceLines: Vector[String]): Set[Int] =
    @annotation.tailrec
    def loop(index: Int, acc: Set[Int]): Set[Int] =
      if index >= sourceLines.length then acc
      else
        MarkdownInlineTablePreview.tableBlockAt(sourceLines, index) match
          case Some(tableBlock) =>
            loop(tableBlock.endIndex + 1, acc ++ (index to tableBlock.endIndex))
          case None =>
            loop(index + 1, acc)

    loop(0, Set.empty)

  def previewWindow(
    sourceLines: Vector[String],
    activeLine: Option[Int],
    fallbackTopLine: Int,
    cache: MarkdownPreviewCache,
    maxSourceLines: Int = Int.MaxValue
  ): PreviewWindow =
    if sourceLines.isEmpty then PreviewWindow(0, 0, "")
    else
      val anchorLine = activeLine
        .filter(line => line >= 0 && line < sourceLines.length)
        .getOrElse(fallbackTopLine.max(0).min(sourceLines.length - 1))
      val blockRange      = MarkdownBlockLens.currentBlock(sourceLines, anchorLine)
      val firstSourceLine = blockRange.start.max(0).min(sourceLines.length - 1)
      val firstPreviewRow = previewRowsForSourceRange(sourceLines, blockRange, cache)
        .map(_.start)
        .orElse(previewRowForSourceLine(sourceLines, firstSourceLine, cache))
        .getOrElse(firstSourceLine)
      PreviewWindow(
        firstSourceLine = firstSourceLine,
        firstPreviewRow = firstPreviewRow,
        source = previewSource(sourceLines, firstSourceLine, maxSourceLines)
      )

  def splitPreviewWindow(
    sourceLines: Vector[String],
    activeLine: Option[Int],
    fallbackTopLine: Int,
    cache: MarkdownPreviewCache,
    maxSourceLines: Int = Int.MaxValue
  ): PreviewWindow =
    if sourceLines.isEmpty then PreviewWindow(0, 0, "")
    else
      val safeMax       = maxSourceLines.max(1)
      val maxStart      = (sourceLines.length - safeMax).max(0)
      val fallbackStart = fallbackTopLine.max(0).min(maxStart)
      val anchorLine = activeLine
        .filter(line => line >= 0 && line < sourceLines.length)
        .getOrElse(fallbackTopLine.max(0).min(sourceLines.length - 1))
      val endExclusive = fallbackStart + safeMax
      val firstSourceLine =
        if anchorLine < fallbackStart then anchorLine.min(maxStart)
        else if anchorLine >= endExclusive then (anchorLine - safeMax / 2).max(0).min(maxStart)
        else fallbackStart
      val firstPreviewRow = previewRowForSourceLine(sourceLines, firstSourceLine, cache).getOrElse(firstSourceLine)
      PreviewWindow(
        firstSourceLine = firstSourceLine,
        firstPreviewRow = firstPreviewRow,
        source = previewSource(sourceLines, firstSourceLine, safeMax)
      )

  private def renderXhtml(
    source: String,
    title: String,
    theme: Theme,
    font: Font,
    baseUri: Option[URI],
    panelChrome: Boolean,
    inlineLineHeightPx: Option[Int],
    cache: MarkdownPreviewCache
  ): String =
    val fragment = renderHtmlFragment(source, title, cache, baseUri)
    MarkdownPreviewXhtml.renderXhtmlFragment(fragment, title, theme, font, panelChrome, inlineLineHeightPx)

  private[serenity] def renderInlineXhtml(
    rows: Vector[InlinePreviewLine],
    sourceLines: Vector[String],
    title: String,
    theme: Theme,
    font: Font,
    inlineLineHeightPx: Int
  ): String =
    val fragment =
      s"<div class=\"inline-rows\">${rows.map(MarkdownPreviewXhtml.inlineRowHtml(_, sourceLines)).mkString}</div>"
    MarkdownPreviewXhtml.renderXhtmlFragment(
      fragment,
      title,
      theme,
      font,
      panelChrome = false,
      Some(inlineLineHeightPx)
    )

  private def previewSource(sourceLines: Vector[String], firstSourceLine: Int, maxSourceLines: Int): String =
    val safeMax = maxSourceLines.max(1)
    val endLine = firstSourceLine + math.min(safeMax, sourceLines.length - firstSourceLine)
    sourceLines.slice(firstSourceLine, endLine).mkString("\n")

end MarkdownDocumentPreview
