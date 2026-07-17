package com.serenity.markdown

import java.awt.image.BufferedImage
import java.awt.{Color, Font, RenderingHints}
import java.io.StringReader
import java.net.URI
import java.util.LinkedHashMap
import javax.xml.parsers.DocumentBuilderFactory

import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.control.NonFatal
import scala.util.hashing.MurmurHash3

import com.serenity.ui.theme.Theme
import org.commonmark.Extension
import org.commonmark.ext.gfm.tables.TablesExtension
import org.commonmark.ext.task.list.items.TaskListItemsExtension
import org.commonmark.node.Image
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.*
import org.w3c.dom.Document
import org.xhtmlrenderer.swing.Java2DRenderer
import org.xml.sax.InputSource

object MarkdownDocumentPreview:

  case class InlinePreviewLine(sourceLine: Option[Int], text: String)

  case class PreviewWindow(firstSourceLine: Int, firstPreviewRow: Int, source: String)

  /** Scales preview typography to match a device-scaled preview image. */
  private[serenity] def fontForDeviceScale(font: Font, deviceScale: Double): Font =
    font.deriveFont((font.getSize2D * deviceScale.max(1.0)).max(1.0).toFloat)

  /** Converts an editor line height into the corresponding preview-image pixel height. */
  private[serenity] def lineHeightForDeviceScale(lineHeightPx: Int, deviceScale: Double): Int =
    math.ceil(lineHeightPx.max(1).toDouble * deviceScale.max(1.0)).toInt.max(1)

  /** Sizes inline lens text to remain readable within an editor row before rendering at device scale. */
  private[serenity] def inlineLensFont(font: Font, lineHeightPx: Int, deviceScale: Double): Font =
    val readableFontSize = math.max(font.getSize2D.toDouble, lineHeightPx.max(1).toDouble * 0.8).toFloat
    fontForDeviceScale(font.deriveFont(readableFontSize), deviceScale)

  private val MaxCachedImages          = 24
  private val MaxCachedHtmlFragments   = 48
  private val MaxCachedInlineDocuments = 32

  private case class SourceFingerprint(length: Int, hash: Int)

  private object SourceFingerprint:
    def from(source: String): SourceFingerprint =
      SourceFingerprint(source.length, MurmurHash3.stringHash(source))

  private case class ImageCacheKey(
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

  private case class HtmlFragmentCacheKey(source: SourceFingerprint, title: String, baseUri: Option[String])

  private case class SourceLinesFingerprint(lineCount: Int, totalLength: Int, hash: Int)

  private object SourceLinesFingerprint:

    def from(sourceLines: Vector[String]): SourceLinesFingerprint =
      SourceLinesFingerprint(
        lineCount = sourceLines.length,
        totalLength = sourceLines.map(_.length).sum,
        hash = MurmurHash3.orderedHash(sourceLines)
      )

  private case class InlineDocumentCacheKey(source: SourceLinesFingerprint)

  private case class InlinePreviewIndex(
      previewLines: Vector[InlinePreviewLine],
      rowsBySourceLine: Map[Int, Vector[Int]],
      tableLineIndexes: Set[Int]
  )

  private val imageCache =
    new LinkedHashMap[ImageCacheKey, BufferedImage](MaxCachedImages, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[ImageCacheKey, BufferedImage]): Boolean =
        size() > MaxCachedImages

  private val htmlFragmentCache =
    new LinkedHashMap[HtmlFragmentCacheKey, String](MaxCachedHtmlFragments, 0.75f, true):
      override def removeEldestEntry(eldest: java.util.Map.Entry[HtmlFragmentCacheKey, String]): Boolean =
        size() > MaxCachedHtmlFragments

  private val inlineDocumentCache =
    new LinkedHashMap[InlineDocumentCacheKey, InlinePreviewIndex](MaxCachedInlineDocuments, 0.75f, true):
      override def removeEldestEntry(
        eldest: java.util.Map.Entry[InlineDocumentCacheKey, InlinePreviewIndex]
      ): Boolean =
        size() > MaxCachedInlineDocuments

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

  def renderHtmlFragment(source: String, title: String, baseUri: Option[URI] = None): String =
    val key = HtmlFragmentCacheKey(SourceFingerprint.from(source), title, baseUri.map(_.toString))
    htmlFragmentCache
      .synchronized {
        Option(htmlFragmentCache.get(key))
      }
      .getOrElse {
        val rendered = htmlRenderer(baseUri).render(parser.parse(source))
        htmlFragmentCache.synchronized {
          val _ = htmlFragmentCache.put(key, rendered)
        }
        rendered
      }

  def renderImage(
    source: String,
    title: String,
    widthPx: Int,
    heightPx: Int,
    theme: Theme,
    font: Font,
    baseUri: Option[URI] = None,
    panelChrome: Boolean = true,
    inlineLineHeightPx: Option[Int] = None
  ): BufferedImage =
    val safeWidth  = widthPx.max(1)
    val safeHeight = heightPx.max(1)
    val key = ImageCacheKey(
      source = SourceFingerprint.from(source),
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
    imageCache
      .synchronized {
        Option(imageCache.get(key))
      }
      .getOrElse {
        val rendered = renderImageUncached(
          source,
          title,
          safeWidth,
          safeHeight,
          theme,
          font,
          baseUri,
          panelChrome,
          inlineLineHeightPx
        )
        imageCache.synchronized {
          val _ = imageCache.put(key, rendered)
        }
        rendered
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
    inlineLineHeightPx: Int
  ): BufferedImage =
    val rows       = inlinePreviewRows(sourceLines, firstSourceLine, maxSourceLines)
    val safeWidth  = widthPx.max(1)
    val safeHeight = heightPx.max(1)
    val key = ImageCacheKey(
      source = SourceFingerprint.from(rows.map(row => s"${row.sourceLine}:${row.text}").mkString("\u0000")),
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
    imageCache
      .synchronized {
        Option(imageCache.get(key))
      }
      .getOrElse {
        val rendered = renderInlineImageUncached(
          rows,
          sourceLines,
          title,
          safeWidth,
          safeHeight,
          theme,
          font,
          inlineLineHeightPx.max(1)
        )
        imageCache.synchronized {
          val _ = imageCache.put(key, rendered)
        }
        rendered
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
    inlineLineHeightPx: Option[Int]
  ): BufferedImage =
    try
      val renderer = Java2DRenderer(
        parseXhtml(renderXhtml(source, title, theme, font, baseUri, panelChrome, inlineLineHeightPx)),
        safeWidth,
        safeHeight
      )
      renderer.getImage()
    catch
      case NonFatal(error) =>
        fallbackImage(safeWidth, safeHeight, theme, font, error.getMessage)

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
        parseXhtml(renderInlineXhtml(rows, sourceLines, title, theme, font, inlineLineHeightPx)),
        safeWidth,
        safeHeight
      )
      renderer.getImage()
    catch
      case NonFatal(error) =>
        fallbackImage(safeWidth, safeHeight, theme, font, error.getMessage)

  def renderInlineLine(source: String): String =
    val trimmed = source.trim
    val Heading = """^(#{1,6})\s+(.+)$""".r
    trimmed match
      case Heading(_, text) =>
        normalizeInline(text).trim
      case text if text.startsWith(">") =>
        s"| ${normalizeInline(text.drop(1)).trim}"
      case text =>
        normalizeInline(text)

  def renderInlineLines(sourceLines: Vector[String]): Vector[String] =
    renderInlineDocument(sourceLines).map(_.text)

  def renderInlineDocument(sourceLines: Vector[String]): Vector[InlinePreviewLine] =
    inlinePreviewIndex(sourceLines).previewLines

  def inlineTableLineIndexes(sourceLines: Vector[String]): Set[Int] =
    inlinePreviewIndex(sourceLines).tableLineIndexes

  def renderInlineLineAt(sourceLines: Vector[String], index: Int): String =
    inlinePreviewIndex(sourceLines).rowsBySourceLine
      .get(index)
      .flatMap(_.headOption)
      .flatMap(renderInlineDocument(sourceLines).lift)
      .map(_.text)
      .orElse(sourceLines.lift(index).map(renderInlineLine))
      .getOrElse("")

  def previewRowForSourceLine(sourceLines: Vector[String], sourceLine: Int): Option[Int] =
    inlinePreviewIndex(sourceLines).rowsBySourceLine.get(sourceLine).flatMap(_.headOption)

  def previewRowsForSourceRange(sourceLines: Vector[String], sourceRange: Range.Inclusive): Option[Range.Inclusive] =
    val preview = renderInlineDocument(sourceLines)
    val mappedRows = sourceRange
      .flatMap(line => inlinePreviewIndex(sourceLines).rowsBySourceLine.getOrElse(line, Vector.empty))
      .distinct
    mappedRows match
      case rows if rows.nonEmpty =>
        val firstMapped = rows.min
        val lastMapped  = rows.max
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
      .takeWhile(index => index >= earliestContextLine && isTableRow(sourceLines(index)))
      .toVector
      .lastOption
      .filter(index => index + 1 <= start && isTableSeparator(sourceLines(index + 1)))
    tableStart.getOrElse(start)

  private def inlinePreviewIndex(sourceLines: Vector[String]): InlinePreviewIndex =
    val key = InlineDocumentCacheKey(SourceLinesFingerprint.from(sourceLines))
    inlineDocumentCache
      .synchronized {
        Option(inlineDocumentCache.get(key))
      }
      .getOrElse {
        val index = buildInlinePreviewIndex(sourceLines)
        inlineDocumentCache.synchronized {
          val _ = inlineDocumentCache.put(key, index)
        }
        index
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
        tableBlockAt(sourceLines, index) match
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
        tableBlockAt(sourceLines, index) match
          case Some(tableBlock) =>
            loop(tableBlock.endIndex + 1, acc ++ (index to tableBlock.endIndex))
          case None =>
            loop(index + 1, acc)

    loop(0, Set.empty)

  def previewWindow(
    sourceLines: Vector[String],
    activeLine: Option[Int],
    fallbackTopLine: Int,
    maxSourceLines: Int = Int.MaxValue
  ): PreviewWindow =
    if sourceLines.isEmpty then PreviewWindow(0, 0, "")
    else
      val anchorLine = activeLine
        .filter(line => line >= 0 && line < sourceLines.length)
        .getOrElse(fallbackTopLine.max(0).min(sourceLines.length - 1))
      val blockRange      = MarkdownBlockLens.currentBlock(sourceLines, anchorLine)
      val firstSourceLine = blockRange.start.max(0).min(sourceLines.length - 1)
      val firstPreviewRow = previewRowsForSourceRange(sourceLines, blockRange)
        .map(_.start)
        .orElse(previewRowForSourceLine(sourceLines, firstSourceLine))
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
      val firstPreviewRow = previewRowForSourceLine(sourceLines, firstSourceLine).getOrElse(firstSourceLine)
      PreviewWindow(
        firstSourceLine = firstSourceLine,
        firstPreviewRow = firstPreviewRow,
        source = previewSource(sourceLines, firstSourceLine, safeMax)
      )

  private def htmlRenderer(baseUri: Option[URI]): HtmlRenderer =
    baseUri match
      case None => defaultHtmlRenderer
      case Some(uri) =>
        HtmlRenderer
          .builder()
          .extensions(extensions)
          .escapeHtml(true)
          .attributeProviderFactory(relativeImageProvider(uri))
          .build()

  private def relativeImageProvider(baseUri: URI): AttributeProviderFactory =
    new AttributeProviderFactory:
      override def create(context: AttributeProviderContext): AttributeProvider =
        new AttributeProvider:
          override def setAttributes(
            node: org.commonmark.node.Node,
            tagName: String,
            attributes: java.util.Map[String, String]
          ): Unit =
            node match
              case image: Image =>
                Option(image.getDestination)
                  .filterNot(isAbsoluteUri)
                  .map(baseUri.resolve)
                  .foreach(uri => attributes.put("src", uri.toString))
              case _ => ()

  private def isAbsoluteUri(value: String): Boolean =
    Try(URI.create(value).isAbsolute).getOrElse(false)

  private def renderXhtml(
    source: String,
    title: String,
    theme: Theme,
    font: Font,
    baseUri: Option[URI],
    panelChrome: Boolean,
    inlineLineHeightPx: Option[Int]
  ): String =
    val fragment = renderHtmlFragment(source, title, baseUri)
    renderXhtmlFragment(fragment, title, theme, font, panelChrome, inlineLineHeightPx)

  private def renderInlineXhtml(
    rows: Vector[InlinePreviewLine],
    sourceLines: Vector[String],
    title: String,
    theme: Theme,
    font: Font,
    inlineLineHeightPx: Int
  ): String =
    val fragment =
      s"<table class=\"inline-rows\"><tbody>${rows.map(inlineRowHtml(_, sourceLines)).mkString}</tbody></table>"
    renderXhtmlFragment(fragment, title, theme, font, panelChrome = false, Some(inlineLineHeightPx))

  private def renderXhtmlFragment(
    fragment: String,
    title: String,
    theme: Theme,
    font: Font,
    panelChrome: Boolean,
    inlineLineHeightPx: Option[Int]
  ): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<html xmlns="http://www.w3.org/1999/xhtml">
       |  <head>
       |    <title>${escapeXml(title)}</title>
       |    <style type="text/css">
|${stylesheet(theme, font, panelChrome, inlineLineHeightPx)}
       |    </style>
       |  </head>
       |  <body>
       |    <div class="markdown-body">
       |$fragment
       |    </div>
       |  </body>
       |</html>""".stripMargin

  private def inlineRowHtml(row: InlinePreviewLine, sourceLines: Vector[String]): String =
    val headingClass = row.sourceLine
      .flatMap(sourceLines.lift)
      .filter(_.matches("^\\s*#{1,6}\\s+.+$"))
      .fold("")(_ => " inline-heading")
    s"<tr><td class=\"inline-row$headingClass\">${escapeXml(row.text)}</td></tr>"

  private def stylesheet(
    theme: Theme,
    font: Font,
    panelChrome: Boolean,
    inlineLineHeightPx: Option[Int]
  ): String =
    val background = if panelChrome then theme.panel.background else theme.background
    val foreground = if panelChrome then theme.panel.foreground else theme.foreground
    val inlineLensOverrides = inlineLineHeightPx.fold("") { lineHeight =>
      s"""      html, body { line-height: ${lineHeight.max(1)}px; }
         |      .markdown-body { padding: 0; }
         |      h1, h2, h3, h4, h5, h6 {
         |        font-size: 1em;
         |        line-height: ${lineHeight.max(1)}px;
         |        margin: 0;
         |        border-bottom: 0;
         |        padding-bottom: 0;
         |      }
         |      p, blockquote, pre, table, ul, ol { margin: 0; }
         |      li { margin: 0; }
         |      .inline-rows {
         |        border: 0;
         |        border-collapse: collapse;
         |        margin: 0;
         |        width: 100%;
         |      }
         |      .inline-rows .inline-row {
         |        border: 0;
         |        height: ${lineHeight.max(1)}px;
         |        line-height: ${lineHeight.max(1)}px;
         |        margin: 0;
         |        padding: 0;
         |        white-space: pre;
         |      }
         |      .inline-heading { font-weight: 700; }
         |""".stripMargin
    }
    s"""      html, body {
       |        margin: 0;
       |        padding: 0;
       |        width: 100%;
       |        height: 100%;
       |        background: ${css(background)};
       |        color: ${css(foreground)};
       |        font-family: ${cssString(font.getFamily)}, sans-serif;
       |        font-size: ${font.getSize2D.max(10.0f)}px;
       |        line-height: 1.45;
       |      }
       |      .markdown-body {
       |        box-sizing: border-box;
       |        padding: 14px 16px 18px 16px;
       |      }
       |      h1, h2, h3, h4, h5, h6 {
       |        color: ${css(theme.foreground)};
       |        font-weight: 700;
       |        line-height: 1.2;
       |        margin: 0.85em 0 0.35em 0;
       |      }
       |      h1 { font-size: 1.8em; border-bottom: 1px solid ${css(theme.border)}; padding-bottom: 0.24em; }
       |      h2 { font-size: 1.45em; border-bottom: 1px solid ${css(theme.border)}; padding-bottom: 0.2em; }
       |      h3 { font-size: 1.2em; }
       |      p { margin: 0.55em 0; }
       |      a { color: ${css(theme.highlighted.foreground)}; text-decoration: underline; }
       |      code {
       |        font-family: ${cssString(Font.MONOSPACED)}, monospace;
       |        background: ${css(theme.background)};
       |        border: 1px solid ${css(theme.border)};
       |        padding: 1px 4px;
       |      }
       |      pre {
       |        background: ${css(theme.background)};
       |        border: 1px solid ${css(theme.border)};
       |        padding: 10px;
       |        overflow: hidden;
       |      }
       |      pre code { border: 0; padding: 0; }
       |      blockquote {
       |        border-left: 4px solid ${css(theme.border)};
       |        color: ${css(theme.muted)};
       |        margin: 0.7em 0;
       |        padding-left: 0.9em;
       |      }
       |      table {
       |        border-collapse: collapse;
       |        margin: 0.8em 0;
       |        width: 100%;
       |      }
       |      th, td {
       |        border: 1px solid ${css(theme.border)};
       |        padding: 6px 8px;
       |        text-align: left;
       |      }
       |      th { background: ${css(theme.background)}; color: ${css(theme.foreground)}; }
       |      img {
       |        max-width: 100%;
       |        height: auto;
       |        border: 1px solid ${css(theme.border)};
       |      }
       |      ul, ol { padding-left: 1.5em; }
       |      li { margin: 0.25em 0; }
       |$inlineLensOverrides
       |""".stripMargin

  private def parseXhtml(xhtml: String): Document =
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(true)
    factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    factory.newDocumentBuilder().parse(InputSource(StringReader(xhtml)))

  private def fallbackImage(width: Int, height: Int, theme: Theme, font: Font, message: String): BufferedImage =
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g     = image.createGraphics()
    try
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      g.setColor(theme.panel.background)
      g.fillRect(0, 0, width, height)
      g.setColor(theme.error.foreground)
      g.setFont(font)
      g.drawString("Markdown preview failed", 16, 28)
      g.setColor(theme.panel.foreground)
      Option(message).foreach(text => g.drawString(text.take(120), 16, 50))
    finally g.dispose()
    image

  private def normalizeInline(text: String): String =
    val withoutImages = """!\[([^\]]*)\]\(([^)]+)\)""".r.replaceAllIn(
      text,
      matched =>
        val label = Option(matched.group(1).trim).filter(_.nonEmpty).getOrElse("Image")
        s"Image: $label (${matched.group(2)})"
    )
    val withoutLinks = """\[([^\]]+)\]\(([^)]+)\)""".r.replaceAllIn(
      withoutImages,
      matched => s"${matched.group(1)} (${matched.group(2)})"
    )
    "`([^`]+)`".r.replaceAllIn(withoutLinks, matched => matched.group(1))

  private case class InlineTableBlock(endIndex: Int, previewLines: Vector[InlinePreviewLine])

  private def previewSource(sourceLines: Vector[String], firstSourceLine: Int, maxSourceLines: Int): String =
    val safeMax = maxSourceLines.max(1)
    val endLine = firstSourceLine + math.min(safeMax, sourceLines.length - firstSourceLine)
    sourceLines.slice(firstSourceLine, endLine).mkString("\n")

  private def tableBlockAt(lines: Vector[String], index: Int): Option[InlineTableBlock] =
    Option
      .when(index + 1 < lines.length && isTableRow(lines(index)) && isTableSeparator(lines(index + 1))) {
        val rows = Iterator
          .iterate(index)(_ + 1)
          .takeWhile(lineIndex => lineIndex < lines.length && isTableRow(lines(lineIndex)))
          .toVector
        val endIndex     = rows.last
        val renderedRows = renderInlineTable(rows.map(lines))
        InlineTableBlock(endIndex, sourceMappedTableRows(rows, renderedRows))
      }
      .filter(_.previewLines.nonEmpty)

  private def sourceMappedTableRows(sourceRows: Vector[Int], renderedRows: Vector[String]): Vector[InlinePreviewLine] =
    renderedRows.zipWithIndex.map {
      case (text, 0) =>
        InlinePreviewLine(None, text)
      case (text, 1) =>
        InlinePreviewLine(sourceRows.headOption, text)
      case (text, 2) =>
        InlinePreviewLine(sourceRows.lift(1), text)
      case (text, rowIndex) if rowIndex == renderedRows.length - 1 =>
        InlinePreviewLine(None, text)
      case (text, rowIndex) =>
        InlinePreviewLine(sourceRows.lift(rowIndex - 1), text)
    }

  private def renderInlineTable(lines: Vector[String]): Vector[String] =
    val parsedRows = lines.map(parseTableCells)
    val contentRows =
      parsedRows.zipWithIndex.collect {
        case (cells, index) if index != 1 => cells.map(normalizeInline)
      }
    val columnCount = contentRows.map(_.length).maxOption.getOrElse(0)
    if columnCount == 0 then Vector.empty
    else
      val widths = (0 until columnCount).map { column =>
        contentRows.flatMap(_.lift(column)).map(_.length).maxOption.getOrElse(0)
      }.toVector

      contentRows.zipWithIndex.flatMap {
        case (cells, 0) =>
          Vector(tableBorder(widths, "\u250c", "\u252c", "\u2510"), boxedTableRow(cells, widths))
        case (cells, _) =>
          Vector(boxedTableRow(cells, widths))
      } match
        case rows if rows.nonEmpty =>
          rows.take(2) ++
            Vector(tableBorder(widths, "\u251c", "\u253c", "\u2524")) ++
            rows.drop(2) ++
            Vector(tableBorder(widths, "\u2514", "\u2534", "\u2518"))
        case _ =>
          Vector.empty

  private def boxedTableRow(cells: Vector[String], widths: Vector[Int]): String =
    widths.zipWithIndex
      .map { case (width, index) => s" ${cells.lift(index).getOrElse("").padTo(width, ' ')} " }
      .mkString("\u2502", "\u2502", "\u2502")

  private def tableBorder(widths: Vector[Int], left: String, separator: String, right: String): String =
    widths
      .map(width => "\u2500" * (width + 2).max(3))
      .mkString(left, separator, right)

  private def parseTableCells(line: String): Vector[String] =
    val trimmed           = line.trim
    val withoutOuterPipes = trimmed.stripPrefix("|").stripSuffix("|")
    withoutOuterPipes.split("\\|", -1).toVector.map(_.trim)

  private def isTableRow(line: String): Boolean =
    val trimmed = line.trim
    trimmed.contains("|") && parseTableCells(trimmed).length >= 2

  private def isTableSeparator(line: String): Boolean =
    isTableRow(line) && parseTableCells(line).forall(cell => cell.matches(""":?-{3,}:?"""))

  private def css(color: Color): String =
    f"#${color.getRed}%02x${color.getGreen}%02x${color.getBlue}%02x"

  private def cssString(value: String): String =
    "'" + value.replace("\\", "\\\\").replace("'", "\\'") + "'"

  private def escapeXml(value: String): String =
    value
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&apos;")

end MarkdownDocumentPreview
