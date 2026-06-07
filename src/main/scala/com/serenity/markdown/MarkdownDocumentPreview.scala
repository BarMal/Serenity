package com.serenity.markdown

import java.awt.image.BufferedImage
import java.awt.{Color, Font, RenderingHints}
import java.io.StringReader
import java.net.URI
import javax.xml.parsers.DocumentBuilderFactory

import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.control.NonFatal

import org.commonmark.Extension
import org.commonmark.ext.gfm.tables.TablesExtension
import org.commonmark.ext.task.list.items.TaskListItemsExtension
import org.commonmark.node.Image
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.*
import org.w3c.dom.Document
import org.xhtmlrenderer.swing.Java2DRenderer
import org.xml.sax.InputSource

import com.serenity.ui.theme.Theme

object MarkdownDocumentPreview:

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
    htmlRenderer(baseUri).render(parser.parse(source))

  def renderImage(
    source: String,
    title: String,
    widthPx: Int,
    heightPx: Int,
    theme: Theme,
    font: Font,
    baseUri: Option[URI] = None
  ): BufferedImage =
    val safeWidth  = widthPx.max(1)
    val safeHeight = heightPx.max(1)
    try
      val renderer = Java2DRenderer(
        parseXhtml(renderXhtml(source, title, theme, font, baseUri)),
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

  private def renderXhtml(source: String, title: String, theme: Theme, font: Font, baseUri: Option[URI]): String =
    val fragment = renderHtmlFragment(source, title, baseUri)
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<html xmlns="http://www.w3.org/1999/xhtml">
       |  <head>
       |    <title>${escapeXml(title)}</title>
       |    <style type="text/css">
       |${stylesheet(theme, font)}
       |    </style>
       |  </head>
       |  <body>
       |    <div class="markdown-body">
       |$fragment
       |    </div>
       |  </body>
       |</html>""".stripMargin

  private def stylesheet(theme: Theme, font: Font): String =
    s"""      html, body {
       |        margin: 0;
       |        padding: 0;
       |        background: ${css(theme.panel.background)};
       |        color: ${css(theme.panel.foreground)};
       |        font-family: ${cssString(font.getFamily)}, sans-serif;
       |        font-size: ${font.getSize.max(10)}px;
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
