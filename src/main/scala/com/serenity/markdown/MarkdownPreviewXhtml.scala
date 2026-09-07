package com.serenity.markdown

import java.awt.{Color, Font}
import java.net.URI

import scala.util.Try

import com.serenity.markdown.MarkdownDocumentPreview.InlinePreviewLine
import com.serenity.ui.theme.{ColorFormat, Theme}
import org.commonmark.Extension
import org.commonmark.node.Image
import org.commonmark.renderer.html.*

/** Assembles the XHTML document (and the CommonMark `HtmlRenderer` that resolves relative image sources within it)
  * that [[MarkdownDocumentPreview]] hands to flying-saucer for layout, including the themed stylesheet shared by the
  * panel preview and the inline Lens.
  */
private[markdown] object MarkdownPreviewXhtml:

  def htmlRenderer(extensions: java.util.List[Extension], defaultRenderer: HtmlRenderer, baseUri: Option[URI])
    : HtmlRenderer =
    baseUri match
      case None => defaultRenderer
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

  def renderXhtmlFragment(
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

  def inlineRowHtml(row: InlinePreviewLine, sourceLines: Vector[String]): String =
    val headingClass = row.sourceLine
      .flatMap(sourceLines.lift)
      .filter(_.matches("^\\s*#{1,6}\\s+.+$"))
      .fold("")(_ => " inline-heading")
    s"<div class=\"inline-row$headingClass\">${escapeXml(row.text)}</div>"

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
         |        margin: 0;
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

  private def css(color: Color): String =
    ColorFormat.toHex(color, withAlpha = false)

  private def cssString(value: String): String =
    "'" + value.replace("\\", "\\\\").replace("'", "\\'") + "'"

  private def escapeXml(value: String): String =
    value
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&apos;")
