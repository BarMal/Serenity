package com.serenity.richtext

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.util.control.NonFatal

import cats.effect.IO
import com.serenity.io.AtomicFileWriter
import org.w3c.dom.{Document as XmlDocument, Element, Node}

/** Reads and writes OpenDocument Text files through Serenity's native rich text model. */
object OdtDocumentCodec:
  private val OfficeNs                = "urn:oasis:names:tc:opendocument:xmlns:office:1.0"
  private val StyleNs                 = "urn:oasis:names:tc:opendocument:xmlns:style:1.0"
  private val TextNs                  = "urn:oasis:names:tc:opendocument:xmlns:text:1.0"
  private val FoNs                    = "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"
  private val SupportedArchiveEntries = Set("mimetype", "META-INF/manifest.xml", "content.xml")

  private val SupportedElements = Set(
    "document-content",
    "automatic-styles",
    "style",
    "text-properties",
    "paragraph-properties",
    "body",
    "text",
    "p",
    "h",
    "span",
    "s",
    "tab",
    "line-break"
  )

  final private case class OdtStyles(
      textStyles: Map[String, RichTextStyle],
      paragraphStyles: Map[String, ParagraphAlignment]
  )

  /** Read an ODT file into Serenity's native rich text model. */
  def read(path: Path): IO[RichTextDocument] =
    IO.blocking(readBytes(RichTextArchive.readFile(path, "ODT")))

  /** Read an ODT file and report structures that the native model cannot round-trip. */
  def readWithFidelity(path: Path): IO[RichTextImport] =
    IO.blocking(readBytesWithFidelity(RichTextArchive.readFile(path, "ODT")))

  /** Write Serenity's native rich text model to an ODT file. */
  def write(document: RichTextDocument, path: Path): IO[Unit] =
    AtomicFileWriter.writeBytes(path, writeBytes(document))

  /** Decode ODT bytes into Serenity's native rich text model. */
  def readBytes(bytes: Array[Byte]): RichTextDocument =
    try
      val content = RichTextArchive.zipEntry(bytes, "content.xml", "ODT").getOrElse {
        throw RichTextCodecException("ODT archive is missing content.xml")
      }
      val xml    = parseXml(content)
      val styles = stylesFromDocument(xml)
      val paragraphs = firstElement(xml.getElementsByTagNameNS(OfficeNs, "text"))
        .map(textElement =>
          childElements(textElement)
            .filter(element => element.getNamespaceURI == TextNs && Set("p", "h").contains(element.getLocalName))
            .map(paragraphFromElement(_, styles))
        )
        .getOrElse(Nil)

      RichTextDocument(
        if paragraphs.nonEmpty then paragraphs
        else List(RichTextParagraph.plain(""))
      ).normalized
    catch
      case error: RichTextCodecException => throw error
      case NonFatal(error)               => throw RichTextCodecException("ODT document could not be decoded", error)

  /** Decode ODT bytes and report structures that the native model cannot round-trip. */
  def readBytesWithFidelity(bytes: Array[Byte]): RichTextImport =
    val document = readBytes(bytes)
    val content  = RichTextArchive.zipEntry(bytes, "content.xml", "ODT").getOrElse(Array.emptyByteArray)
    val xml      = parseXml(content)
    val unsupportedElements =
      (0 until xml.getElementsByTagName("*").getLength)
        .map(xml.getElementsByTagName("*").item)
        .collect { case element: Element => element.getLocalName }
        .filterNot(SupportedElements.contains)
        .toSet
    val unsupportedEntries = RichTextArchive.entryNames(bytes, "ODT") -- SupportedArchiveEntries
    RichTextImport(document, RichTextFidelity(unsupportedElements, unsupportedEntries))

  /** Encode Serenity's native rich text model as ODT bytes. */
  def writeBytes(document: RichTextDocument): Array[Byte] =
    val output = ByteArrayOutputStream()
    val zip    = ZipOutputStream(output, StandardCharsets.UTF_8)
    try
      writeZipEntry(zip, "mimetype", "application/vnd.oasis.opendocument.text")
      writeZipEntry(zip, "META-INF/manifest.xml", manifestXml)
      writeZipEntry(zip, "content.xml", contentXml(document.normalized))
    finally zip.close()
    output.toByteArray

  private def parseXml(bytes: Array[Byte]): XmlDocument =
    RichTextXmlParser.parse(bytes)

  private def stylesFromDocument(document: XmlDocument): OdtStyles =
    val styleElements = elements(document.getElementsByTagNameNS(StyleNs, "style"))
    OdtStyles(
      textStyles = styleElements.flatMap(textStyleFromElement).toMap,
      paragraphStyles = styleElements.flatMap(paragraphStyleFromElement).toMap
    )

  private def textStyleFromElement(element: Element): Option[(String, RichTextStyle)] =
    Option
      .when(attribute(element, StyleNs, "family").contains("text")) {
        val style = childElement(element, StyleNs, "text-properties")
          .map(textStyleFromProperties)
          .getOrElse(RichTextStyle.empty)
        attribute(element, StyleNs, "name").map(_ -> style)
      }
      .flatten

  private def paragraphStyleFromElement(element: Element): Option[(String, ParagraphAlignment)] =
    Option
      .when(attribute(element, StyleNs, "family").contains("paragraph")) {
        val alignment = childElement(element, StyleNs, "paragraph-properties")
          .flatMap(paragraphAlignmentFromProperties)
          .getOrElse(ParagraphAlignment.Left)
        attribute(element, StyleNs, "name").map(_ -> alignment)
      }
      .flatten

  private def textStyleFromProperties(element: Element): RichTextStyle =
    RichTextStyle(
      marks = List(
        Option.when(isBold(element))(InlineMark.Bold),
        Option.when(attribute(element, FoNs, "font-style").contains("italic"))(InlineMark.Italic),
        Option.when(isUnderlined(element))(InlineMark.Underline)
      ).flatten.toSet,
      fontFamily = attribute(element, StyleNs, "font-name").orElse(attribute(element, FoNs, "font-family")),
      fontSize = attribute(element, FoNs, "font-size").flatMap(parsePointSize),
      color = attribute(element, FoNs, "color").filterNot(_ == "#000000")
    )

  private def isBold(element: Element): Boolean =
    attribute(element, FoNs, "font-weight").exists(weight => weight == "bold" || weight.toIntOption.exists(_ >= 600))

  private def isUnderlined(element: Element): Boolean =
    attribute(element, StyleNs, "text-underline-style").exists(_ != "none")

  private def parsePointSize(value: String): Option[Float] =
    value.stripSuffix("pt").toFloatOption

  private def paragraphAlignmentFromProperties(element: Element): Option[ParagraphAlignment] =
    attribute(element, FoNs, "text-align").map {
      case "center"                => ParagraphAlignment.Center
      case "end" | "right"         => ParagraphAlignment.Right
      case "justify" | "justified" => ParagraphAlignment.Justify
      case _                       => ParagraphAlignment.Left
    }

  private def paragraphFromElement(element: Element, styles: OdtStyles): RichTextParagraph =
    val alignment = attribute(element, TextNs, "style-name")
      .flatMap(styles.paragraphStyles.get)
      .getOrElse(ParagraphAlignment.Left)
    val role = Option
      .when(element.getNamespaceURI == TextNs && element.getLocalName == "h") {
        ParagraphRole.Heading(attribute(element, TextNs, "outline-level").flatMap(_.toIntOption).getOrElse(1).max(1))
      }
      .getOrElse(ParagraphRole.Body)
    RichTextParagraph(runsFromChildren(element, RichTextStyle.empty, styles), alignment, role).normalized

  private def runsFromChildren(element: Element, currentStyle: RichTextStyle, styles: OdtStyles): List[RichTextRun] =
    childNodes(element).flatMap(runsFromNode(_, currentStyle, styles))

  private def runsFromNode(node: Node, currentStyle: RichTextStyle, styles: OdtStyles): List[RichTextRun] =
    node.getNodeType match
      case Node.TEXT_NODE =>
        Option(node.getNodeValue).filter(_.nonEmpty).map(text => RichTextRun(text, currentStyle)).toList
      case Node.ELEMENT_NODE =>
        val element = node.asInstanceOf[Element]
        if element.getNamespaceURI == TextNs && element.getLocalName == "span" then
          val spanStyle = attribute(element, TextNs, "style-name")
            .flatMap(styles.textStyles.get)
            .map(mergeStyles(currentStyle, _))
            .getOrElse(currentStyle)
          runsFromChildren(element, spanStyle, styles)
        else if element.getNamespaceURI == TextNs && element.getLocalName == "s" then
          val count = attribute(element, TextNs, "c").flatMap(_.toIntOption).getOrElse(1)
          List(RichTextRun(" " * count.max(1), currentStyle))
        else if element.getNamespaceURI == TextNs && element.getLocalName == "tab" then
          List(RichTextRun("\t", currentStyle))
        else if element.getNamespaceURI == TextNs && element.getLocalName == "line-break" then
          List(RichTextRun("\n", currentStyle))
        else runsFromChildren(element, currentStyle, styles)
      case _ =>
        Nil

  private def mergeStyles(base: RichTextStyle, overlay: RichTextStyle): RichTextStyle =
    RichTextStyle(
      marks = base.marks ++ overlay.marks,
      fontFamily = overlay.fontFamily.orElse(base.fontFamily),
      fontSize = overlay.fontSize.orElse(base.fontSize),
      color = overlay.color.orElse(base.color)
    )

  private def contentXml(document: RichTextDocument): String =
    val textStyleNames = distinctRunStyles(document).zipWithIndex.map((style, index) => style -> s"T$index").toMap
    val paragraphStyleNames = document.paragraphs
      .map(_.alignment)
      .distinct
      .zipWithIndex
      .map((alignment, index) => alignment -> s"P$index")
      .toMap

    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<office:document-content
       |    xmlns:office="$OfficeNs"
       |    xmlns:style="$StyleNs"
       |    xmlns:text="$TextNs"
       |    xmlns:fo="$FoNs">
       |  <office:automatic-styles>
       |${automaticStylesXml(textStyleNames, paragraphStyleNames)}
       |  </office:automatic-styles>
       |  <office:body>
       |    <office:text>
       |${paragraphsXml(document, textStyleNames, paragraphStyleNames)}
       |    </office:text>
       |  </office:body>
       |</office:document-content>""".stripMargin

  private def distinctRunStyles(document: RichTextDocument): List[RichTextStyle] =
    document.paragraphs.flatMap(_.runs.map(_.style)).filterNot(_ == RichTextStyle.empty).distinct

  private def automaticStylesXml(
    textStyleNames: Map[RichTextStyle, String],
    paragraphStyleNames: Map[ParagraphAlignment, String]
  ): String =
    val textStyles = textStyleNames.toList
      .sortBy(_._2)
      .map((style, name) => s"""    <style:style style:name="$name" style:family="text">
           |      <style:text-properties${textPropertiesAttributes(style)}/>
           |    </style:style>""".stripMargin)
    val paragraphStyles = paragraphStyleNames.toList
      .sortBy(_._2)
      .map((alignment, name) => s"""    <style:style style:name="$name" style:family="paragraph">
           |      <style:paragraph-properties fo:text-align="${alignmentAttribute(alignment)}"/>
           |    </style:style>""".stripMargin)
    (textStyles ++ paragraphStyles).mkString("\n")

  private def textPropertiesAttributes(style: RichTextStyle): String =
    List(
      Option.when(style.marks.contains(InlineMark.Bold))("""fo:font-weight="bold""""),
      Option.when(style.marks.contains(InlineMark.Italic))("""fo:font-style="italic""""),
      Option.when(style.marks.contains(InlineMark.Underline))("""style:text-underline-style="solid""""),
      style.fontFamily.map(value =>
        s"""style:font-name="${escapeAttribute(value)}" fo:font-family="${escapeAttribute(value)}""""
      ),
      style.fontSize.map(value => s"""fo:font-size="${value.round}pt""""),
      style.color.map(value => s"""fo:color="${escapeAttribute(value)}"""")
    ).flatten match
      case Nil        => ""
      case attributes => " " + attributes.mkString(" ")

  private def alignmentAttribute(alignment: ParagraphAlignment): String =
    alignment match
      case ParagraphAlignment.Left    => "start"
      case ParagraphAlignment.Center  => "center"
      case ParagraphAlignment.Right   => "end"
      case ParagraphAlignment.Justify => "justify"

  private def paragraphsXml(
    document: RichTextDocument,
    textStyleNames: Map[RichTextStyle, String],
    paragraphStyleNames: Map[ParagraphAlignment, String]
  ): String =
    document.paragraphs
      .map(paragraph =>
        val styleName = paragraphStyleNames(paragraph.alignment)
        paragraph.role match
          case ParagraphRole.Body =>
            s"""      <text:p text:style-name="$styleName">${runsXml(paragraph.runs, textStyleNames)}</text:p>"""
          case ParagraphRole.Heading(level) =>
            s"""      <text:h text:outline-level="${level.max(1)}" text:style-name="$styleName">${runsXml(paragraph.runs, textStyleNames)}</text:h>"""
      )
      .mkString("\n")

  private def runsXml(runs: List[RichTextRun], textStyleNames: Map[RichTextStyle, String]): String =
    runs.map { run =>
      if run.style == RichTextStyle.empty then runTextXml(run.text)
      else s"""<text:span text:style-name="${textStyleNames(run.style)}">${runTextXml(run.text)}</text:span>"""
    }.mkString

  private def runTextXml(text: String): String =
    text
      .foldLeft((StringBuilder(), List.empty[String])) {
        case ((chunk, acc), '\t') =>
          (StringBuilder(), acc ++ textChunkXml(chunk) :+ "<text:tab/>")
        case ((chunk, acc), '\n') =>
          (StringBuilder(), acc ++ textChunkXml(chunk) :+ "<text:line-break/>")
        case ((chunk, acc), ' ') =>
          (StringBuilder(), acc ++ textChunkXml(chunk) :+ "<text:s/>")
        case ((chunk, acc), char) =>
          chunk.append(char)
          (chunk, acc)
      } match
      case (chunk, acc) =>
        (acc ++ textChunkXml(chunk)).mkString

  private def textChunkXml(chunk: StringBuilder): Option[String] =
    Option.when(chunk.nonEmpty)(escapeText(chunk.toString))

  private def manifestXml: String =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<manifest:manifest
      |    xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0"
      |    manifest:version="1.2">
      |  <manifest:file-entry manifest:media-type="application/vnd.oasis.opendocument.text" manifest:full-path="/"/>
      |  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="content.xml"/>
      |</manifest:manifest>""".stripMargin

  private def writeZipEntry(zip: ZipOutputStream, name: String, content: String): Unit =
    zip.putNextEntry(ZipEntry(name))
    zip.write(content.getBytes(StandardCharsets.UTF_8))
    zip.closeEntry()

  private def attribute(element: Element, namespace: String, localName: String): Option[String] =
    Option(element.getAttributeNS(namespace, localName)).filter(_.nonEmpty)

  private def childElement(element: Element, namespace: String, localName: String): Option[Element] =
    childElements(element).find(child => child.getNamespaceURI == namespace && child.getLocalName == localName)

  private def childElements(element: Element): List[Element] =
    childNodes(element).collect { case child: Element => child }

  private def childNodes(element: Element): List[Node] =
    nodes(element.getChildNodes)

  private def firstElement(nodes: org.w3c.dom.NodeList): Option[Element] =
    elements(nodes).headOption

  private def elements(nodeList: org.w3c.dom.NodeList): List[Element] =
    nodes(nodeList).collect { case element: Element => element }

  private def nodes(nodeList: org.w3c.dom.NodeList): List[Node] =
    (0 until nodeList.getLength).toList.map(nodeList.item)

  private def escapeText(value: String): String =
    value
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")

  private def escapeAttribute(value: String): String =
    escapeText(value).replace("\"", "&quot;")
