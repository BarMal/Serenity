package com.serenity.richtext

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.util.control.NonFatal

import cats.effect.IO
import com.serenity.io.AtomicFileWriter
import org.w3c.dom.{Document as XmlDocument, Element, Node}

/** Reads and writes Word Open XML documents through Serenity's native rich text model. */
object DocxDocumentCodec:
  private val WNs = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  private val SupportedArchiveEntries = Set(
    "[Content_Types].xml",
    "_rels/.rels",
    "word/document.xml",
    "word/_rels/document.xml.rels"
  )
  private val SupportedElements = Set(
    "document",
    "body",
    "p",
    "pPr",
    "jc",
    "pStyle",
    "r",
    "rPr",
    "b",
    "i",
    "u",
    "rFonts",
    "sz",
    "color",
    "t",
    "tab",
    "br",
    "hyperlink",
    "sectPr"
  )

  /** Read a DOCX file into Serenity's native rich text model. */
  def read(path: Path): IO[RichTextDocument] =
    IO.blocking(readBytes(RichTextArchive.readFile(path, "DOCX")))

  /** Write Serenity's native rich text model to a DOCX file. */
  def write(document: RichTextDocument, path: Path): IO[Unit] =
    AtomicFileWriter.writeBytes(path, writeBytes(document))

  /** Decode DOCX bytes into Serenity's native rich text model. */
  def readBytes(bytes: Array[Byte]): RichTextDocument =
    try
      val content = RichTextArchive.zipEntry(bytes, "word/document.xml", "DOCX").getOrElse {
        throw RichTextCodecException("DOCX archive is missing word/document.xml")
      }
      val xml = parseXml(content)
      val paragraphs = firstElement(xml.getElementsByTagNameNS(WNs, "body"))
        .map(body =>
          childElements(body)
            .filter(element => element.getNamespaceURI == WNs && element.getLocalName == "p")
            .map(paragraphFromElement)
        )
        .getOrElse(Nil)

      RichTextDocument(
        if paragraphs.nonEmpty then paragraphs
        else List(RichTextParagraph.plain(""))
      ).normalized
    catch
      case error: RichTextCodecException => throw error
      case NonFatal(error)               => throw RichTextCodecException("DOCX document could not be decoded", error)

  /** Decode DOCX bytes and report structures that the native model cannot round-trip. */
  def readBytesWithFidelity(bytes: Array[Byte]): RichTextImport =
    val document = readBytes(bytes)
    val content = RichTextArchive.zipEntry(bytes, "word/document.xml", "DOCX").getOrElse(Array.emptyByteArray)
    val xml     = parseXml(content)
    val unsupportedElements =
      (0 until xml.getElementsByTagNameNS(WNs, "*").getLength)
        .map(xml.getElementsByTagNameNS(WNs, "*").item)
        .collect { case element: Element => element.getLocalName }
        .filterNot(SupportedElements.contains)
        .toSet
    val unsupportedEntries = RichTextArchive.entryNames(bytes, "DOCX") -- SupportedArchiveEntries
    RichTextImport(document, RichTextFidelity(unsupportedElements, unsupportedEntries))

  /** Encode Serenity's native rich text model as DOCX bytes. */
  def writeBytes(document: RichTextDocument): Array[Byte] =
    val output = ByteArrayOutputStream()
    val zip    = ZipOutputStream(output, StandardCharsets.UTF_8)
    try
      writeZipEntry(zip, "[Content_Types].xml", contentTypesXml)
      writeZipEntry(zip, "_rels/.rels", packageRelationshipsXml)
      writeZipEntry(zip, "word/document.xml", documentXml(document.normalized))
      writeZipEntry(zip, "word/_rels/document.xml.rels", documentRelationshipsXml)
    finally zip.close()
    output.toByteArray

  private def parseXml(bytes: Array[Byte]): XmlDocument =
    RichTextXmlParser.parse(bytes)

  private def paragraphFromElement(element: Element): RichTextParagraph =
    val paragraphProperties = childElement(element, WNs, "pPr")
    val alignment = paragraphProperties
      .flatMap(childElement(_, WNs, "jc"))
      .flatMap(attribute(_, WNs, "val"))
      .map(alignmentFromValue)
      .getOrElse(ParagraphAlignment.Left)
    val role = paragraphProperties
      .flatMap(childElement(_, WNs, "pStyle"))
      .flatMap(attribute(_, WNs, "val"))
      .flatMap(headingRoleFromStyle)
      .getOrElse(ParagraphRole.Body)
    RichTextParagraph(
      childElements(element).flatMap(runsFromNode),
      alignment,
      role
    ).normalized

  private def alignmentFromValue(value: String): ParagraphAlignment =
    value match
      case "center"              => ParagraphAlignment.Center
      case "right" | "end"       => ParagraphAlignment.Right
      case "both" | "distribute" => ParagraphAlignment.Justify
      case _                     => ParagraphAlignment.Left

  private def headingRoleFromStyle(value: String): Option[ParagraphRole] =
    val normalized = value.toLowerCase
    Option.when(normalized.startsWith("heading")) {
      val level = normalized.drop("heading".length).filter(_.isDigit).toIntOption.getOrElse(1)
      ParagraphRole.Heading(level.max(1))
    }

  private def runFromElement(element: Element): Option[RichTextRun] =
    val style = childElement(element, WNs, "rPr").map(styleFromRunProperties).getOrElse(RichTextStyle.empty)
    val text = childElements(element).flatMap {
      case child if child.getNamespaceURI == WNs && child.getLocalName == "t" =>
        Option(child.getTextContent).toList
      case child if child.getNamespaceURI == WNs && child.getLocalName == "tab" =>
        List("\t")
      case child if child.getNamespaceURI == WNs && child.getLocalName == "br" =>
        List("\n")
      case _ =>
        Nil
    }.mkString
    Option.when(text.nonEmpty)(RichTextRun(text, style))

  private def runsFromNode(element: Element): List[RichTextRun] =
    if element.getNamespaceURI == WNs && element.getLocalName == "r" then runFromElement(element).toList
    else childElements(element).flatMap(runsFromNode)

  private def styleFromRunProperties(element: Element): RichTextStyle =
    RichTextStyle(
      marks = List(
        Option.when(toggleElementEnabled(element, "b"))(InlineMark.Bold),
        Option.when(toggleElementEnabled(element, "i"))(InlineMark.Italic),
        Option.when(underlineEnabled(element))(InlineMark.Underline)
      ).flatten.toSet,
      fontFamily = childElement(element, WNs, "rFonts")
        .flatMap(fonts => attribute(fonts, WNs, "ascii").orElse(attribute(fonts, WNs, "hAnsi"))),
      fontSize = childElement(element, WNs, "sz").flatMap(attribute(_, WNs, "val")).flatMap(parseHalfPointSize),
      color = childElement(element, WNs, "color").flatMap(attribute(_, WNs, "val")).flatMap(parseColor)
    )

  private def toggleElementEnabled(element: Element, localName: String): Boolean =
    childElement(element, WNs, localName).exists(child =>
      attribute(child, WNs, "val").forall(value => value != "false" && value != "0")
    )

  private def underlineEnabled(element: Element): Boolean =
    childElement(element, WNs, "u").exists(child => attribute(child, WNs, "val").forall(_ != "none"))

  private def parseHalfPointSize(value: String): Option[Float] =
    value.toFloatOption.map(_ / 2.0f)

  private def parseColor(value: String): Option[String] =
    Option(value)
      .map(_.stripPrefix("#"))
      .filter(hex => hex.matches("[0-9a-fA-F]{6}") && hex != "000000")
      .map(hex => s"#${hex.toLowerCase}")

  private def documentXml(document: RichTextDocument): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<w:document xmlns:w="$WNs">
       |  <w:body>
       |${paragraphsXml(document)}
       |    <w:sectPr/>
       |  </w:body>
       |</w:document>""".stripMargin

  private def paragraphsXml(document: RichTextDocument): String =
    document.paragraphs.map(paragraphXml).mkString("\n")

  private def paragraphXml(paragraph: RichTextParagraph): String =
    s"""    <w:p>
       |${paragraphPropertiesXml(paragraph)}
       |${runsXml(paragraph.runs)}
       |    </w:p>""".stripMargin

  private def paragraphPropertiesXml(paragraph: RichTextParagraph): String =
    val roleProperty = paragraph.role match
      case ParagraphRole.Body =>
        None
      case ParagraphRole.Heading(level) =>
        Some(s"""<w:pStyle w:val="Heading${level.max(1)}"/>""")
    val alignmentProperty = Option
      .when(paragraph.alignment != ParagraphAlignment.Left)(
        s"""<w:jc w:val="${alignmentValue(paragraph.alignment)}"/>"""
      )
    val properties = List(roleProperty, alignmentProperty).flatten
    if properties.isEmpty then ""
    else s"""      <w:pPr>${properties.mkString}</w:pPr>"""

  private def alignmentValue(alignment: ParagraphAlignment): String =
    alignment match
      case ParagraphAlignment.Left    => "left"
      case ParagraphAlignment.Center  => "center"
      case ParagraphAlignment.Right   => "right"
      case ParagraphAlignment.Justify => "both"

  private def runsXml(runs: List[RichTextRun]): String =
    runs
      .map(run => s"""      <w:r>${runPropertiesXml(run.style)}${runTextXml(run.text)}</w:r>""")
      .mkString("\n")

  private def runTextXml(text: String): String =
    text
      .foldLeft((StringBuilder(), List.empty[String])) {
        case ((chunk, acc), '\t') =>
          (StringBuilder(), acc ++ textChunkXml(chunk) :+ "<w:tab/>")
        case ((chunk, acc), '\n') =>
          (StringBuilder(), acc ++ textChunkXml(chunk) :+ "<w:br/>")
        case ((chunk, acc), char) =>
          chunk.append(char)
          (chunk, acc)
      } match
      case (chunk, acc) =>
        (acc ++ textChunkXml(chunk)).mkString

  private def textChunkXml(chunk: StringBuilder): Option[String] =
    Option.when(chunk.nonEmpty)(s"""<w:t xml:space="preserve">${escapeText(chunk.toString)}</w:t>""")

  private def runPropertiesXml(style: RichTextStyle): String =
    val properties = List(
      Option.when(style.marks.contains(InlineMark.Bold))("<w:b/>"),
      Option.when(style.marks.contains(InlineMark.Italic))("<w:i/>"),
      Option.when(style.marks.contains(InlineMark.Underline))("""<w:u w:val="single"/>"""),
      style.fontFamily.map(value =>
        s"""<w:rFonts w:ascii="${escapeAttribute(value)}" w:hAnsi="${escapeAttribute(value)}"/>"""
      ),
      style.fontSize.map(value => s"""<w:sz w:val="${(value * 2).round}"/>"""),
      style.color.map(value => s"""<w:color w:val="${escapeAttribute(value.stripPrefix("#"))}"/>""")
    ).flatten
    if properties.isEmpty then "" else s"<w:rPr>${properties.mkString}</w:rPr>"

  private def contentTypesXml: String =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
      |  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
      |  <Default Extension="xml" ContentType="application/xml"/>
      |  <Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>
      |</Types>""".stripMargin

  private def packageRelationshipsXml: String =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
      |  <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>
      |</Relationships>""".stripMargin

  private def documentRelationshipsXml: String =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"/>""".stripMargin

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
