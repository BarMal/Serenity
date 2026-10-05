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
  private val WNs    = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  private val RelNs  = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
  private val PkgRel = "http://schemas.openxmlformats.org/package/2006/relationships"

  private val DocumentRelationshipsEntry = "word/_rels/document.xml.rels"
  private val HyperlinkRelationshipType  = s"$RelNs/hyperlink"

  private val SupportedArchiveEntries = Set(
    "[Content_Types].xml",
    "_rels/.rels",
    "word/document.xml",
    DocumentRelationshipsEntry
  )

  private val SupportedElements = Set(
    "document",
    "body",
    "p",
    "pPr",
    "jc",
    "pStyle",
    "framePr",
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

  def read(path: Path): IO[RichTextDocument] =
    IO.blocking(RichTextArchive.readFile(path, "DOCX")).flatMap(bytes => IO.fromEither(readBytes(bytes)))

  /** Read a DOCX file and report structures that the native model cannot round-trip. */
  def readWithFidelity(path: Path): IO[RichTextImport] =
    IO.blocking(RichTextArchive.readFile(path, "DOCX")).flatMap(bytes => IO.fromEither(readBytesWithFidelity(bytes)))

  def write(document: RichTextDocument, path: Path): IO[Unit] =
    AtomicFileWriter.writeBytes(path, writeBytes(document))

  def readBytes(bytes: Array[Byte]): Either[RichTextCodecException, RichTextDocument] =
    try
      val content = RichTextArchive.zipEntry(bytes, "word/document.xml", "DOCX").getOrElse {
        throw RichTextCodecException("DOCX archive is missing word/document.xml")
      }
      val xml = parseXml(content)
      val links = RichTextArchive
        .zipEntry(bytes, DocumentRelationshipsEntry, "DOCX")
        .map(hyperlinkTargets)
        .getOrElse(Map.empty)
      val paragraphs = firstElement(xml.getElementsByTagNameNS(WNs, "body"))
        .map(body =>
          childElements(body)
            .filter(element => element.getNamespaceURI == WNs && element.getLocalName == "p")
            .map(paragraphFromElement(_, links))
        )
        .getOrElse(Nil)

      Right(
        RichTextDocument(
          if paragraphs.nonEmpty then paragraphs
          else List(RichTextParagraph.plain(""))
        ).normalized
      )
    catch
      case error: RichTextCodecException => Left(error)
      case NonFatal(error)               => Left(RichTextCodecException("DOCX document could not be decoded", error))

  /** Decode DOCX bytes and report structures that the native model cannot round-trip. */
  def readBytesWithFidelity(bytes: Array[Byte]): Either[RichTextCodecException, RichTextImport] =
    readBytes(bytes).flatMap { document =>
      try
        val content = RichTextArchive.zipEntry(bytes, "word/document.xml", "DOCX").getOrElse(Array.emptyByteArray)
        val xml     = parseXml(content)
        val unsupportedElements =
          (0 until xml.getElementsByTagNameNS(WNs, "*").getLength)
            .map(xml.getElementsByTagNameNS(WNs, "*").item)
            .collect { case element: Element => element.getLocalName }
            .filterNot(SupportedElements.contains)
            .toSet
        val unsupportedEntries = RichTextArchive.entryNames(bytes, "DOCX") -- SupportedArchiveEntries
        Right(RichTextImport(document, RichTextFidelity(unsupportedElements, unsupportedEntries)))
      catch
        case error: RichTextCodecException => Left(error)
        case NonFatal(error)               => Left(RichTextCodecException("DOCX document could not be decoded", error))
    }

  def writeBytes(document: RichTextDocument): Array[Byte] =
    val normalized = document.normalized
    val linkIds    = externalLinkIds(normalized)
    val output     = ByteArrayOutputStream()
    val zip        = ZipOutputStream(output, StandardCharsets.UTF_8)
    try
      writeZipEntry(zip, "[Content_Types].xml", contentTypesXml)
      writeZipEntry(zip, "_rels/.rels", packageRelationshipsXml)
      writeZipEntry(zip, "word/document.xml", documentXml(normalized, linkIds))
      writeZipEntry(zip, DocumentRelationshipsEntry, documentRelationshipsXml(linkIds))
    finally zip.close()
    output.toByteArray

  private def parseXml(bytes: Array[Byte]): XmlDocument =
    RichTextXmlParser.parse(bytes)

  /** Relationship id to external target, for the relationships that are hyperlinks. */
  private def hyperlinkTargets(relationshipsXml: Array[Byte]): Map[String, String] =
    elements(parseXml(relationshipsXml).getElementsByTagNameNS(PkgRel, "Relationship"))
      .filter(_.getAttribute("Type") == HyperlinkRelationshipType)
      .flatMap(relationship =>
        Option(relationship.getAttribute("Id"))
          .filter(_.nonEmpty)
          .zip(Option(relationship.getAttribute("Target")).filter(_.nonEmpty))
      )
      .toMap

  private def paragraphFromElement(element: Element, links: Map[String, String]): RichTextParagraph =
    val paragraphProperties = childElement(element, WNs, "pPr")
    val alignment = paragraphProperties
      .flatMap(childElement(_, WNs, "jc"))
      .flatMap(attribute(_, WNs, "val"))
      .map(alignmentFromValue)
      .getOrElse(ParagraphAlignment.Left)
    val role = paragraphProperties
      .flatMap(dropCapRoleFromProperties)
      .orElse(
        paragraphProperties
          .flatMap(childElement(_, WNs, "pStyle"))
          .flatMap(attribute(_, WNs, "val"))
          .flatMap(headingRoleFromStyle)
      )
      .getOrElse(ParagraphRole.Body)
    RichTextParagraph(
      childElements(element).flatMap(runsFromNode(_, links, None)),
      alignment,
      role
    ).normalized

  private def alignmentFromValue(value: String): ParagraphAlignment =
    value match
      case "center"              => ParagraphAlignment.Center
      case "right" | "end"       => ParagraphAlignment.Right
      case "both" | "distribute" => ParagraphAlignment.Justify
      case _                     => ParagraphAlignment.Left

  /** DOCX's native drop cap representation: a `w:framePr` paragraph frame property carrying `w:dropCap` (`"drop"` or
    * `"margin"`, either meaning the first character is a drop cap) and `w:lines` (the span, in lines). This is a
    * paragraph *frame* property, not a named style, unlike headings.
    */
  private def dropCapRoleFromProperties(paragraphProperties: Element): Option[ParagraphRole] =
    childElement(paragraphProperties, WNs, "framePr").flatMap { frame =>
      attribute(frame, WNs, "dropCap").filter(_.nonEmpty).map { _ =>
        val lines = attribute(frame, WNs, "lines").flatMap(_.toIntOption).getOrElse(ParagraphRole.DefaultDropCapLines)
        ParagraphRole.dropCap(lines)
      }
    }

  private def headingRoleFromStyle(value: String): Option[ParagraphRole] =
    val normalized = value.toLowerCase
    Option.when(normalized.startsWith("heading")) {
      val level = normalized.drop("heading".length).filter(_.isDigit).toIntOption.getOrElse(1)
      ParagraphRole.Heading(level.max(1))
    }

  private def runsFromRunElement(element: Element, link: Option[String]): List[RichTextRun] =
    val style = childElement(element, WNs, "rPr")
      .map(styleFromRunProperties)
      .getOrElse(RichTextStyle.empty)
      .copy(link = link)
    childElements(element).flatMap {
      case child if child.getNamespaceURI == WNs && child.getLocalName == "t" =>
        Option(child.getTextContent).map(RichTextRun(_, style)).toList
      case child if child.getNamespaceURI == WNs && child.getLocalName == "tab" =>
        List(RichTextRun("\t", style))
      case child if child.getNamespaceURI == WNs && child.getLocalName == "br" =>
        List(RichTextRun.softBreak(style))
      case _ =>
        Nil
    }

  private def runsFromNode(element: Element, links: Map[String, String], link: Option[String]): List[RichTextRun] =
    if element.getNamespaceURI == WNs && element.getLocalName == "r" then runsFromRunElement(element, link)
    else
      val childLink =
        if element.getNamespaceURI == WNs && element.getLocalName == "hyperlink" then hyperlinkTarget(element, links)
        else link
      childElements(element).flatMap(runsFromNode(_, links, childLink))

  private def hyperlinkTarget(element: Element, links: Map[String, String]): Option[String] =
    attribute(element, RelNs, "id")
      .flatMap(links.get)
      .orElse(attribute(element, WNs, "anchor").map("#" + _))

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

  /** Relationship ids for the document's external link targets, in order of first use. Anchors need none. */
  private def externalLinkIds(document: RichTextDocument): Map[String, String] =
    document.paragraphs
      .flatMap(_.runs.flatMap(_.style.link))
      .filterNot(_.startsWith("#"))
      .distinct
      .zipWithIndex
      .map((target, index) => target -> s"rId${index + 1}")
      .toMap

  private def documentXml(document: RichTextDocument, linkIds: Map[String, String]): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<w:document xmlns:w="$WNs" xmlns:r="$RelNs">
       |  <w:body>
       |${paragraphsXml(document, linkIds)}
       |    <w:sectPr/>
       |  </w:body>
       |</w:document>""".stripMargin

  private def paragraphsXml(document: RichTextDocument, linkIds: Map[String, String]): String =
    document.paragraphs.map(paragraphXml(_, linkIds)).mkString("\n")

  private def paragraphXml(paragraph: RichTextParagraph, linkIds: Map[String, String]): String =
    s"""    <w:p>
       |${paragraphPropertiesXml(paragraph)}
       |${paragraph.linkSpans.map(linkSpanXml(_, linkIds)).mkString("\n")}
       |    </w:p>""".stripMargin

  private def paragraphPropertiesXml(paragraph: RichTextParagraph): String =
    val roleProperty = paragraph.role match
      case ParagraphRole.Body =>
        None
      case ParagraphRole.Heading(level) =>
        Some(s"""<w:pStyle w:val="Heading${level.max(1)}"/>""")
      case ParagraphRole.DropCap(lines) =>
        Some(
          s"""<w:framePr w:dropCap="drop" w:lines="${lines.max(1)}" w:wrap="around" w:vAnchor="text" w:hAnchor="text"/>"""
        )
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

  private def linkSpanXml(span: (Option[String], List[RichTextRun]), linkIds: Map[String, String]): String =
    val (target, runs) = span
    val runsXml = runs
      .map(run => s"""      <w:r>${runPropertiesXml(run.style)}${runContentXml(run)}</w:r>""")
      .mkString("\n")
    target.fold(runsXml) { link =>
      val reference = linkIds
        .get(link)
        .map(id => s"""r:id="$id"""")
        .getOrElse(s"""w:anchor="${escapeAttribute(link.stripPrefix("#"))}"""")
      s"""      <w:hyperlink $reference>
         |$runsXml
         |      </w:hyperlink>""".stripMargin
    }

  private def runContentXml(run: RichTextRun): String =
    run.atom.fold(runTextXml(run.text)) { case InlineAtom.SoftBreak => "<w:br/>" }

  private def runTextXml(text: String): String =
    text
      .foldLeft((StringBuilder(), List.empty[String])) {
        case ((chunk, acc), '\t') =>
          (StringBuilder(), acc ++ textChunkXml(chunk) :+ "<w:tab/>")
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

  private def documentRelationshipsXml(linkIds: Map[String, String]): String =
    val relationships = linkIds.toList
      .sortBy(_._2.stripPrefix("rId").toInt)
      .map((target, id) =>
        s"""  <Relationship Id="$id" Type="$HyperlinkRelationshipType" Target="${escapeAttribute(
            target
          )}" TargetMode="External"/>"""
      )
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<Relationships xmlns="$PkgRel">
       |${relationships.mkString("\n")}
       |</Relationships>""".stripMargin

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
