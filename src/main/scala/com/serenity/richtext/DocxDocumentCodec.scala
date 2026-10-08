package com.serenity.richtext

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.util.control.NonFatal

import cats.effect.IO
import com.serenity.io.AtomicFileWriter
import com.serenity.richtext.XmlDom.{childElements, elements, isElement}
import org.w3c.dom.Element

/** Reads and writes Word Open XML documents through Serenity's native rich text model.
  *
  * A document read from a package remembers it (`RichTextDocument.source`): saving copies every part the model does not
  * own and every body paragraph that was not edited byte for byte, and writes again only the paragraphs that changed,
  * with the properties the model does not hold (#1896).
  */
object DocxDocumentCodec:
  private val WNs = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

  private[richtext] val DocumentEntry              = "word/document.xml"
  private[richtext] val DocumentRelationshipsEntry = "word/_rels/document.xml.rels"

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
    readBytesWithFidelity(bytes).map(_.document)

  /** Decode DOCX bytes and report structures that the native model cannot round-trip. */
  def readBytesWithFidelity(bytes: Array[Byte]): Either[RichTextCodecException, RichTextImport] =
    decode(bytes, RichTextDecodeSteps.live)

  /** One archive pass and one `document.xml` parse yield the model and the fidelity report together (#1882). */
  private[richtext] def decode(
    bytes: Array[Byte],
    steps: RichTextDecodeSteps
  ): Either[RichTextCodecException, RichTextImport] =
    try
      val archive = steps.readArchive(bytes, "DOCX", Set(DocumentEntry, DocumentRelationshipsEntry))
      val content = archive.entries.getOrElse(
        DocumentEntry,
        throw RichTextCodecException("DOCX archive is missing word/document.xml")
      )
      val xml           = steps.parseBody(content)
      val relationships = archive.entries.get(DocumentRelationshipsEntry)
      val main          = MainPart.decode(content)
      val sources = main
        .filter(_.text.contains(s"""xmlns:w="$WNs""""))
        .flatMap(part => SourceMap.of(xml.getDocumentElement, part.text))
      val context = DocxReadContext(relationships.map(DocxLinks.hyperlinkTargets).getOrElse(Map.empty), sources)
      val body    = elements(xml.getElementsByTagNameNS(WNs, "body")).headOption
      val paragraphs = body.toList
        .flatMap(childElements)
        .zipWithIndex
        .collect { case (element, index) if isParagraph(element) => DocxParagraphReader.read(element, index, context) }
      val source = DocumentSource(
        PackageFormat.Docx,
        bytes,
        DocumentEntry,
        main,
        body.flatMap(element => sources.flatMap(_.bodySource(element, isParagraph))),
        relationships.flatMap(MainPart.decode).map(_.text)
      )
      val document = RichTextDocument(
        if paragraphs.nonEmpty then paragraphs
        else List(RichTextParagraph.plain(""))
      ).normalized.withSource(Some(source))
      val unsupportedElements =
        RichTextXmlParser.elementNames(xml.getDocumentElement, Some(WNs)) -- SupportedElements
      val unsupportedEntries = archive.entryNames -- SupportedArchiveEntries
      Right(RichTextImport(document, RichTextFidelity(unsupportedElements, unsupportedEntries)))
    catch
      case error: RichTextCodecException => Left(error)
      case NonFatal(error)               => Left(RichTextCodecException("DOCX document could not be decoded", error))

  private def isParagraph(element: Element): Boolean =
    isElement(element, WNs, "p")

  /** The package bytes for `document`: its source package with the edits applied when it has one, else a new package.
    */
  def writeBytes(document: RichTextDocument): Array[Byte] =
    val normalized = document.normalized
    normalized.source
      .filter(_.format == PackageFormat.Docx)
      .fold(newPackage(normalized))(DocxPackageWriter.rewrite(normalized, _))

  private def newPackage(document: RichTextDocument): Array[Byte] =
    val links  = DocxLinks.allocate(document.paragraphs.flatMap(_.runs.flatMap(_.style.link)), None)
    val output = ByteArrayOutputStream()
    val zip    = ZipOutputStream(output, StandardCharsets.UTF_8)
    try
      writeZipEntry(zip, "[Content_Types].xml", contentTypesXml)
      writeZipEntry(zip, "_rels/.rels", packageRelationshipsXml)
      writeZipEntry(zip, DocumentEntry, DocxPackageWriter.newDocumentXml(document, links))
      writeZipEntry(zip, DocumentRelationshipsEntry, DocxLinks.relationshipsDocument(links.additions))
    finally zip.close()
    output.toByteArray

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

  private def writeZipEntry(zip: ZipOutputStream, name: String, content: String): Unit =
    zip.putNextEntry(ZipEntry(name))
    zip.write(content.getBytes(StandardCharsets.UTF_8))
    zip.closeEntry()
