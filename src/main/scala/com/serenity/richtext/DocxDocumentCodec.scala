package com.serenity.richtext

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.ZipOutputStream

import scala.util.Using

import cats.effect.IO
import cats.syntax.all.*
import com.serenity.io.AtomicFileWriter
import com.serenity.richtext.XmlDom.{childElements, elements}
import org.w3c.dom.Element

/** Reads and writes Word Open XML documents through Serenity's native rich text model.
  *
  * A document read from a package remembers it (`RichTextDocument.source`): saving copies every part the model does not
  * own and every body paragraph that was not edited byte for byte, and writes again only the paragraphs that changed,
  * with the properties the model does not hold (#1896).
  */
object DocxDocumentCodec:
  private val Format = "DOCX"
  private val WNs    = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

  private[richtext] val DocumentEntry              = "word/document.xml"
  private[richtext] val DocumentRelationshipsEntry = "word/_rels/document.xml.rels"

  def read(path: Path): IO[RichTextDocument] =
    RichTextArchive.readFile(path, Format).flatMap(bytes => IO.fromEither(readBytes(bytes)))

  /** Read a DOCX file and report structures that the native model cannot round-trip. */
  def readWithFidelity(path: Path): IO[RichTextImport] =
    RichTextArchive.readFile(path, Format).flatMap(bytes => IO.fromEither(readBytesWithFidelity(bytes)))

  def write(document: RichTextDocument, path: Path): IO[Unit] =
    IO.delay(encode(document)).flatMap(IO.fromEither).flatMap(AtomicFileWriter.writeBytes(path, _))

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
    for
      archive <- steps.readArchive(bytes, Format, Set(DocumentEntry, DocumentRelationshipsEntry))
      content <- archive.entries
        .get(DocumentEntry)
        .toRight(RichTextCodecException("DOCX archive is missing word/document.xml"))
      xml           <- steps.parseBody(content, Format)
      relationships <- archive.entries.get(DocumentRelationshipsEntry).traverse(DocxLinks.hyperlinkTargets)
      imported      <- model(bytes, archive, content, xml, relationships)
    yield imported

  /** The DOM walk is the last place a malformed document can surface an unchecked exception, so it is the one place
    * `catchNonFatal` stands guard.
    */
  private def model(
    bytes: Array[Byte],
    archive: ArchiveContents,
    content: Array[Byte],
    xml: org.w3c.dom.Document,
    hyperlinkTargets: Option[Map[String, String]]
  ): Either[RichTextCodecException, RichTextImport] =
    Either
      .catchNonFatal {
        val relationships = archive.entries.get(DocumentRelationshipsEntry)
        val main          = MainPart.decode(content)
        val sources = main
          .filter(_.text.contains(s"""xmlns:w="$WNs""""))
          .flatMap(part => SourceMap.of(xml.getDocumentElement, part.text))
        val context = DocxReadContext(hyperlinkTargets.getOrElse(Map.empty), sources)
        val body    = elements(xml.getElementsByTagNameNS(WNs, "body")).headOption
        val paragraphs = body.toList
          .flatMap(childElements)
          .zipWithIndex
          .flatMap((element, index) => bodyParagraph(element, index, context))
        val source = DocumentSource(
          PackageFormat.Docx,
          bytes,
          DocumentEntry,
          main,
          body.flatMap(element => sources.flatMap(_.bodySource(element, BodyBlocks.docx))),
          relationships.flatMap(MainPart.decode).map(_.text),
          archive.entryNames
        )
        val document = RichTextDocument(
          if paragraphs.nonEmpty then paragraphs
          else List(RichTextParagraph.plain(""))
        ).normalized.withSource(Some(source))
        RichTextImport(document, FidelityReport.forSave(document, SaveTarget.Docx))
      }
      .left
      .map(error => RichTextCodecException(s"$Format document could not be decoded", error))

  private def bodyParagraph(element: Element, index: Int, context: DocxReadContext): Option[RichTextParagraph] =
    BodyBlocks.docx(element) match
      case BodyKind.Paragraph      => Some(DocxParagraphReader.read(element, index, context))
      case BodyKind.Block(feature) => Some(BodyBlocks.line(context.raw(element), feature, index))
      case BodyKind.Structural     => None

  /** The package bytes for `document`: its source package with the edits applied when it has one, else a new package.
    */
  def encode(document: RichTextDocument): Either[RichTextCodecException, Array[Byte]] =
    val normalized = document.normalized
    normalized.source
      .filter(_.format == PackageFormat.Docx)
      .fold(newPackage(normalized))(DocxPackageWriter.rewrite(normalized, _))

  /** [[encode]] for callers that cannot take an `Either`; it throws the `Left`. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def writeBytes(document: RichTextDocument): Array[Byte] =
    encode(document).fold(error => throw error, identity)

  private def newPackage(document: RichTextDocument): Either[RichTextCodecException, Array[Byte]] =
    DocxLinks.allocate(document.paragraphs.flatMap(_.runs.flatMap(_.style.link)), None).flatMap { links =>
      val output = ByteArrayOutputStream()
      Using(ZipOutputStream(output, StandardCharsets.UTF_8)) { zip =>
        writeZipEntry(zip, "[Content_Types].xml", contentTypesXml)
        writeZipEntry(zip, "_rels/.rels", packageRelationshipsXml)
        writeZipEntry(zip, DocumentEntry, DocxPackageWriter.newDocumentXml(document, links))
        writeZipEntry(zip, DocumentRelationshipsEntry, DocxLinks.relationshipsDocument(links.additions))
      }.toEither.left
        .map(error => RichTextCodecException(s"$Format archive could not be written", error))
        .map(_ => output.toByteArray)
    }

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
    zip.putNextEntry(NewPackageEntry.named(name))
    zip.write(content.getBytes(StandardCharsets.UTF_8))
    zip.closeEntry()
