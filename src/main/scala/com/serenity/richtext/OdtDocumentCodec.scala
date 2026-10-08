package com.serenity.richtext

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.util.control.NonFatal

import cats.effect.IO
import com.serenity.io.AtomicFileWriter
import com.serenity.richtext.OdtStyles.{OfficeNs, TextNs}
import com.serenity.richtext.XmlDom.{childElements, elements}
import org.w3c.dom.Element

/** Reads and writes OpenDocument Text files through Serenity's native rich text model.
  *
  * A document read from a package remembers it (`RichTextDocument.source`): saving copies every part the model does not
  * own and every body paragraph that was not edited byte for byte, and writes again only the paragraphs that changed.
  */
object OdtDocumentCodec:
  private val ContentEntry = "content.xml"

  def read(path: Path): IO[RichTextDocument] =
    IO.blocking(RichTextArchive.readFile(path, "ODT")).flatMap(bytes => IO.fromEither(readBytes(bytes)))

  /** Read an ODT file and report structures that the native model cannot round-trip. */
  def readWithFidelity(path: Path): IO[RichTextImport] =
    IO.blocking(RichTextArchive.readFile(path, "ODT")).flatMap(bytes => IO.fromEither(readBytesWithFidelity(bytes)))

  def write(document: RichTextDocument, path: Path): IO[Unit] =
    AtomicFileWriter.writeBytes(path, writeBytes(document))

  def readBytes(bytes: Array[Byte]): Either[RichTextCodecException, RichTextDocument] =
    readBytesWithFidelity(bytes).map(_.document)

  /** Decode ODT bytes and report structures that the native model cannot round-trip. */
  def readBytesWithFidelity(bytes: Array[Byte]): Either[RichTextCodecException, RichTextImport] =
    decode(bytes, RichTextDecodeSteps.live)

  /** One archive pass and one `content.xml` parse yield the model and the fidelity report together (#1882). */
  private[richtext] def decode(
    bytes: Array[Byte],
    steps: RichTextDecodeSteps
  ): Either[RichTextCodecException, RichTextImport] =
    try
      val archive = steps.readArchive(bytes, "ODT", Set(ContentEntry))
      val content = archive.entries.getOrElse(
        ContentEntry,
        throw RichTextCodecException("ODT archive is missing content.xml")
      )
      val xml  = steps.parseBody(content)
      val main = MainPart.decode(content)
      val sources = main
        .filter(_.text.contains(s"""xmlns:text="$TextNs""""))
        .flatMap(part => SourceMap.of(xml.getDocumentElement, part.text))
      val context     = OdtReadContext(OdtStyles.fromDocument(xml), sources)
      val textElement = elements(xml.getElementsByTagNameNS(OfficeNs, "text")).headOption
      val paragraphs = textElement.toList
        .flatMap(childElements)
        .zipWithIndex
        .flatMap((element, index) => bodyParagraph(element, index, context))
      val source = DocumentSource(
        PackageFormat.Odt,
        bytes,
        ContentEntry,
        main,
        textElement.flatMap(element => sources.flatMap(_.bodySource(element, BodyBlocks.odt))),
        None,
        archive.entryNames
      )
      val document = RichTextDocument(
        if paragraphs.nonEmpty then paragraphs
        else List(RichTextParagraph.plain(""))
      ).normalized.withSource(Some(source))
      Right(RichTextImport(document, FidelityReport.forSave(document, SaveTarget.Odt)))
    catch
      case error: RichTextCodecException => Left(error)
      case NonFatal(error)               => Left(RichTextCodecException("ODT document could not be decoded", error))

  private def bodyParagraph(element: Element, index: Int, context: OdtReadContext): Option[RichTextParagraph] =
    BodyBlocks.odt(element) match
      case BodyKind.Paragraph      => Some(OdtParagraphReader.read(element, index, context))
      case BodyKind.Block(feature) => Some(BodyBlocks.line(context.raw(element), feature, index))
      case BodyKind.Structural     => None

  /** The package bytes for `document`: its source package with the edits applied when it has one, else a new package.
    */
  def writeBytes(document: RichTextDocument): Array[Byte] =
    val normalized = document.normalized
    normalized.source
      .filter(_.format == PackageFormat.Odt)
      .fold(newPackage(normalized))(OdtPackageWriter.rewrite(normalized, _))

  private def newPackage(document: RichTextDocument): Array[Byte] =
    val output = ByteArrayOutputStream()
    val zip    = ZipOutputStream(output, StandardCharsets.UTF_8)
    try
      writeZipEntry(zip, "mimetype", "application/vnd.oasis.opendocument.text")
      writeZipEntry(zip, "META-INF/manifest.xml", manifestXml)
      writeZipEntry(zip, ContentEntry, OdtPackageWriter.newContentXml(document))
    finally zip.close()
    output.toByteArray

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
