package com.serenity.richtext

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.ZipOutputStream

import scala.util.Using

import cats.effect.IO
import cats.syntax.all.*
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
  private val Format       = "ODT"
  private val ContentEntry = "content.xml"

  def read(path: Path): IO[RichTextDocument] =
    RichTextArchive.readFile(path, Format).flatMap(bytes => IO.fromEither(readBytes(bytes)))

  /** Read an ODT file and report structures that the native model cannot round-trip. */
  def readWithFidelity(path: Path): IO[RichTextImport] =
    RichTextArchive.readFile(path, Format).flatMap(bytes => IO.fromEither(readBytesWithFidelity(bytes)))

  def write(document: RichTextDocument, path: Path): IO[Unit] =
    IO.delay(encode(document)).flatMap(IO.fromEither).flatMap(AtomicFileWriter.writeBytes(path, _))

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
    for
      archive <- steps.readArchive(bytes, Format, Set(ContentEntry))
      content <- archive.entries.get(ContentEntry).toRight(RichTextCodecException("ODT archive is missing content.xml"))
      xml     <- steps.parseBody(content, Format)
      imported <- model(bytes, archive, content, xml)
    yield imported

  /** The DOM walk is the last place a malformed document can surface an unchecked exception, so it is the one place
    * `catchNonFatal` stands guard.
    */
  private def model(
    bytes: Array[Byte],
    archive: ArchiveContents,
    content: Array[Byte],
    xml: org.w3c.dom.Document
  ): Either[RichTextCodecException, RichTextImport] =
    Either
      .catchNonFatal {
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
        RichTextImport(document, FidelityReport.forSave(document, SaveTarget.Odt))
      }
      .left
      .map(error => RichTextCodecException(s"$Format document could not be decoded", error))

  private def bodyParagraph(element: Element, index: Int, context: OdtReadContext): Option[RichTextParagraph] =
    BodyBlocks.odt(element) match
      case BodyKind.Paragraph      => Some(OdtParagraphReader.read(element, index, context))
      case BodyKind.Block(feature) => Some(BodyBlocks.line(context.raw(element), feature, index))
      case BodyKind.Structural     => None

  /** The package bytes for `document`: its source package with the edits applied when it has one, else a new package.
    */
  def encode(document: RichTextDocument): Either[RichTextCodecException, Array[Byte]] =
    val normalized = document.normalized
    normalized.source
      .filter(_.format == PackageFormat.Odt)
      .fold(newPackage(normalized))(OdtPackageWriter.rewrite(normalized, _))

  /** [[encode]] for callers that cannot take an `Either`; it throws the `Left`. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def writeBytes(document: RichTextDocument): Array[Byte] =
    encode(document).fold(error => throw error, identity)

  private def newPackage(document: RichTextDocument): Either[RichTextCodecException, Array[Byte]] =
    val output = ByteArrayOutputStream()
    Using(ZipOutputStream(output, StandardCharsets.UTF_8)) { zip =>
      writeZipEntry(zip, "mimetype", "application/vnd.oasis.opendocument.text")
      writeZipEntry(zip, "META-INF/manifest.xml", manifestXml)
      writeZipEntry(zip, ContentEntry, OdtPackageWriter.newContentXml(document))
    }.toEither.left
      .map(error => RichTextCodecException(s"$Format archive could not be written", error))
      .map(_ => output.toByteArray)

  private def manifestXml: String =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<manifest:manifest
      |    xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0"
      |    manifest:version="1.2">
      |  <manifest:file-entry manifest:media-type="application/vnd.oasis.opendocument.text" manifest:full-path="/"/>
      |  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="content.xml"/>
      |</manifest:manifest>""".stripMargin

  private def writeZipEntry(zip: ZipOutputStream, name: String, content: String): Unit =
    zip.putNextEntry(NewPackageEntry.named(name))
    zip.write(content.getBytes(StandardCharsets.UTF_8))
    zip.closeEntry()
