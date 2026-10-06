package com.serenity.exporting

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.time.Instant
import java.util.{GregorianCalendar, TimeZone}

import scala.util.Using

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.serenity.manuscript.layout.{Page, PagedDocument, PlacedLine, PlacedRun}
import com.serenity.manuscript.typography.{FaceStyle, FontFamily, PageTypography}
import com.serenity.manuscript.{Manuscript, ManuscriptMeta}
import com.serenity.richtext.InlineMark
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.font.PDType0Font
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.{PDDocumentOutline, PDOutlineItem}
import org.apache.pdfbox.pdmodel.{PDDocument, PDDocumentInformation, PDPage, PDPageContentStream, PageMode}

/** A run the painter could not draw, such as a character the embedded face has no glyph for. */
final case class PdfPaintError(message: String) extends Exception(message)

/** Paints a [[PagedDocument]] into a PDF: every run at the baseline and x the paginator chose, in subset fonts embedded
  * from the same bundled files the measurer read, so what was laid out is what is drawn. The document's creation date
  * is injected and also seeds the file ID, so the same input and date always give the same bytes.
  */
object PdfPainter:

  final private case class Face(family: FontFamily, style: FaceStyle)

  final private case class Shade(red: Float, green: Float, blue: Float)

  private type Faces = Map[Face, PDType0Font]

  private val UnderlineOffset = 1.5f
  private val UnderlineWidth  = 0.5f

  def paint(
    manuscript: Manuscript,
    paged: PagedDocument,
    typography: PageTypography,
    created: Instant
  ): IO[Array[Byte]] =
    document.use { doc =>
      embeddedFaces(doc, paged).flatMap { faces =>
        IO.blocking(render(doc, manuscript, paged, typography, faces, created)).flatMap(IO.fromEither)
      }
    }

  private val document: Resource[IO, PDDocument] =
    Resource.make(IO.blocking(PDDocument()))(doc => IO.blocking(doc.close()))

  private def embeddedFaces(doc: PDDocument, paged: PagedDocument): IO[Faces] =
    val used = paged.pages.flatMap(page => page.head.toVector ++ page.lines).flatMap(_.runs).map(faceOf).distinct
    used
      .traverse(face => embed(doc, face).map(face -> _))
      .map(_.toMap)

  private def faceOf(run: PlacedRun): Face = Face(run.font.family, run.font.style)

  private def embed(doc: PDDocument, face: Face): IO[PDType0Font] =
    BundledFonts
      .read(face.family, face.style)
      .flatMap(bytes => IO.blocking(PDType0Font.load(doc, ByteArrayInputStream(bytes), true)))

  private def render(
    doc: PDDocument,
    manuscript: Manuscript,
    paged: PagedDocument,
    typography: PageTypography,
    faces: Faces,
    created: Instant
  ): Either[PdfPaintError, Array[Byte]] =
    describe(doc, manuscript.meta, created)
    paged.pages.traverse_(paintPage(doc, _, typography, faces)).map { _ =>
      outline(doc, PdfOutline.entries(manuscript, paged))
      bytesOf(doc)
    }

  private def bytesOf(doc: PDDocument): Array[Byte] =
    val out = ByteArrayOutputStream()
    doc.save(out)
    out.toByteArray

  /** The file ID is derived from the document ID and the info dictionary, so fixing the first fixes the ID. */
  private def describe(doc: PDDocument, meta: ManuscriptMeta, created: Instant): Unit =
    val info = PDDocumentInformation()
    info.setTitle(meta.title)
    Some(meta.author.legal).filter(_.nonEmpty).orElse(Some(meta.byline).filter(_.nonEmpty)).foreach(info.setAuthor)
    info.setCreator("Serenity")
    val stamp = GregorianCalendar(TimeZone.getTimeZone("UTC"))
    stamp.setTimeInMillis(created.toEpochMilli)
    info.setCreationDate(stamp)
    info.setModificationDate(stamp)
    doc.setDocumentInformation(info)
    doc.getDocumentCatalog.setLanguage(meta.language)
    doc.setDocumentId(created.toEpochMilli)

  private def paintPage(
    doc: PDDocument,
    page: Page,
    typography: PageTypography,
    faces: Faces
  ): Either[PdfPaintError, Unit] =
    val sheet = PDPage(PDRectangle(typography.pageWidth, typography.pageHeight))
    doc.addPage(sheet)
    Using.resource(PDPageContentStream(doc, sheet)) { stream =>
      (page.head.toVector ++ page.lines).traverse_(paintLine(stream, _, typography.pageHeight, faces))
    }

  private def paintLine(
    stream: PDPageContentStream,
    line: PlacedLine,
    pageHeight: Float,
    faces: Faces
  ): Either[PdfPaintError, Unit] =
    line.runs.filter(_.text.nonEmpty).traverse_(paintRun(stream, _, pageHeight - line.baselineY, faces))

  private def paintRun(
    stream: PDPageContentStream,
    run: PlacedRun,
    y: Float,
    faces: Faces
  ): Either[PdfPaintError, Unit] =
    faces
      .get(faceOf(run))
      .toRight(PdfPaintError(s"${run.font.family.displayName} ${run.font.style} is not embedded"))
      .flatMap { font =>
        Either
          .catchOnly[IllegalArgumentException | IOException](drawRun(stream, font, run, y))
          .leftMap(failure => PdfPaintError(Option(failure.getMessage).getOrElse("a run could not be drawn")))
      }

  /** A coloured run is drawn between a save and a restore, so its colour does not leak into the runs after it. */
  private def drawRun(stream: PDPageContentStream, font: PDType0Font, run: PlacedRun, y: Float): Unit =
    val colour = run.style.color.flatMap(parseColour)
    colour.foreach { shade =>
      stream.saveGraphicsState()
      stream.setNonStrokingColor(shade.red, shade.green, shade.blue)
      stream.setStrokingColor(shade.red, shade.green, shade.blue)
    }
    stream.beginText()
    stream.setFont(font, run.font.sizePoints)
    stream.newLineAtOffset(run.x, y)
    stream.showText(run.text)
    stream.endText()
    if run.style.marks.contains(InlineMark.Underline) then underline(stream, font, run, y)
    if colour.isDefined then stream.restoreGraphicsState()

  private def underline(stream: PDPageContentStream, font: PDType0Font, run: PlacedRun, y: Float): Unit =
    val width = font.getStringWidth(run.text.stripTrailing) / 1000f * run.font.sizePoints
    stream.setLineWidth(UnderlineWidth)
    stream.moveTo(run.x, y - UnderlineOffset)
    stream.lineTo(run.x + width, y - UnderlineOffset)
    stream.stroke()

  /** `#RRGGBB` or `RRGGBB`; anything else leaves the run in the default black. */
  private def parseColour(text: String): Option[Shade] =
    Some(text.trim.stripPrefix("#"))
      .filter(_.matches("[0-9A-Fa-f]{6}"))
      .map(Integer.parseInt(_, 16))
      .map(rgb => Shade(channel(rgb, 16), channel(rgb, 8), channel(rgb, 0)))

  private def channel(rgb: Int, shift: Int): Float = ((rgb >> shift) & 0xff) / 255f

  private def outline(doc: PDDocument, entries: Vector[OutlineEntry]): Unit =
    if entries.nonEmpty then
      val root = PDDocumentOutline()
      entries.foreach(entry => root.addLast(outlineItem(doc, entry)))
      root.openNode()
      doc.getDocumentCatalog.setDocumentOutline(root)
      doc.getDocumentCatalog.setPageMode(PageMode.USE_OUTLINES)

  private def outlineItem(doc: PDDocument, entry: OutlineEntry): PDOutlineItem =
    val item = PDOutlineItem()
    item.setTitle(entry.title)
    item.setDestination(doc.getPage(entry.pageIndex))
    entry.children.foreach(child => item.addLast(outlineItem(doc, child)))
    if entry.children.nonEmpty then item.openNode()
    item
