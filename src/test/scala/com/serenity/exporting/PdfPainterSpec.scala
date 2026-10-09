package com.serenity.exporting

import java.time.Instant
import java.util.List as JList

import scala.jdk.CollectionConverters.*
import scala.util.Using

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.manuscript.layout.PaginatorFixture.{chapter, manuscript, prose}
import com.serenity.manuscript.layout.{Page, PageKind, PagedDocument, Paginator, PlacedLine, PlacedRun}
import com.serenity.manuscript.typography.{FaceStyle, FontFamily, FontSpec, PageTypography}
import com.serenity.manuscript.{FrontMatter, Manuscript, Section, SectionHeading}
import com.serenity.richtext.{InlineMark, RichTextStyle}
import org.apache.pdfbox.Loader
import org.apache.pdfbox.contentstream.operator.Operator
import org.apache.pdfbox.cos.{COSName, COSNumber}
import org.apache.pdfbox.pdfparser.PDFStreamParser
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.PDOutlineItem
import org.apache.pdfbox.text.{PDFTextStripper, TextPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, OptionValues}

/** Paints real Courier Prime pages and reads the PDF back with PDFBox itself. */
class PdfPainterSpec extends AnyFlatSpec with Matchers with EitherValues with OptionValues:

  private val typography = PageTypography.StandardManuscriptCourier
  private val created    = Instant.parse("2026-10-05T12:30:00Z")

  private def sentences(count: Int): String =
    (1 to count).map(index => s"Sentence $index of the long night goes on.").mkString(" ")

  private val book: Manuscript = manuscript(
    Vector(
      chapter("Arrival", prose(sentences(60)), prose(sentences(10))),
      chapter("Departure", prose(sentences(25)))
    ),
    front = List(FrontMatter.TitlePage, FrontMatter.Dedication("For Ada"))
  )

  private def layout(m: Manuscript): PagedDocument =
    FontBoxTextMeasurer
      .resource()
      .use(measurer => IO(Paginator.paginate(m, typography, measurer)))
      .unsafeRunSync()
      .fold(error => fail(error.message), identity)

  private def paint(m: Manuscript, paged: PagedDocument, at: Instant = created): Array[Byte] =
    PdfPainter.paint(m, paged, typography, at).unsafeRunSync()

  private val paged: PagedDocument = layout(book)
  private val pdf: Array[Byte]     = paint(book, paged)

  private def withPdf[A](bytes: Array[Byte] = pdf)(read: PDDocument => A): A =
    Using.resource(Loader.loadPDF(bytes))(read)

  private def pageLines(doc: PDDocument, pageNumber: Int): List[String] =
    val stripper = PDFTextStripper()
    stripper.setSortByPosition(true)
    stripper.setStartPage(pageNumber)
    stripper.setEndPage(pageNumber)
    stripper.getText(doc).linesIterator.map(_.trim).filter(_.nonEmpty).toList

  private def placedTexts(page: Page): List[String] =
    (page.head.toList ++ page.lines)
      .map(line => line.runs.map(_.text.trim).filter(_.nonEmpty).mkString(" "))
      .filter(_.nonEmpty)
      .toList

  /** One output line per drawn character: its left edge, its baseline measured from the top of the page, and itself. */
  final private class GlyphPositions extends PDFTextStripper:

    override protected def writeString(text: String, positions: JList[TextPosition]): Unit =
      positions.asScala.foreach(glyph =>
        output.write(f"${glyph.getXDirAdj}%.3f ${glyph.getYDirAdj}%.3f ${glyph.getUnicode}%s\n")
      )

    override protected def writeWordSeparator(): Unit = ()
    override protected def writeLineSeparator(): Unit = ()

  private def glyphs(doc: PDDocument, pageNumber: Int): List[(Float, Float, String)] =
    val stripper = GlyphPositions()
    stripper.setSortByPosition(true)
    stripper.setStartPage(pageNumber)
    stripper.setEndPage(pageNumber)
    stripper.getText(doc).linesIterator.toList.map(_.split(" ", 3)).collect {
      case Array(x, y, glyph) => (x.toFloat, y.toFloat, glyph)
    }

  "Painting a paged document" should "keep the layout's page count and paper size" in {
    paged.pages.size should be > 4

    withPdf() { doc =>
      doc.getNumberOfPages shouldBe paged.pages.size
      doc.getPage(0).getMediaBox.getWidth shouldBe typography.pageWidth
      doc.getPage(0).getMediaBox.getHeight shouldBe typography.pageHeight
    }
  }

  it should "extract, page by page, exactly the text of the paged lines" in
    withPdf()(doc => paged.pages.foreach(page => pageLines(doc, page.number) shouldBe placedTexts(page)))

  it should "put every run at its x position and baseline" in
    withPdf() { doc =>
      paged.pages.foreach { page =>
        val drawn = glyphs(doc, page.number)
        (page.head.toList ++ page.lines).foreach { line =>
          line.runs.filter(_.text.trim.nonEmpty).foreach { run =>
            val leading = run.text.indexWhere(!_.isWhitespace)
            val left    = run.x + leading * run.font.sizePoints * 0.6f
            drawn.exists((x, y, glyph) =>
              glyph == run.text.charAt(leading).toString && math.abs(x - left) < 0.05f &&
                math.abs(y - line.baselineY) < 0.05f
            ) shouldBe true
          }
        }
      }
    }

  it should "print Surname / TITLE / 1 on the first body page and nothing on the front matter" in {
    paged.pages.map(_.kind).take(3) shouldBe Vector(PageKind.Title, PageKind.Dedication, PageKind.SectionStart)

    withPdf() { doc =>
      pageLines(doc, 1).exists(_.contains(" / ")) shouldBe false
      pageLines(doc, 2) shouldBe List("For Ada")
      pageLines(doc, 3).headOption shouldBe Some("Writer / NIGHT / 1")
      pageLines(doc, 4).headOption shouldBe Some("Writer / NIGHT / 2")
    }
  }

  it should "embed every font, as a subset" in
    withPdf() { doc =>
      val fonts = doc.getPages.asScala.flatMap { page =>
        page.getResources.getFontNames.asScala.map(name => page.getResources.getFont(name))
      }.toList
      fonts should not be empty
      fonts.foreach(_.isEmbedded shouldBe true)
      fonts.foreach(_.getName should fullyMatch regex "[A-Z]{6}\\+CourierPrime.*")
    }

  it should "embed the face a run asks for, not only the regular one" in {
    val styled = manuscript(
      Vector(chapter("One", prose("Plain text."))),
      front = Nil
    )
    val laid = layout(styled)
    val italic = laid.copy(pages =
      laid.pages.map(page =>
        page.copy(lines =
          page.lines.map(line =>
            line.copy(runs = line.runs.map(run => run.copy(font = run.font.copy(style = FaceStyle.Italic))))
          )
        )
      )
    )

    withPdf(paint(styled, italic)) { doc =>
      doc.getPages.asScala
        .flatMap(page => page.getResources.getFontNames.asScala.map(page.getResources.getFont(_).getName))
        .exists(_.contains("Italic")) shouldBe true
    }
  }

  private def outlineItems(first: PDOutlineItem): List[PDOutlineItem] =
    Iterator
      .iterate(Option(first))(_.flatMap(item => Option(item.getNextSibling)))
      .takeWhile(_.isDefined)
      .flatten
      .toList

  private def topLevel(doc: PDDocument): List[PDOutlineItem] =
    Option(doc.getDocumentCatalog.getDocumentOutline).flatMap(o => Option(o.getFirstChild)).toList.flatMap(outlineItems)

  it should "outline the chapters by heading, each aimed at the page it starts" in
    withPdf() { doc =>
      val items = topLevel(doc)
      items.map(_.getTitle) shouldBe List("Arrival", "Departure")
      val starts = paged.pages.zipWithIndex.collect {
        case (page, index) if page.kind == PageKind.SectionStart => index
      }
      items.map(item => doc.getPages.indexOf(item.findDestinationPage(doc))) shouldBe starts.toList
    }

  it should "nest the chapters of a titled part in the outline" in {
    val parted = manuscript(
      Vector(
        Section.Part(
          Some(SectionHeading.of("Part One")),
          Vector(chapter("Arrival", prose("a")), chapter("Departure", prose("b")))
        )
      )
    )

    withPdf(paint(parted, layout(parted))) { doc =>
      val parts = topLevel(doc)
      parts.map(_.getTitle) shouldBe List("Part One")
      outlineItems(parts.head.getFirstChild).map(_.getTitle) shouldBe List("Arrival", "Departure")
    }
  }

  it should "leave the outline out when no section has a heading" in {
    val bare = manuscript(Vector(Section.Chapter(None, Vector(prose("Just text.")))))

    withPdf(paint(bare, layout(bare)))(doc => Option(doc.getDocumentCatalog.getDocumentOutline) shouldBe None)
  }

  it should "record the title, author, creation date and the language in the document" in
    withPdf() { doc =>
      val info = doc.getDocumentInformation
      info.getTitle shouldBe "The Long Night"
      info.getAuthor shouldBe "Jane Q. Writer"
      info.getCreationDate.toInstant shouldBe created
      info.getModificationDate.toInstant shouldBe created
      info.getCreator shouldBe "Serenity"
      doc.getDocumentCatalog.getLanguage shouldBe "en"
    }

  it should "be byte-for-byte identical for the same creation date, trailer ID included" in {
    val again = paint(book, layout(book))

    again.toSeq shouldBe pdf.toSeq
    withPdf()(doc => doc.getDocument.getTrailer.getCOSArray(COSName.ID).size shouldBe 2)
  }

  it should "differ, and carry a different file ID, for another creation date" in {
    val later = paint(book, paged, created.plusSeconds(60))

    later.toSeq should not be pdf.toSeq
    def fileId(bytes: Array[Byte]): String =
      withPdf(bytes)(_.getDocument.getTrailer.getCOSArray(COSName.ID).toString)
    fileId(later) should not be fileId(pdf)
  }

  private def singleLine(style: RichTextStyle): (Manuscript, PagedDocument) =
    val font = FontSpec(FontFamily.CourierPrime, FaceStyle.Regular, 12f)
    val line = PlacedLine(100f, Vector(PlacedRun("Hello", font, 72f, style)))
    (
      manuscript(Vector(chapter("One", prose("Hello")))),
      PagedDocument(Vector(Page(1, Some(1), PageKind.Body, None, Vector(line))))
    )

  private def operators(doc: PDDocument): List[Operator] =
    PDFStreamParser(doc.getPage(0)).parse().asScala.toList.collect { case operator: Operator => operator }

  private def tokensBefore(doc: PDDocument, name: String): List[Any] =
    val tokens = PDFStreamParser(doc.getPage(0)).parse().asScala.toList
    tokens
      .takeWhile { case operator: Operator => operator.getName != name; case _ => true }
      .reverse
      .takeWhile(!_.isInstanceOf[Operator])
      .reverse

  it should "draw a line under an underlined run, and under nothing else" in {
    val (m, plain) = singleLine(RichTextStyle.empty)
    val (_, under) = singleLine(RichTextStyle.empty.withMark(InlineMark.Underline))

    withPdf(paint(m, plain))(doc => operators(doc).map(_.getName) should not contain "S")
    withPdf(paint(m, under)) { doc =>
      operators(doc).map(_.getName) should contain("S")
      val end = tokensBefore(doc, "l").collect { case n: COSNumber => n.floatValue }
      end.size shouldBe 2
      end.head shouldBe (72f + 5 * 7.2f +- 0.01f)
      end(1) shouldBe (typography.pageHeight - 100f - 1.5f +- 0.01f)
    }
  }

  it should "set a run's colour from its style" in {
    val (m, red)   = singleLine(RichTextStyle.empty.withColor("#FF0000"))
    val (_, plain) = singleLine(RichTextStyle.empty)

    withPdf(paint(m, plain))(doc => operators(doc).map(_.getName) should not contain "rg")
    withPdf(paint(m, red)) { doc =>
      operators(doc).map(_.getName) should contain("rg")
      tokensBefore(doc, "rg").collect { case n: COSNumber => n.floatValue } shouldBe List(1f, 0f, 0f)
    }
  }
