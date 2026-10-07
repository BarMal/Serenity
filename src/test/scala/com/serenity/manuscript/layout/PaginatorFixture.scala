package com.serenity.manuscript.layout

import com.serenity.manuscript.typography.{FontSpec, LineMetrics, MeasureError, PageTypography, TextMeasurer}
import com.serenity.manuscript.{
  AuthorName,
  Block,
  FrontMatter,
  Manuscript,
  ManuscriptMeta,
  ParagraphKind,
  Section,
  SectionHeading,
  WordCountRounding
}
import com.serenity.richtext.RichTextRun

/** Every character is `advance` points wide and a line is 12 points tall, so on the Courier preset (double spacing) the
  * pitch is 24 points and the 468-point measure holds 78 characters.
  */
final class FixedAdvanceMeasurer(advance: Float = 6f, missing: Set[Int] = Set.empty) extends TextMeasurer:

  def advances(text: String, font: FontSpec): Either[MeasureError, IArray[Float]] =
    text.codePoints.toArray.find(missing.contains) match
      case Some(codePoint) => Left(MeasureError.MissingGlyph(codePoint, font.family, font.style))
      case None            => Right(IArray.fill(text.codePointCount(0, text.length))(advance))

  def lineMetrics(font: FontSpec): Either[MeasureError, LineMetrics] =
    Right(LineMetrics(ascent = 10f, descent = 2f, lineGap = 0f))

object PaginatorFixture:

  val typography: PageTypography = PageTypography.StandardManuscriptCourier

  val measurer: TextMeasurer = FixedAdvanceMeasurer()

  val pitch: Float       = 24f
  val chapterDrop: Float = 216f

  val meta: ManuscriptMeta = ManuscriptMeta(
    title = "The Long Night",
    shortTitle = "NIGHT",
    author = AuthorName.fromLegal("Jane Q. Writer"),
    byline = "Jane Q. Writer",
    contact = List("1 High Street", "Springfield"),
    wordCount = 86412,
    wordCountRounding = WordCountRounding.Novel
  )

  def manuscript(
    sections: Vector[Section],
    front: List[FrontMatter] = Nil,
    endMarker: Option[String] = None
  ): Manuscript =
    Manuscript(meta, front, sections, endMarker)

  def chapter(title: String, blocks: Block*): Section =
    Section.Chapter(Some(SectionHeading.of(title)), blocks.toVector)

  def prose(text: String): Block = Block.Paragraph(List(RichTextRun(text)), ParagraphKind.Body)

  /** A paragraph of exactly `lines` lines, each starting with `marker`: every word is 70 characters, so no two share a
    * line of the 78-character measure.
    */
  def paragraphOf(marker: Char, lines: Int): Block =
    prose(Vector.fill(lines)(marker.toString * 70).mkString(" "))

  def paginate(m: Manuscript, t: PageTypography = typography): PagedDocument =
    Paginator
      .paginate(m, t, measurer)
      .fold(error => sys.error(error.message), identity)

  /** One character per line, the first of its text, so a page reads as a string such as `Haaabb`. */
  def marks(page: Page): String =
    page.lines.flatMap(_.text.headOption).mkString

  def visibleRight(line: PlacedLine, advance: Float = 6f): Float =
    line.runs.lastOption.fold(0f)(run => run.x + advance * run.text.stripTrailing.length)
