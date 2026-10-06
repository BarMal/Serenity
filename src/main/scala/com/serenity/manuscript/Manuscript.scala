package com.serenity.manuscript

import java.util.Locale

import com.serenity.richtext.RichTextRun
import com.serenity.text.TextStatistics

/** A compiled book, independent of any output format. Writers (DOCX now; EPUB and PDF later) consume only this. */
final case class Manuscript(
    meta: ManuscriptMeta,
    front: List[FrontMatter],
    body: Vector[Section],
    endMarker: Option[String]
)

final case class AuthorName(legal: String, surname: String)

object AuthorName:
  val unknown: AuthorName = AuthorName("", "")

  /** The surname is the last word of the legal name; a conf file can name it explicitly when that guess is wrong. */
  def fromLegal(legal: String): AuthorName =
    val trimmed = legal.trim
    AuthorName(trimmed, trimmed.split("\\s+").lastOption.getOrElse(""))

/** `shortTitle` is the running-header keyword; `byline` is the name printed under the title, which may be a pen name.
  * `language` is a BCP 47 tag and `identifier` a book's permanent id, both for reflowable formats; with no identifier
  * the EPUB writer derives a stable one from the title and author. `labels` override the navigation headings that
  * `language` would otherwise choose.
  */
final case class ManuscriptMeta(
    title: String,
    shortTitle: String,
    author: AuthorName,
    byline: String,
    contact: List[String],
    wordCount: Int,
    wordCountRounding: WordCountRounding,
    language: String = "en",
    identifier: Option[String] = None,
    labels: Map[LabelKey, String] = Map.empty
):
  def wordCountLine: String = wordCountRounding.describe(wordCount)

/** How the title page rounds the word count: to the nearest 1,000 for a novel and the nearest 100 for short fiction,
  * never below one unit for a non-empty text.
  */
enum WordCountRounding(val key: String, val unit: Int):
  case Novel        extends WordCountRounding("novel", 1000)
  case ShortFiction extends WordCountRounding("short-fiction", 100)
  case Exact        extends WordCountRounding("exact", 1)

  def rounded(count: Int): Int =
    if count <= 0 then 0 else (math.round(count.toDouble / unit).toInt * unit).max(unit)

  def describe(count: Int): String =
    val words = String.format(Locale.US, "%,d words", Int.box(rounded(count)))
    if this == Exact then words else s"about $words"

object WordCountRounding:
  def fromKey(key: String): Option[WordCountRounding] =
    WordCountRounding.values.find(_.key == key.trim.toLowerCase)

/** The files a manuscript can be exported as. */
enum ManuscriptFileFormat(val key: String, val extension: String, val label: String):
  case Docx extends ManuscriptFileFormat("docx", "docx", "Word document (DOCX)")
  case Epub extends ManuscriptFileFormat("epub", "epub", "E-book (EPUB 3)")

enum FrontMatter:
  case TitlePage
  case Dedication(text: String)

/** Each line renders as its own centred paragraph, so a template like `Chapter <$n>\n<$t>` gives a two-line heading. */
final case class SectionHeading(lines: Vector[String]):
  def title: String = lines.mkString(" ")

object SectionHeading:
  def of(title: String): SectionHeading = SectionHeading(Vector(title))

enum Section:
  case Part(heading: Option[SectionHeading], sections: Vector[Section])
  case Chapter(heading: Option[SectionHeading], blocks: Vector[Block])

enum ParagraphKind:
  case Body
  case BlockQuote
  case Centered

/** A scene break is a block of its own rather than a paragraph holding `#`, so each writer picks its own glyph. */
enum Block:
  case Paragraph(runs: List[RichTextRun], kind: ParagraphKind)
  case SceneBreak
  case Preformatted(lines: Vector[String])

object ManuscriptText:

  /** Every chapter in reading order, with parts flattened away. */
  def chapters(sections: Vector[Section]): Vector[Section.Chapter] =
    sections.flatMap {
      case chapter: Section.Chapter => Vector(chapter)
      case Section.Part(_, nested)  => chapters(nested)
    }

  /** The prose of every paragraph and code line, one entry per paragraph or line, headings and scene breaks excluded.
    */
  def bodyTexts(sections: Vector[Section]): Vector[String] =
    chapters(sections).flatMap(_.blocks.flatMap(blockTexts))

  def wordCount(sections: Vector[Section]): Int =
    bodyTexts(sections).map(text => TextStatistics.ofString(text).wordCount).sum

  def plainText(runs: List[RichTextRun]): String =
    runs.map(_.text).mkString

  private def blockTexts(block: Block): Vector[String] =
    block match
      case Block.Paragraph(runs, _)  => Vector(plainText(runs))
      case Block.SceneBreak          => Vector.empty
      case Block.Preformatted(lines) => lines
