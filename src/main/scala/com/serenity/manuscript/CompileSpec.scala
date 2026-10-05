package com.serenity.manuscript

import java.util.Locale

import com.serenity.richtext.RichTextDocument

/** A source as the compiler sees it: an immutable snapshot, so compiling can never change the buffer it came from. */
enum SourceDocument:
  case Markdown(text: String)
  case Rich(document: RichTextDocument)

/** A source file named in `manuscript.conf`, relative to that file. Excluded files are skipped and take no number. */
final case class SourceEntry(path: String, include: Boolean)

/** Whole-text changes made while compiling. They run in the order the cases are declared, whatever order a conf file
  * lists them in, so `EmDashAsDoubleHyphen` sees the em dashes `SmartPunctuation` made and `StraightPunctuation` never
  * turns an em dash into three hyphens first.
  */
enum TextTransform(val key: String):
  case SmartPunctuation     extends TextTransform("smart-punctuation")
  case EmDashAsDoubleHyphen extends TextTransform("em-dash-as-double-hyphen")
  case StraightPunctuation  extends TextTransform("straight-punctuation")
  case ItalicsAsUnderline   extends TextTransform("italics-as-underline")

object TextTransform:
  def fromKey(key: String): Option[TextTransform] =
    TextTransform.values.find(_.key == key.trim.toLowerCase)

/** A find-and-replace applied to the prose (not to code), literally or as a Java regular expression. */
final case class Replacement(find: String, replace: String, regex: Boolean)

/** A heading pattern. `<$n>` is the section's number, `<$R>`/`<$r>` the same in upper/lower-case Roman numerals, `<$t>`
  * the heading as written in the source. A newline starts another heading line; lines left empty are dropped.
  */
final case class HeadingTemplate(pattern: String):

  def render(number: Int, title: String): Option[SectionHeading] =
    val lines = pattern
      .split("\n", -1)
      .toVector
      .map(
        _.replace("<$n>", number.toString)
          .replace("<$R>", HeadingTemplate.roman(number))
          .replace("<$r>", HeadingTemplate.roman(number).toLowerCase(Locale.ROOT))
          .replace("<$t>", title)
          .trim
      )
      .filter(_.nonEmpty)
    Option.when(lines.nonEmpty)(SectionHeading(lines))

object HeadingTemplate:
  val SourceTitle: HeadingTemplate = HeadingTemplate("<$t>")

  private val RomanDigits = List(
    1000 -> "M",
    900  -> "CM",
    500  -> "D",
    400  -> "CD",
    100  -> "C",
    90   -> "XC",
    50   -> "L",
    40   -> "XL",
    10   -> "X",
    9    -> "IX",
    5    -> "V",
    4    -> "IV",
    1    -> "I"
  )

  /** Roman numerals stop at 3999; past that, and below 1, the arabic number is used instead. */
  def roman(number: Int): String =
    if number < 1 || number > 3999 then number.toString
    else
      RomanDigits
        .foldLeft((number, "")) {
          case ((remaining, numeral), (value, digits)) =>
            (remaining % value, numeral + digits * (remaining / value))
        }
        ._2

enum CompileError(val message: String):
  case InvalidConfiguration(detail: String) extends CompileError(s"manuscript.conf is not valid: $detail")
  case InvalidReplacement(pattern: String, detail: String)
      extends CompileError(s"Replacement pattern '$pattern' is not a valid regular expression: $detail")
  case NothingToCompile extends CompileError("There is no text to export")

/** Everything that decides what a compiled manuscript contains and how it is laid out. */
final case class CompileSpec(
    title: String,
    shortTitle: Option[String],
    author: AuthorName,
    byline: Option[String],
    contact: List[String],
    sources: List[SourceEntry],
    rules: SectionRules,
    chapterHeading: HeadingTemplate,
    partHeading: HeadingTemplate,
    transforms: List[TextTransform],
    replacements: List[Replacement],
    titlePage: Boolean,
    wordCountRounding: WordCountRounding,
    dedication: Option[String],
    endMarker: Option[String],
    format: ManuscriptFormat
):
  def includedSourcePaths: List[String] =
    sources.filter(_.include).map(_.path)

object CompileSpec:

  /** The spec for exporting one document with no `manuscript.conf`: the modern preset, a title page, chapters at
    * top-level headings, and headings as written.
    */
  def forTitle(title: String): CompileSpec =
    CompileSpec(
      title = title,
      shortTitle = None,
      author = AuthorName.unknown,
      byline = None,
      contact = Nil,
      sources = Nil,
      rules = SectionRules.default,
      chapterHeading = HeadingTemplate.SourceTitle,
      partHeading = HeadingTemplate.SourceTitle,
      transforms = ManuscriptFormat.Modern.defaultTransforms,
      replacements = Nil,
      titlePage = true,
      wordCountRounding = WordCountRounding.Novel,
      dedication = None,
      endMarker = Some("END"),
      format = ManuscriptFormat.Modern
    )
