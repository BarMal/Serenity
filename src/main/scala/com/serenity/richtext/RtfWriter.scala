package com.serenity.richtext

import java.nio.charset.StandardCharsets
import java.text.Normalizer

/** Encodes a [[RichTextDocument]] as 7-bit ASCII RTF: non-ASCII text becomes `\uN` escapes with a letter fallback, and
  * headings and drop caps are written as the paragraph properties other RTF readers recognise.
  */
private[richtext] object RtfWriter:
  private val DefaultFontName = "Times New Roman"

  /** Half-point sizes Word's built-in heading styles use, so other applications render headings sensibly. */
  private val HeadingHalfPoints: Map[Int, Int] = Map(1 -> 32, 2 -> 28, 3 -> 26, 4 -> 24)
  private val SmallHeadingHalfPoints           = 22

  final private case class Tables(fonts: Vector[String], colors: Vector[String], headingLevels: Vector[Int])

  def write(document: RichTextDocument): Array[Byte] =
    val paragraphs = document.normalized.paragraphs
    val tables     = tablesFor(paragraphs)
    val header     = Vector(rtfPreamble, fontTable(tables), colorTable(tables), stylesheet(tables))
    val body       = paragraphs.map(paragraphText(_, tables))
    (header ++ body :+ "}").mkString("\n").getBytes(StandardCharsets.US_ASCII)

  private def tablesFor(paragraphs: List[RichTextParagraph]): Tables =
    val styles = paragraphs.flatMap(_.runs).map(_.style)
    Tables(
      fonts = styles.flatMap(_.fontFamily).map(sanitizedFontName).distinct.toVector,
      colors = styles.flatMap(_.color).flatMap(parseColor).distinct.toVector,
      headingLevels = paragraphs
        .collect {
          case RichTextParagraph(_, _, ParagraphRole.Heading(level)) =>
            level.max(1)
        }
        .distinct
        .sorted
        .toVector
    )

  private def rtfPreamble: String =
    "{\\rtf1\\ansi\\ansicpg1252\\uc1\\deff0"

  private def fontTable(tables: Tables): String =
    val entries = (DefaultFontName +: tables.fonts).zipWithIndex.map { (name, index) =>
      s"{\\f$index\\fnil\\fcharset0 ${escape(name)};}"
    }
    s"{\\fonttbl${entries.mkString}}"

  private def colorTable(tables: Tables): String =
    val entries = tables.colors.map { hex =>
      def channel(start: Int): Int = Integer.parseInt(hex.substring(start, start + 2), 16)
      s"\\red${channel(0)}\\green${channel(2)}\\blue${channel(4)};"
    }
    s"{\\colortbl ;${entries.mkString}}"

  private def stylesheet(tables: Tables): String =
    val headings = tables.headingLevels.map { level =>
      val size = HeadingHalfPoints.getOrElse(level, SmallHeadingHalfPoints)
      s"{\\s$level\\sbasedon0\\snext0${outlineWord(level)}\\b\\fs$size heading $level;}"
    }
    s"{\\stylesheet{\\s0 Normal;}${headings.mkString}}"

  private def outlineWord(level: Int): String =
    if level <= 9 then s"\\outlinelevel${level - 1}" else ""

  private def paragraphText(paragraph: RichTextParagraph, tables: Tables): String =
    val runs = paragraph.runs.map(run => runText(run, tables)).mkString
    s"\\pard${roleWords(paragraph.role)}${alignmentWord(paragraph.alignment)} $runs\\par"

  private def roleWords(role: ParagraphRole): String =
    role match
      case ParagraphRole.Body           => ""
      case ParagraphRole.Heading(level) => s"\\s${level.max(1)}${outlineWord(level.max(1))}"
      case ParagraphRole.DropCap(lines) => s"\\dropcapli${lines.max(1)}\\dropcapt1"

  private def alignmentWord(alignment: ParagraphAlignment): String =
    alignment match
      case ParagraphAlignment.Left    => ""
      case ParagraphAlignment.Center  => "\\qc"
      case ParagraphAlignment.Right   => "\\qr"
      case ParagraphAlignment.Justify => "\\qj"

  private def runText(run: RichTextRun, tables: Tables): String =
    val words = styleWords(run.style, tables)
    val text  = run.atom.fold(escape(run.text)) {
      case InlineAtom.SoftBreak    => "\\line "
      case InlineAtom.Opaque(_, _) => ""
    }
    if words.isEmpty || text.isEmpty then text else s"{$words $text}"

  private def styleWords(style: RichTextStyle, tables: Tables): String =
    List(
      style.fontFamily.map(family => s"\\f${tables.fonts.indexOf(sanitizedFontName(family)) + 1}"),
      style.fontSize.map(size => s"\\fs${(size * 2).round}"),
      style.color.flatMap(parseColor).map(hex => s"\\cf${tables.colors.indexOf(hex) + 1}"),
      Option.when(style.marks.contains(InlineMark.Bold))("\\b"),
      Option.when(style.marks.contains(InlineMark.Italic))("\\i"),
      Option.when(style.marks.contains(InlineMark.Underline))("\\ul")
    ).flatten.mkString

  private def sanitizedFontName(name: String): String =
    name.replace(';', ' ')

  private def parseColor(value: String): Option[String] =
    Option(value.stripPrefix("#")).filter(_.matches("[0-9a-fA-F]{6}")).map(_.toLowerCase)

  private def escape(text: String): String =
    text.iterator.map(escapeChar).mkString

  private def escapeChar(char: Char): String =
    char match
      case '\\' | '{' | '}'                                  => s"\\$char"
      case '\n'                                              => "\\line "
      case '\t'                                              => "\\tab "
      case printable if printable >= ' ' && printable <= '~' => printable.toString
      case other                                             => s"\\u${other.toInt.toShort}${fallbackFor(other)}"

  /** The single ASCII character a reader without `\uN` support shows: the unaccented letter when there is one. */
  private def fallbackFor(char: Char): Char =
    Normalizer
      .normalize(char.toString, Normalizer.Form.NFD)
      .headOption
      .filter(base => (base >= 'a' && base <= 'z') || (base >= 'A' && base <= 'Z'))
      .getOrElse('?')
