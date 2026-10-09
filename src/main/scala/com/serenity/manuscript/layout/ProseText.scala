package com.serenity.manuscript.layout

import com.serenity.richtext.{RichTextRun, RichTextStyle}

/** Makes prose measurable. Ordinary documents carry characters no font has a glyph for -- a line break, a tab, a stray
  * control code -- and none of them may fail an export:
  *   - a line break (`\n`, as a Markdown hard break or `<br>` produces, U+2028, or a vertical tab) ends the line;
  *   - a leading tab is the paragraph's first-line indent, and any other tab a single space;
  *   - every other control or format character is dropped.
  */
private[layout] object ProseText:

  /** `lines` are the paragraph's lines before wrapping, with their styles kept. */
  final case class Prepared(indented: Boolean, lines: Vector[List[RichTextRun]])

  private enum Item:
    case Glyph(char: Char, style: RichTextStyle)
    case Tab(style: RichTextStyle)
    case Break

  def prepare(runs: List[RichTextRun]): Prepared =
    val items = runs.toVector.flatMap(run => run.text.toVector.flatMap(classify(_, run.style)))
    val leadingTabs = items.takeWhile {
      case _: Item.Tab => true
      case _           => false
    }
    val rest     = items.drop(leadingTabs.size).map(spaceForTab)
    val segments = rest.foldLeft(Vector(Vector.empty[Item.Glyph]))(split)
    val trimmedEnd =
      if segments.size > 1 && segments.lastOption.exists(_.isEmpty) then segments.dropRight(1) else segments
    val lines = if rest.isEmpty then Vector.empty else trimmedEnd
    Prepared(leadingTabs.nonEmpty, lines.map(regroup))

  /** Replaces each tab with the spaces that reach the next multiple of `width` columns, so code keeps its indentation.
    * Neither the manuscript nor the typography carries a tab width, so callers pass the editor's default of 4.
    */
  def expandTabs(line: String, width: Int): String =
    val step = width.max(1)
    line.foldLeft("")((out, char) => if char == '\t' then out + " " * (step - out.length % step) else out + char)

  /** A single line of text with no layout meaning for its breaks or tabs, such as a title or a name. */
  def clean(text: String): String =
    text.toVector
      .flatMap(classify(_, RichTextStyle.empty))
      .collect {
        case Item.Glyph(char, _) => char
        case _: Item.Tab         => ' '
        case Item.Break          => ' '
      }
      .mkString

  private def classify(char: Char, style: RichTextStyle): Option[Item] =
    char match
      case '\n' | '\u2028' | '\u000B' => Some(Item.Break)
      case '\t'                       => Some(Item.Tab(style))
      case other =>
        val kind = Character.getType(other)
        Option.unless(kind == Character.CONTROL || kind == Character.FORMAT)(Item.Glyph(other, style))

  private def spaceForTab(item: Item): Item =
    item match
      case Item.Tab(style) => Item.Glyph(' ', style)
      case other           => other

  private def split(segments: Vector[Vector[Item.Glyph]], item: Item): Vector[Vector[Item.Glyph]] =
    item match
      case glyph: Item.Glyph =>
        segments.dropRight(1) :+ (segments.lastOption.getOrElse(Vector.empty) :+ glyph)
      case _ => segments :+ Vector.empty

  private def regroup(glyphs: Vector[Item.Glyph]): List[RichTextRun] =
    glyphs
      .foldLeft(List.empty[RichTextRun]) {
        case (RichTextRun(text, style, None) :: earlier, Item.Glyph(char, next)) if style == next =>
          RichTextRun(text + char, style) :: earlier
        case (runs, Item.Glyph(char, style)) => RichTextRun(char.toString, style) :: runs
      }
      .reverse
