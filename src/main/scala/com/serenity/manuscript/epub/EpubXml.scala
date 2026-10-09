package com.serenity.manuscript.epub

import com.serenity.manuscript.ManuscriptText
import com.serenity.richtext.{InlineMark, RichTextRun}

/** XML text and inline markup for XHTML and OPF content. */
private[epub] object EpubXml:

  /** Escapes the five predefined entities and drops characters XML 1.0 cannot carry at all (stray control characters
    * from pasted text would otherwise make the whole chapter unparseable).
    */
  def escape(text: String): String =
    text.codePoints.toArray.iterator
      .filter(isXmlChar)
      .map {
        case 0x26  => "&amp;"
        case 0x3c  => "&lt;"
        case 0x3e  => "&gt;"
        case 0x22  => "&quot;"
        case 0x27  => "&#39;"
        case point => String(Character.toChars(point))
      }
      .mkString

  /** For metadata and titles, which are single lines: whitespace runs, newlines included, collapse to one space. */
  def line(text: String): String =
    escape(text.replaceAll("\\s+", " ").trim)

  def runs(runs: List[RichTextRun]): String =
    runs.filter(_.text.nonEmpty).map(run).mkString

  def plain(runs: List[RichTextRun]): String =
    ManuscriptText.plainText(runs)

  private def run(run: RichTextRun): String =
    val marks = run.style.marks
    val text  = escape(run.text).replace("\n", "<br/>")
    val underlined =
      if marks.contains(InlineMark.Underline) then s"""<span class="underline">$text</span>""" else text
    val emphasised = if marks.contains(InlineMark.Italic) then s"<em>$underlined</em>" else underlined
    if marks.contains(InlineMark.Bold) then s"<strong>$emphasised</strong>" else emphasised

  private def isXmlChar(point: Int): Boolean =
    point == 0x9 || point == 0xa || point == 0xd || (point >= 0x20 && point <= 0xd7ff) ||
      (point >= 0xe000 && point <= 0xfffd) || (point >= 0x10000 && point <= 0x10ffff)
