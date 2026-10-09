package com.serenity.richtext

import scala.annotation.tailrec

/** Where one element sits in the decoded text of an XML part. `openEnd` is the index after the start tag's `>`; `end`
  * is the index after the end tag (or after the start tag of an empty-element tag).
  */
final private[richtext] case class XmlSpan(
    start: Int,
    openEnd: Int,
    end: Int,
    qualifiedName: String,
    parent: Int,
    selfClosing: Boolean
):
  def closeStart(text: String): Int =
    if selfClosing then end else text.lastIndexOf('<', end - 1)

/** The elements of an already well-formed XML part, in document order, with their character offsets. DOM and StAX do
  * not give reliable offsets, and byte-exact passthrough of untouched paragraphs needs them, so this scans the text
  * once. It trusts the input: the secure DOM parse has already rejected anything malformed.
  */
final private[richtext] class XmlSpans(val text: String, val spans: Vector[XmlSpan]):
  def raw(index: Int): String =
    spans.lift(index).fold("")(span => text.substring(span.start, span.end))

  private lazy val childIndexes: Map[Int, Vector[Int]] =
    spans.indices.toVector.groupBy(spans(_).parent)

  def children(index: Int): Vector[Int] =
    childIndexes.getOrElse(index, Vector.empty)

  def openTag(index: Int): String =
    spans.lift(index).fold("")(span => text.substring(span.start, span.openEnd))

  def closeTag(index: Int): String =
    spans.lift(index).fold("")(span => if span.selfClosing then "" else text.substring(span.closeStart(text), span.end))

  /** The text between the start tag and the end tag. */
  def inner(index: Int): String =
    spans.lift(index).fold("")(span => text.substring(span.openEnd, span.closeStart(text).max(span.openEnd)))

private[richtext] object XmlSpans:

  def scan(text: String): XmlSpans =
    XmlSpans(text, next(text, 0, Nil, Vector.empty))

  @tailrec
  private def next(text: String, from: Int, open: List[Int], spans: Vector[XmlSpan]): Vector[XmlSpan] =
    val lt = text.indexOf('<', from)
    if lt < 0 then spans
    else if text.startsWith("<!--", lt) then next(text, afterMarker(text, lt, "-->"), open, spans)
    else if text.startsWith("<![CDATA[", lt) then next(text, afterMarker(text, lt, "]]>"), open, spans)
    else if text.startsWith("<?", lt) then next(text, afterMarker(text, lt, "?>"), open, spans)
    else if text.startsWith("</", lt) then
      val end = text.indexOf('>', lt) + 1
      open match
        case top :: rest => next(text, end, rest, spans.updated(top, spans(top).copy(end = end)))
        case Nil         => spans
    else
      val tagEnd      = endOfTag(text, lt + 1, quote = None)
      val selfClosing = text.charAt(tagEnd - 2) == '/'
      val name        = text.substring(lt + 1, nameEnd(text, lt + 1))
      val span        = XmlSpan(lt, tagEnd, tagEnd, name, open.headOption.getOrElse(-1), selfClosing)
      val added       = spans :+ span
      next(text, tagEnd, if selfClosing then open else (added.size - 1) :: open, added)

  private def afterMarker(text: String, from: Int, terminator: String): Int =
    val found = text.indexOf(terminator, from)
    if found < 0 then text.length else found + terminator.length

  private def nameEnd(text: String, from: Int): Int =
    val offset = text.indexWhere(char => char.isWhitespace || char == '/' || char == '>', from)
    if offset < 0 then text.length else offset

  /** Index after the `>` closing the tag, skipping any `>` inside a quoted attribute value. */
  @tailrec
  private def endOfTag(text: String, from: Int, quote: Option[Char]): Int =
    if from >= text.length then text.length
    else
      val char = text.charAt(from)
      quote match
        case Some(open) => endOfTag(text, from + 1, Option.when(char != open)(open))
        case None =>
          if char == '>' then from + 1
          else endOfTag(text, from + 1, Option.when(char == '"' || char == '\'')(char))
