package com.serenity.document

import com.serenity.state.models.{Buffer, BufferId, HeadingIdentity, NoteKey}

/** The ghost of a chapter note: its overview, shown faded on the blank lines under a chapter that has no prose yet.
  * Computed from the manuscript and its notes alone -- nothing is written into the document, and the ghost goes as soon
  * as the chapter has any text of its own.
  */
object ChapterGhosts:

  /** Overview text keyed by the buffer line it is shown on. Empty for a buffer with no notes, without parsing it, and
    * for a hidden buffer, which has no chapters of its own. Only as many overview lines as there are blank lines under
    * the heading are shown.
    */
  def byLine(buffer: Buffer, buffers: Map[BufferId, Buffer]): Map[Int, String] =
    if buffer.hidden || buffer.annotations.notes.isEmpty then Map.empty
    else
      val headings     = HeadingIdentity.forHeadings(DocumentOutline.forBuffer(buffer))
      val headingLines = headings.map(_._2.location.line)
      val lineOf       = headings.map { case (found, heading) => found -> heading.location.line }.toMap
      val nextHeadingLine: Map[Int, Option[Int]] =
        headingLines.zip(headingLines.drop(1).map(Option.apply) :+ Option.empty[Int]).toMap

      buffer.annotations.notes.toList.flatMap {
        case (NoteKey.Chapter(found), notes) =>
          val ghost =
            for
              headingLine <- lineOf.get(found)
              note        <- buffers.get(notes.overview)
            yield ghostLines(buffer, headingLine, nextHeadingLine.getOrElse(headingLine, None), note)
          ghost.getOrElse(Nil)
        case _ => Nil
      }.toMap

  private def ghostLines(
    buffer: Buffer,
    headingLine: Int,
    nextHeadingLine: Option[Int],
    note: Buffer
  ): List[(Int, String)] =
    val content = buffer.document.content
    val body    = ((headingLine + 1) until nextHeadingLine.getOrElse(content.lineCount)).toList
    val isEmpty = body.forall(line => content.getLine(line).forall(_.isBlank))
    if isEmpty then body.zip(overviewLines(note)) else Nil

  private def overviewLines(note: Buffer): List[String] =
    note.document.content
      .collect()
      .linesIterator
      .map(_.stripTrailing)
      .toList
      .dropWhile(_.isBlank)
      .reverse
      .dropWhile(_.isBlank)
      .reverse
