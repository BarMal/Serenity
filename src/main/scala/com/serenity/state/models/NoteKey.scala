package com.serenity.state.models

import com.serenity.ui.layout.{Symbol, SymbolKind}

/** What a note describes. Neither case stores a position: a chapter is found again by its heading and a keyword by its
  * text, so a note needs no remapping as the manuscript is edited around it.
  */
enum NoteKey:
  case Chapter(heading: HeadingIdentity)
  case Keyword(term: String)

/** A chapter heading, independent of where it sits and of the number in front of it: `title` is the normalised title
  * and `occurrence` counts earlier headings with the same title, so repeated titles ("Interlude") stay distinct.
  *
  * Retitling a chapter detaches its note, because the title is all there is to find it by.
  */
final case class HeadingIdentity(title: String, occurrence: Int)

object HeadingIdentity:

  private val ChapterNumberPrefix = """^chapter\s+\d+\s*[:.–—-]?\s*""".r

  /** Lower-cased, whitespace-collapsed, with a leading "Chapter <n>" and its separator removed, so renumbering never
    * changes a chapter's identity. A heading that is only a chapter number normalises to the empty string, and its
    * `occurrence` then follows the chapter order.
    */
  def normalizedTitle(heading: String): String =
    val collapsed = heading.trim.toLowerCase(java.util.Locale.ROOT).split("\\s+").mkString(" ")
    ChapterNumberPrefix.replaceFirstIn(collapsed, "").trim

  /** Every heading among `symbols` with its identity, in document order. */
  def forHeadings(symbols: List[Symbol]): List[(HeadingIdentity, Symbol)] =
    val headings = symbols.filter(_.kind == SymbolKind.Heading)
    headings
      .foldLeft((List.empty[(HeadingIdentity, Symbol)], Map.empty[String, Int])) {
        case ((found, seen), heading) =>
          val title      = normalizedTitle(heading.name)
          val occurrence = seen.getOrElse(title, 0)
          ((HeadingIdentity(title, occurrence), heading) :: found, seen.updated(title, occurrence + 1))
      }
      ._1
      .reverse

  def resolve(identity: HeadingIdentity, symbols: List[Symbol]): Option[Symbol] =
    forHeadings(symbols).collectFirst { case (found, heading) if found == identity => heading }

/** The hidden buffers holding one note's text: the overview shown as the ghost under an empty chapter, and optionally a
  * longer set of notes.
  */
final case class Notes(overview: BufferId, extra: Option[BufferId] = None):
  def bufferIds: List[BufferId] = overview :: extra.toList
