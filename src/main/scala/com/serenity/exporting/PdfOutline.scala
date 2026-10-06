package com.serenity.exporting

import com.serenity.manuscript.layout.PagedDocument
import com.serenity.manuscript.{Manuscript, Section, SectionHeading}

/** A bookmark: the heading's text, the zero-based page it starts on, and the chapters nested beneath it. */
final case class OutlineEntry(title: String, pageIndex: Int, children: Vector[OutlineEntry])

/** The PDF's bookmarks, from the manuscript's headings and the pages the paginator says each section opens. Sections
  * are numbered depth first, as [[com.serenity.manuscript.layout.SectionRef]] is, so no text is compared. A heading
  * whose section opened no page (an empty chapter) has nothing to point at and gets no bookmark. An untitled part lifts
  * its chapters to its own level.
  */
object PdfOutline:

  final private case class Walk(entries: Vector[OutlineEntry], next: Int)

  def entries(manuscript: Manuscript, paged: PagedDocument): Vector[OutlineEntry] =
    val pageOf = paged.pages.zipWithIndex.flatMap((page, index) => page.opens.map(_.index -> index)).toMap
    walk(manuscript.body, 0, pageOf).entries

  private def walk(sections: Vector[Section], first: Int, pageOf: Map[Int, Int]): Walk =
    sections.foldLeft(Walk(Vector.empty, first)) { (found, section) =>
      val visited = visit(section, found.next, pageOf)
      Walk(found.entries ++ visited.entries, visited.next)
    }

  private def visit(section: Section, index: Int, pageOf: Map[Int, Int]): Walk =
    section match
      case Section.Chapter(heading, _) =>
        Walk(entryFor(heading, index, pageOf, Vector.empty).toVector, index + 1)
      case Section.Part(heading, nested) =>
        val inside = walk(nested, index + 1, pageOf)
        val entry  = entryFor(heading, index, pageOf, inside.entries)
        Walk(entry.fold(inside.entries)(Vector(_)), inside.next)

  private def entryFor(
    heading: Option[SectionHeading],
    index: Int,
    pageOf: Map[Int, Int],
    children: Vector[OutlineEntry]
  ): Option[OutlineEntry] =
    for
      head <- heading
      title = titleOf(head)
      if title.nonEmpty
      page <- pageOf.get(index)
    yield OutlineEntry(title, page, children)

  /** Lines joined by a space; a tab or break becomes a space and other control characters are dropped, as the page's
    * own text is.
    */
  private def titleOf(heading: SectionHeading): String =
    heading.title.flatMap {
      case '\t' | '\n' | ' ' | '\u000B' => " "
      case other =>
        val kind = Character.getType(other)
        if kind == Character.CONTROL || kind == Character.FORMAT then "" else other.toString
    }.trim
