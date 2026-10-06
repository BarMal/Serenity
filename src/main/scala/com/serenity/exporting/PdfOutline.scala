package com.serenity.exporting

import com.serenity.manuscript.layout.{PageKind, PagedDocument}
import com.serenity.manuscript.{Manuscript, Section, SectionHeading}

/** A bookmark: the heading's text, the zero-based page it starts on, and the chapters nested beneath it. */
final case class OutlineEntry(title: String, pageIndex: Int, children: Vector[OutlineEntry])

/** The PDF's bookmarks, derived from the manuscript's headings and where the paginator put them. A [[PagedDocument]]
  * does not say which section opened a page, so each heading claims the first section-start page, from where the last
  * one left off, whose first line is its own. A section without a heading has no bookmark but still owns its page,
  * which the search steps over.
  */
object PdfOutline:

  final private case class Opening(pageIndex: Int, firstLine: String)

  def entries(manuscript: Manuscript, paged: PagedDocument): Vector[OutlineEntry] =
    val openings = paged.pages.zipWithIndex.collect {
      case (page, index) if page.kind == PageKind.SectionStart =>
        Opening(index, page.lines.headOption.fold("")(_.text.trim))
    }
    walk(manuscript.body, openings.toList)._1

  private def walk(sections: Vector[Section], openings: List[Opening]): (Vector[OutlineEntry], List[Opening]) =
    sections.foldLeft((Vector.empty[OutlineEntry], openings)) {
      case ((found, remaining), section) =>
        val (entries, rest) = visit(section, remaining)
        (found ++ entries, rest)
    }

  private def visit(section: Section, openings: List[Opening]): (Vector[OutlineEntry], List[Opening]) =
    section match
      case Section.Chapter(heading, _) =>
        val (claimed, rest) = claim(heading, openings)
        (claimed.map(entry(_, Vector.empty)).toVector, rest)
      case Section.Part(heading, nested) =>
        val (claimed, afterHeading) = claim(heading, openings)
        val (children, rest)        = walk(nested, afterHeading)
        (claimed.fold(children)(opening => Vector(entry(opening, children))), rest)

  final private case class Claim(title: String, opening: Opening)

  private def entry(claim: Claim, children: Vector[OutlineEntry]): OutlineEntry =
    OutlineEntry(claim.title, claim.opening.pageIndex, children)

  /** The heading's claim on its page, and the pages that remain after it. A heading too long for one line wraps, so its
    * first line is matched whole before it is matched as a prefix.
    */
  private def claim(heading: Option[SectionHeading], openings: List[Opening]): (Option[Claim], List[Opening]) =
    val titled = heading.filter(_.title.trim.nonEmpty)
    val found = titled.flatMap { head =>
      val lead  = head.lines.headOption.fold("")(_.trim)
      val whole = openings.indexWhere(_.firstLine == lead)
      val index = if whole >= 0 then whole else openings.indexWhere(wrapsInto(lead, _))
      Option.when(index >= 0)(head -> index)
    }
    found.fold((None, openings)) { (head, index) =>
      (Some(Claim(head.title.trim, openings(index))), openings.drop(index + 1))
    }

  private def wrapsInto(lead: String, opening: Opening): Boolean =
    opening.firstLine.nonEmpty && lead.startsWith(s"${opening.firstLine} ")
