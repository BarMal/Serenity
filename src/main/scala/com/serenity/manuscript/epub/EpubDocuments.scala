package com.serenity.manuscript.epub

import com.serenity.manuscript.{Block, FrontMatter, Manuscript, ParagraphKind, Section, SectionHeading}

/** One content document of the book: its place in the package, and the markup between `<body>` and `</body>`. */
final private[epub] case class EpubDocument(
    id: String,
    epubType: String,
    title: String,
    body: Vector[String]
):
  def path: String = s"text/$id.xhtml"

/** A table-of-contents line; parts nest their chapters. */
final private[epub] case class TocEntry(title: String, path: String, children: Vector[TocEntry])

/** The manuscript cut into content documents, in reading order, with the table of contents that points at them. */
final private[epub] case class EpubContent(
    front: Vector[EpubDocument],
    body: Vector[EpubDocument],
    toc: Vector[TocEntry]
):
  def all: Vector[EpubDocument] = front ++ body

private[epub] object EpubDocuments:

  /** Book typesetting puts an ornament between scenes; the submission `#` is for editors, not readers. */
  val SceneBreakOrnament: String = "* * *"

  def of(manuscript: Manuscript): EpubContent =
    val hasParts = manuscript.body.exists {
      case _: Section.Part    => true
      case _: Section.Chapter => false
    }
    val built = build(manuscript.body, hasParts, Built(Vector.empty, Vector.empty, 0, 0))
    EpubContent(
      front = frontDocuments(manuscript),
      body = withEndMarker(built.documents, manuscript.endMarker),
      toc = built.toc
    )

  final private case class Built(documents: Vector[EpubDocument], toc: Vector[TocEntry], parts: Int, chapters: Int)

  private def build(sections: Vector[Section], hasParts: Boolean, from: Built): Built =
    sections.foldLeft(from) { (built, section) =>
      section match
        case Section.Chapter(heading, blocks) =>
          val number   = built.chapters + 1
          val document = chapter(number, heading, blocks, hasParts)
          built.copy(
            documents = built.documents :+ document,
            toc = built.toc :+ TocEntry(document.title, document.path, Vector.empty),
            chapters = number
          )
        case Section.Part(Some(heading), nested) =>
          val number   = built.parts + 1
          val document = part(number, heading)
          val inner = build(nested, hasParts, Built(built.documents :+ document, Vector.empty, number, built.chapters))
          inner.copy(toc = built.toc :+ TocEntry(document.title, document.path, inner.toc))
        case Section.Part(None, nested) =>
          build(nested, hasParts, built)
    }

  private def frontDocuments(manuscript: Manuscript): Vector[EpubDocument] =
    manuscript.front.toVector.map {
      case FrontMatter.TitlePage => titlePage(manuscript)
      case FrontMatter.Dedication(text) =>
        EpubDocument(
          "dedication",
          "dedication",
          "Dedication",
          Vector(s"""<p class="dedication">${EpubXml.escape(text)}</p>""")
        )
    }

  private def titlePage(manuscript: Manuscript): EpubDocument =
    val meta   = manuscript.meta
    val byline = Option.when(meta.byline.trim.nonEmpty)(s"""<p class="byline">by ${EpubXml.escape(meta.byline)}</p>""")
    EpubDocument(
      "titlepage",
      "titlepage",
      "Title Page",
      Vector(s"""<h1 class="title">${EpubXml.escape(meta.title)}</h1>""") ++ byline
    )

  private def part(number: Int, heading: SectionHeading): EpubDocument =
    EpubDocument(f"part-$number%03d", "part", heading.title, Vector(headingMarkup("h1", "part-title", heading)))

  private def chapter(
    number: Int,
    heading: Option[SectionHeading],
    blocks: Vector[Block],
    hasParts: Boolean
  ): EpubDocument =
    val tag = if hasParts then "h2" else "h1"
    EpubDocument(
      f"chapter-$number%03d",
      "chapter",
      heading.fold(s"Chapter $number")(_.title),
      heading.map(headingMarkup(tag, "chapter-title", _)).toVector ++ blockMarkup(blocks)
    )

  private def headingMarkup(tag: String, cssClass: String, heading: SectionHeading): String =
    val lines = heading.lines.map(EpubXml.escape).mkString("<br/>")
    s"""<$tag class="$cssClass">$lines</$tag>"""

  private def blockMarkup(blocks: Vector[Block]): Vector[String] =
    blocks.zipWithIndex.map { (block, index) =>
      val opensScene = index == 0 || blocks.lift(index - 1).contains(Block.SceneBreak)
      block match
        case Block.Paragraph(runs, ParagraphKind.Body) =>
          val css = if opensScene then """ class="first"""" else ""
          s"<p$css>${EpubXml.runs(runs)}</p>"
        case Block.Paragraph(runs, ParagraphKind.BlockQuote) =>
          s"<blockquote><p>${EpubXml.runs(runs)}</p></blockquote>"
        case Block.Paragraph(runs, ParagraphKind.Centered) =>
          s"""<p class="centered">${EpubXml.runs(runs)}</p>"""
        case Block.SceneBreak =>
          s"""<p class="scene-break">$SceneBreakOrnament</p>"""
        case Block.Preformatted(lines) =>
          s"<pre>${lines.map(EpubXml.escape).mkString("\n")}</pre>"
    }

  private def withEndMarker(documents: Vector[EpubDocument], marker: Option[String]): Vector[EpubDocument] =
    marker.fold(documents) { text =>
      documents.lastOption.fold(documents)(last =>
        documents.dropRight(1) :+ last.copy(body =
          last.body :+ s"""<p class="end-marker">${EpubXml.escape(text)}</p>"""
        )
      )
    }
