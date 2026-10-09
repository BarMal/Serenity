package com.serenity.manuscript.docx

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.util.zip.{ZipEntry, ZipOutputStream}

import com.serenity.manuscript.{
  Block,
  FrontMatter,
  Manuscript,
  ManuscriptFormat,
  ManuscriptPageNumbering,
  ParagraphKind,
  Section
}
import com.serenity.richtext.{InlineMark, RichTextRun}

/** Writes a [[Manuscript]] as a Word document laid out in a manuscript [[ManuscriptFormat]].
  *
  * This is deliberately not `DocxDocumentCodec`: that codec round-trips the editor's own documents and treats styles,
  * headers and section properties as content it cannot keep, while a manuscript needs exactly those. The output is
  * byte-for-byte deterministic -- fixed part order and entry timestamps, no dates in the metadata -- so it can be
  * checked against golden files.
  */
object ManuscriptDocxWriter:

  private val WNs = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  private val RNs = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"

  /** The DOS epoch, so that the archive never records when it was written. */
  private val EntryTime = LocalDateTime.of(1980, 1, 1, 0, 0)

  def write(manuscript: Manuscript, format: ManuscriptFormat): Array[Byte] =
    val output = ByteArrayOutputStream()
    val zip    = ZipOutputStream(output, StandardCharsets.UTF_8)
    try
      parts(manuscript, format).foreach { (name, content) =>
        val entry = ZipEntry(name)
        entry.setTimeLocal(EntryTime)
        zip.putNextEntry(entry)
        zip.write(content.getBytes(StandardCharsets.UTF_8))
        zip.closeEntry()
      }
    finally zip.close()
    output.toByteArray

  /** Every part of the package, in archive order. */
  def parts(manuscript: Manuscript, format: ManuscriptFormat): List[(String, String)] =
    List(
      "[Content_Types].xml"          -> ManuscriptDocxParts.contentTypes,
      "_rels/.rels"                  -> ManuscriptDocxParts.packageRelationships,
      "docProps/core.xml"            -> ManuscriptDocxParts.coreProperties(manuscript.meta),
      "word/document.xml"            -> documentXml(manuscript, format),
      "word/styles.xml"              -> ManuscriptDocxParts.styles(format),
      "word/settings.xml"            -> ManuscriptDocxParts.settings(format),
      "word/header1.xml"             -> ManuscriptDocxParts.header(manuscript.meta, format),
      "word/_rels/document.xml.rels" -> ManuscriptDocxParts.documentRelationships
    )

  /** One `w:p`. Properties are written in the order the WordprocessingML schema requires. */
  final private case class DocxParagraph(
      style: String,
      runs: String,
      pageBreakBefore: Boolean = false,
      rightTabTwips: Option[Int] = None,
      spaceBeforeTwips: Option[Int] = None,
      spaceAfterTwips: Option[Int] = None,
      sectionProperties: Option[String] = None
  ):

    def xml: String =
      val spacing = Option.when(spaceBeforeTwips.isDefined || spaceAfterTwips.isDefined)(
        "<w:spacing" + spaceBeforeTwips.fold("")(twips => s""" w:before="$twips"""") +
          spaceAfterTwips.fold("")(twips => s""" w:after="$twips"""") + "/>"
      )
      val properties = List(
        Some(s"""<w:pStyle w:val="$style"/>"""),
        Option.when(pageBreakBefore)("<w:pageBreakBefore/>"),
        rightTabTwips.map(twips => s"""<w:tabs><w:tab w:val="right" w:pos="$twips"/></w:tabs>"""),
        spacing,
        sectionProperties
      ).flatten
      s"""    <w:p><w:pPr>${properties.mkString}</w:pPr>$runs</w:p>"""

  private def documentXml(manuscript: Manuscript, format: ManuscriptFormat): String =
    val front = frontParagraphs(manuscript, format)
    val frontSection = front.lastOption.fold(Vector.empty[DocxParagraph])(last =>
      front.dropRight(1) :+ last.copy(sectionProperties = Some(sectionPropertiesXml(format, body = false)))
    )
    val hasParts = manuscript.body.exists {
      case _: Section.Part    => true
      case _: Section.Chapter => false
    }
    val body = manuscript.body.zipWithIndex.flatMap((section, index) =>
      sectionParagraphs(section, format, hasParts, opensSection = index == 0)
    ) ++ manuscript.endMarker.map(marker =>
      DocxParagraph("Centered", textRuns(marker), spaceBeforeTwips = Some(format.lineSpacing))
    )
    s"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
       |<w:document xmlns:w="$WNs" xmlns:r="$RNs">
       |  <w:body>
       |${(frontSection ++ body).map(_.xml).mkString("\n")}
       |    ${sectionPropertiesXml(format, body = true)}
       |  </w:body>
       |</w:document>
       |""".stripMargin

  /** The title page and dedication form their own section, with no running header, so the story opens on page 1. */
  private def frontParagraphs(manuscript: Manuscript, format: ManuscriptFormat): Vector[DocxParagraph] =
    val paragraphs = manuscript.front.toVector.flatMap {
      case FrontMatter.TitlePage =>
        val meta      = manuscript.meta
        val wordCount = meta.wordCountLine
        val firstLine = DocxParagraph(
          "ContactBlock",
          textRuns(meta.author.legal) + "<w:r><w:tab/></w:r>" + textRuns(wordCount),
          rightTabTwips = Some(format.textWidthTwips)
        )
        val contact = meta.contact.map(line => DocxParagraph("ContactBlock", textRuns(line)))
        val title =
          DocxParagraph("Title", textRuns(meta.title), spaceBeforeTwips = Some(drop(format, format.titleDrop)))
        val byline = Option.when(meta.byline.trim.nonEmpty)(DocxParagraph("Byline", textRuns(s"by ${meta.byline}")))
        Vector(firstLine) ++ contact ++ Vector(title) ++ byline.toVector
      case FrontMatter.Dedication(text) =>
        Vector(
          DocxParagraph(
            "Centered",
            textRuns(text),
            pageBreakBefore = true,
            spaceBeforeTwips = Some(drop(format, format.chapterDrop))
          )
        )
    }
    paragraphs.headOption.fold(paragraphs)(first => first.copy(pageBreakBefore = false) +: paragraphs.drop(1))

  /** Every part and chapter starts a new page, a third of the way down. The first section of the body follows a section
    * break, which already starts a page, so it takes no page break of its own.
    */
  private def sectionParagraphs(
    section: Section,
    format: ManuscriptFormat,
    hasParts: Boolean,
    opensSection: Boolean
  ): Vector[DocxParagraph] =
    section match
      case Section.Part(heading, chapters) =>
        val headingLines =
          heading.fold(Vector.empty[String])(_.lines).map(line => DocxParagraph("Heading1", textRuns(line)))
        opening(headingLines, format, opensSection) ++
          chapters.flatMap(sectionParagraphs(_, format, hasParts, opensSection = false))
      case Section.Chapter(heading, blocks) =>
        val style        = if hasParts then "Heading2" else "Heading1"
        val headingLines = heading.fold(Vector.empty[String])(_.lines).map(line => DocxParagraph(style, textRuns(line)))
        val spaced =
          headingLines.lastOption.fold(headingLines)(last =>
            headingLines.dropRight(1) :+ last.copy(spaceAfterTwips = Some(format.lineSpacing))
          )
        opening(spaced ++ blocks.flatMap(blockParagraphs(_, format)), format, opensSection)

  private def opening(
    paragraphs: Vector[DocxParagraph],
    format: ManuscriptFormat,
    opensSection: Boolean
  ): Vector[DocxParagraph] =
    paragraphs.headOption.fold(paragraphs)(first =>
      first.copy(pageBreakBefore = !opensSection, spaceBeforeTwips = Some(drop(format, format.chapterDrop))) +:
        paragraphs.drop(1)
    )

  private def blockParagraphs(block: Block, format: ManuscriptFormat): Vector[DocxParagraph] =
    block match
      case Block.Paragraph(runs, kind) =>
        val style = kind match
          case ParagraphKind.Body       => "Normal"
          case ParagraphKind.BlockQuote => "BlockQuote"
          case ParagraphKind.Centered   => "Centered"
        Vector(DocxParagraph(style, runs.map(runXml).mkString))
      case Block.SceneBreak =>
        Vector(DocxParagraph("SceneBreak", textRuns(format.sceneBreak)))
      case Block.Preformatted(lines) =>
        lines.map(line => DocxParagraph("Preformatted", textRuns(line)))

  private def drop(format: ManuscriptFormat, fraction: Double): Int =
    math.round(format.textHeightTwips * fraction).toInt

  private def sectionPropertiesXml(format: ManuscriptFormat, body: Boolean): String =
    val header    = if body then """<w:headerReference w:type="default" r:id="rId3"/>""" else ""
    val numbering = if body then s"""<w:pgNumType w:start="${ManuscriptPageNumbering.FirstBodyPage}"/>""" else ""
    val paper     = format.paper
    val margin    = format.marginTwips
    s"""<w:sectPr>$header<w:pgSz w:w="${paper.widthTwips}" w:h="${paper.heightTwips}"/>""" +
      s"""<w:pgMar w:top="$margin" w:right="$margin" w:bottom="$margin" w:left="$margin" """ +
      s"""w:header="${format.headerMarginTwips}" w:footer="${format.headerMarginTwips}" w:gutter="0"/>$numbering</w:sectPr>"""

  /** Only the marks carry over: a manuscript is set in one face and size, whatever fonts the draft used. */
  private def runXml(run: RichTextRun): String =
    val marks = List(
      Option.when(run.style.marks.contains(InlineMark.Bold))("<w:b/>"),
      Option.when(run.style.marks.contains(InlineMark.Italic))("<w:i/>"),
      Option.when(run.style.marks.contains(InlineMark.Underline))("""<w:u w:val="single"/>""")
    ).flatten
    val properties = if marks.isEmpty then "" else marks.mkString("<w:rPr>", "", "</w:rPr>")
    s"<w:r>$properties${ManuscriptDocxParts.textContent(run.text)}</w:r>"

  private def textRuns(text: String): String =
    if text.isEmpty then "" else runXml(RichTextRun(text))
