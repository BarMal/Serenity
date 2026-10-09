package com.serenity.richtext

import java.nio.charset.StandardCharsets

/** Saves a document that was read from a DOCX package: the package with `word/document.xml` (and the relationships for
  * any new hyperlink) replaced and every other part copied as it was.
  */
private[richtext] object DocxPackageWriter:
  private val WNs   = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  private val RelNs = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"

  def rewrite(document: RichTextDocument, source: DocumentSource): Array[Byte] =
    val paragraphs = document.paragraphs
    val plan       = source.body.fold(BodyPlanner.planWithoutSlices(paragraphs))(BodyPlanner.plan(paragraphs, _))
    val rewritten  = plan.collect { case PlannedBlock.Rewrite(paragraph, _) => paragraph }
    val links      = DocxLinks.allocate(rewritten.flatMap(_.runs.flatMap(_.style.link)).toList, source.relationships)
    val declaresRel =
      source.main.forall(_.text.contains(s"""xmlns:r="$RelNs""""))
    val markup = plan.map {
      case PlannedBlock.Verbatim(text) => text
      case PlannedBlock.Rewrite(paragraph, gap) =>
        DocxParagraphWriter.paragraphXml(paragraph, DocxWriteContext(links.ids, declaresRel, native = true)) + gap
    }.mkString
    val documentXml = (source.main, source.body) match
      case (Some(_), Some(body)) => body.head + markup + body.tail
      case _                     => documentShell(markup)
    val encoded       = source.main.fold(documentXml.getBytes(StandardCharsets.UTF_8))(_.encode(documentXml))
    val relationships = links.withAdditionsIn(source.relationships).map(_.getBytes(StandardCharsets.UTF_8))
    PackageRewriter.rewrite(
      source.archive,
      "DOCX",
      Map(DocxDocumentCodec.DocumentEntry -> encoded) ++
        relationships
          .filter(_ => source.relationships.isDefined)
          .map(DocxDocumentCodec.DocumentRelationshipsEntry -> _),
      relationships
        .filter(_ => source.relationships.isEmpty)
        .map(DocxDocumentCodec.DocumentRelationshipsEntry -> _)
        .toList
    )

  /** `word/document.xml` for a document with no source: the paragraphs and an empty section. */
  def newDocumentXml(document: RichTextDocument, links: DocxLinks): String =
    val context = DocxWriteContext(links.ids, declaresRelationshipNamespace = true, native = false)
    documentShell(
      document.paragraphs.map(DocxParagraphWriter.paragraphXml(_, context)).mkString("\n") + "\n    <w:sectPr/>\n"
    )

  private def documentShell(body: String): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<w:document xmlns:w="$WNs" xmlns:r="$RelNs">
       |  <w:body>
       |$body  </w:body>
       |</w:document>""".stripMargin
