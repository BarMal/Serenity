package com.serenity.richtext

import com.serenity.richtext.XmlDom.{escapeAttribute, escapeText}

/** How relationships are referenced from rewritten paragraphs. `declaresRelationshipNamespace` is false when the source
  * root element never declares the `r:` prefix, in which case a hyperlink declares it itself. `native` is true when the
  * document was read from a DOCX package: only then are the properties and opaque XML it carries DOCX.
  */
final private[richtext] case class DocxWriteContext(
    linkIds: Map[String, String],
    declaresRelationshipNamespace: Boolean,
    native: Boolean
)

/** Writes a model paragraph as a `w:p` element, with the properties it was imported with. */
private[richtext] object DocxParagraphWriter:
  private val RelNs = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"

  def paragraphXml(paragraph: RichTextParagraph, context: DocxWriteContext): String =
    val attributes = paragraph.source.filter(_ => context.native).fold("")(_.openTagAttributes)
    val content    = paragraph.linkSpans.map(linkSpanXml(_, context)).mkString
    s"<w:p$attributes>${DocxProperties.paragraphPropertiesXml(paragraph, context.native)}$content</w:p>"

  private def linkSpanXml(span: (Option[String], List[RichTextRun]), context: DocxWriteContext): String =
    val (target, runs) = span
    val runsXml        = runs.map(runXml(_, context)).mkString
    target.fold(runsXml) { link =>
      val reference =
        context.linkIds.get(link).fold(s"""w:anchor="${escapeAttribute(link.stripPrefix("#"))}"""") { id =>
          val namespace = if context.declaresRelationshipNamespace then "" else s""" xmlns:r="$RelNs""""
          s"""r:id="$id"$namespace"""
        }
      val notes = runs.headOption.filter(_ => context.native).fold("")(run => RawProperty.linkAttributesFor(run.style))
      s"<w:hyperlink $reference$notes>$runsXml</w:hyperlink>"
    }

  private def runXml(run: RichTextRun, context: DocxWriteContext): String =
    run.atom match
      case Some(InlineAtom.Opaque(raw, _)) => if context.native then raw else ""
      case Some(InlineAtom.SoftBreak)      => wrapped(run, "<w:br/>", context)
      case None                            => wrapped(run, textXml(run.text), context)

  private def wrapped(run: RichTextRun, content: String, context: DocxWriteContext): String =
    s"<w:r>${DocxProperties.runPropertiesXml(run.style, context.native)}$content</w:r>"

  private def textXml(text: String): String =
    text
      .foldLeft((StringBuilder(), List.empty[String])) {
        case ((chunk, acc), '\t') =>
          (StringBuilder(), acc ++ textChunkXml(chunk) :+ "<w:tab/>")
        case ((chunk, acc), char) =>
          chunk.append(char)
          (chunk, acc)
      } match
      case (chunk, acc) => (acc ++ textChunkXml(chunk)).mkString

  private def textChunkXml(chunk: StringBuilder): Option[String] =
    Option.when(chunk.nonEmpty)(s"""<w:t xml:space="preserve">${escapeText(chunk.toString)}</w:t>""")
