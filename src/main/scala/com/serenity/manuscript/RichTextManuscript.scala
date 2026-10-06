package com.serenity.manuscript

import com.serenity.richtext.{ParagraphAlignment, ParagraphRole, RichTextDocument, RichTextParagraph}

/** A rich document to manuscript sections. Blank paragraphs are dropped: in a manuscript the spacing between paragraphs
  * belongs to the format, not to the text.
  */
object RichTextManuscript:

  def sections(document: RichTextDocument, rules: SectionRules): Vector[Section] =
    SectionAssembly.assemble(rules, document.paragraphs.flatMap(paragraph => element(rules, paragraph.normalized)))

  private def element(rules: SectionRules, paragraph: RichTextParagraph): Option[SourceElement] =
    val text = paragraph.plainText
    paragraph.role match
      case _ if text.trim.isEmpty        => None
      case ParagraphRole.Heading(level)  => Some(SourceElement.Heading(level, paragraph.runs))
      case _ if rules.isSceneBreak(text) => Some(SourceElement.Content(Block.SceneBreak))
      case ParagraphRole.Body | ParagraphRole.DropCap(_) =>
        val kind =
          if paragraph.alignment == ParagraphAlignment.Center then ParagraphKind.Centered else ParagraphKind.Body
        Some(SourceElement.Content(Block.Paragraph(paragraph.runs, kind)))
