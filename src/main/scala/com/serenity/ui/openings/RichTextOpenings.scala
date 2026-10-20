package com.serenity.ui.openings

import com.serenity.richtext.{InlineMark, ParagraphAlignment, ParagraphRole, RichTextDocument, RichTextParagraph}

/** Classifies the paragraphs of a rich-text document for the search for chapter openings. */
private[openings] object RichTextOpenings:

  def classify(document: RichTextDocument, rules: OpeningRules, explicitEpigraph: Int => Boolean)(
    line: Int
  ): OpeningLine =
    document.paragraphAt(line).fold(OpeningLine.Blocked)(classifyParagraph(_, line, rules, explicitEpigraph))

  private def classifyParagraph(
    paragraph: RichTextParagraph,
    line: Int,
    rules: OpeningRules,
    explicitEpigraph: Int => Boolean
  ): OpeningLine =
    val text = paragraph.plainText.trim
    if paragraph.isOpaqueBlock then OpeningLine.Blocked
    else if text.isEmpty then OpeningLine.Blank
    else
      paragraph.role match
        case ParagraphRole.Heading(level) if level <= 1 => OpeningLine.ChapterTitle
        case ParagraphRole.Heading(_)                   => OpeningLine.SubHeading
        case role =>
          if SceneBreaks.isSceneBreak(text) then OpeningLine.SceneBreak
          else if isEpigraph(paragraph, line, rules.epigraph, explicitEpigraph) then OpeningLine.Epigraph
          else if text.length < 2 then OpeningLine.Blocked
          else OpeningLine.Body(handDropCap = isHandDropCap(role))

  private def isEpigraph(
    paragraph: RichTextParagraph,
    line: Int,
    rule: EpigraphRule,
    explicitEpigraph: Int => Boolean
  ): Boolean =
    rule match
      case EpigraphRule.Never        => false
      case EpigraphRule.ExplicitOnly => explicitEpigraph(line)
      case EpigraphRule.AutoDetect   => looksLikeEpigraph(paragraph)

  private def isHandDropCap(role: ParagraphRole): Boolean =
    role match
      case ParagraphRole.DropCap(_) => true
      case _                        => false

  def looksLikeEpigraph(paragraph: RichTextParagraph): Boolean =
    paragraph.alignment == ParagraphAlignment.Center || paragraph.alignment == ParagraphAlignment.Right ||
      entirelyItalic(paragraph)

  private def entirelyItalic(paragraph: RichTextParagraph): Boolean =
    val written = paragraph.runs.filter(_.text.exists(!_.isWhitespace))
    written.nonEmpty && written.forall(_.style.marks.contains(InlineMark.Italic))
