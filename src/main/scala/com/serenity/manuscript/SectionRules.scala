package com.serenity.manuscript

import com.serenity.richtext.RichTextRun

/** How a source's headings and separator lines map onto manuscript structure. Headings deeper than `chapterLevel` stay
  * in the chapter as centred paragraphs; with no `partLevel`, every heading at or above `chapterLevel` opens a chapter.
  */
final case class SectionRules(partLevel: Option[Int], chapterLevel: Int, sceneBreakPatterns: Set[String]):

  def isSceneBreak(text: String): Boolean =
    sceneBreakPatterns.contains(text.trim)

  private[manuscript] def roleOf(level: Int): HeadingRole =
    if partLevel.contains(level) then HeadingRole.Part
    else if level <= chapterLevel then HeadingRole.Chapter
    else HeadingRole.Subheading

object SectionRules:
  val DefaultSceneBreakPatterns: Set[String] = Set("#", "***", "* * *", "---")

  val default: SectionRules = SectionRules(None, 1, DefaultSceneBreakPatterns)

private[manuscript] enum HeadingRole:
  case Part
  case Chapter
  case Subheading

/** One structural element read from a source, before headings are grouped into sections. */
private[manuscript] enum SourceElement:
  case Heading(level: Int, runs: List[RichTextRun])
  case Content(block: Block)

private[manuscript] object SectionAssembly:

  final private case class OpenPart(heading: Option[SectionHeading], sections: Vector[Section])

  final private case class OpenChapter(heading: Option[SectionHeading], blocks: Vector[Block])

  final private case class Assembly(closed: Vector[Section], part: Option[OpenPart], chapter: Option[OpenChapter]):

    def withChapterClosed: Assembly =
      chapter.fold(this) { open =>
        val section = Section.Chapter(open.heading, open.blocks)
        part match
          case Some(openPart) =>
            copy(part = Some(openPart.copy(sections = openPart.sections :+ section)), chapter = None)
          case None => copy(closed = closed :+ section, chapter = None)
      }

    def withPartClosed: Assembly =
      val chapterClosed = withChapterClosed
      chapterClosed.part.fold(chapterClosed)(open =>
        chapterClosed.copy(closed = chapterClosed.closed :+ Section.Part(open.heading, open.sections), part = None)
      )

  /** Content before the first heading opens a chapter without a heading, so nothing in the source is dropped. */
  def assemble(rules: SectionRules, elements: List[SourceElement]): Vector[Section] =
    elements
      .foldLeft(Assembly(Vector.empty, None, None)) { (assembly, element) =>
        element match
          case SourceElement.Heading(level, runs) =>
            val heading = Some(SectionHeading.of(ManuscriptText.plainText(runs).trim))
            rules.roleOf(level) match
              case HeadingRole.Part =>
                assembly.withPartClosed.copy(part = Some(OpenPart(heading, Vector.empty)))
              case HeadingRole.Chapter =>
                assembly.withChapterClosed.copy(chapter = Some(OpenChapter(heading, Vector.empty)))
              case HeadingRole.Subheading =>
                withBlock(assembly, Block.Paragraph(runs, ParagraphKind.Centered))
          case SourceElement.Content(block) =>
            withBlock(assembly, block)
      }
      .withPartClosed
      .closed

  private def withBlock(assembly: Assembly, block: Block): Assembly =
    val open = assembly.chapter.getOrElse(OpenChapter(None, Vector.empty))
    assembly.copy(chapter = Some(open.copy(blocks = open.blocks :+ block)))
