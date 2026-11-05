package com.serenity.richtext

/** Frees headings of the direct formatting their text carries as a whole, so the heading definition shows again
  * (#2628).
  *
  * A heading's definition is the base of its look and a run's own size, family, colour and marks win over it, so a
  * heading imported with direct formatting keeps that look when the definition changes. Reformatting removes a
  * formatting value from a heading only when every text run of the heading has it, which is what an import produces for
  * a heading set as a whole; emphasis on part of a heading is the author's and stays, as do links, raw properties,
  * inline atoms and the paragraph's alignment.
  */
object HeadingReformat:

  /** How many headings changed and how many formatting values were removed from them in all. */
  final case class Report(headings: Int, attributes: Int):
    def isEmpty: Boolean = headings == 0

  object Report:
    val none: Report = Report(headings = 0, attributes = 0)

  final case class Result(document: RichTextDocument, report: Report)

  /** A formatting value of a run: `value` is what the run has of it, if anything, and `cleared` is the style without
    * it.
    */
  final private case class Attribute(value: RichTextStyle => Option[String], cleared: RichTextStyle => RichTextStyle)

  private def mark(which: InlineMark): Attribute =
    Attribute(style => Option.when(style.marks.contains(which))(which.toString), _.withoutMark(which))

  private val attributes: List[Attribute] = List(
    Attribute(_.fontSize.map(_.toString), _.copy(fontSize = None)),
    Attribute(_.fontFamily, _.copy(fontFamily = None)),
    Attribute(_.color, _.copy(color = None)),
    mark(InlineMark.Bold),
    mark(InlineMark.Italic),
    mark(InlineMark.Underline)
  )

  private def isText(run: RichTextRun): Boolean = run.atom.isEmpty && !run.isEmpty

  private def sharedByAll(attribute: Attribute, styles: List[RichTextStyle]): Boolean =
    styles.headOption.map(attribute.value) match
      case Some(first @ Some(_)) => styles.forall(attribute.value(_) == first)
      case _                     => false

  /** `paragraph` with the formatting its text has as a whole removed, and how many values that was. Anything that is
    * not a heading, and a heading with nothing to remove, comes back as it is.
    */
  private[richtext] def reformat(paragraph: RichTextParagraph): (RichTextParagraph, Int) =
    paragraph.role match
      case ParagraphRole.Heading(_) if !paragraph.isOpaqueBlock =>
        val textStyles = paragraph.runs.filter(isText).map(_.style)
        val removed    = attributes.filter(sharedByAll(_, textStyles))
        if removed.isEmpty then (paragraph, 0)
        else
          val stripped = paragraph.runs.map { run =>
            if isText(run) then
              run.copy(style = removed.foldLeft(run.style)((style, attribute) => attribute.cleared(style)))
            else run
          }
          (paragraph.copy(runs = stripped).normalized, removed.size)
      case _ => (paragraph, 0)
