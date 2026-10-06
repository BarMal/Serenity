package com.serenity.richtext

import com.serenity.richtext.XmlDom.{escapeAttribute, escapeText}

/** The style names a written paragraph refers to. `native` is true when the document was read from an ODT package: only
  * then are the attributes and opaque XML it carries ODT. `anchorDeclarations` declares the `xlink` prefix on the links
  * when the part's root does not.
  */
final private[richtext] case class OdtNaming(
    paragraph: RichTextParagraph => Option[String],
    span: RichTextStyle => Option[String],
    anchorDeclarations: String,
    native: Boolean
)

/** Writes a model paragraph as a `text:p` or `text:h` element. */
private[richtext] object OdtParagraphWriter:

  def paragraphXml(paragraph: RichTextParagraph, naming: OdtNaming): String =
    paragraph.opaqueBlock.fold(textParagraphXml(paragraph, naming))(block =>
      if naming.native then block.raw else "<text:p/>"
    )

  private def textParagraphXml(paragraph: RichTextParagraph, naming: OdtNaming): String =
    val (tag, level) = paragraph.role match
      case ParagraphRole.Heading(headingLevel) => ("text:h", s""" text:outline-level="${headingLevel.max(1)}"""")
      case _                                   => ("text:p", "")
    val attributes = paragraph.source.filter(_ => naming.native).fold("")(_.openTagAttributes)
    val style      = naming.paragraph(paragraph).fold("")(name => s""" text:style-name="${escapeAttribute(name)}"""")
    s"<$tag$attributes$level$style>${runsXml(paragraph, naming)}</$tag>"

  final private case class RunContext(run: RichTextRun, before: Option[Char], after: Option[Char])

  private def runsXml(paragraph: RichTextParagraph, naming: OdtNaming): String =
    val runs = paragraph.runs.toVector
    val contexts = runs.indices.toList.map { index =>
      RunContext(
        runs(index),
        runs
          .lift(index - 1)
          .flatMap(neighbour => Option.when(neighbour.atom.isEmpty)(neighbour.text.lastOption).flatten),
        runs
          .lift(index + 1)
          .flatMap(neighbour => Option.when(neighbour.atom.isEmpty)(neighbour.text.headOption).flatten)
      )
    }
    contexts
      .foldRight(List.empty[(Option[String], List[RunContext])]) {
        case (context, (target, group) :: tail) if target == context.run.style.link =>
          (target, context :: group) :: tail
        case (context, acc) => (context.run.style.link, List(context)) :: acc
      }
      .map { (target, group) =>
        val spanXml = group.map(runXml(_, naming)).mkString
        target.fold(spanXml) { link =>
          val notes =
            group.headOption
              .filter(_ => naming.native)
              .fold("")(first => RawProperty.linkAttributesFor(first.run.style))
          s"""<text:a xlink:type="simple" xlink:href="${escapeAttribute(link)}"$notes${naming.anchorDeclarations}>$spanXml</text:a>"""
        }
      }
      .mkString

  private def runXml(context: RunContext, naming: OdtNaming): String =
    val run = context.run
    run.atom match
      case Some(InlineAtom.Opaque(raw, _)) => if naming.native then raw else ""
      case Some(InlineAtom.SoftBreak)      => styled(run, "<text:line-break/>", naming)
      case Some(InlineAtom.Block(_, _))    => ""
      case None                            => styled(run, textXml(run.text, context.before, context.after), naming)

  private def styled(run: RichTextRun, content: String, naming: OdtNaming): String =
    naming
      .span(run.style)
      .fold(content)(name => s"""<text:span text:style-name="${escapeAttribute(name)}">$content</text:span>""")

  /** A space between two non-space characters is written as itself; any other space, which ODF would collapse or drop,
    * is a `text:s`. `before` and `after` are the characters of the neighbouring text runs.
    */
  private def textXml(text: String, before: Option[Char], after: Option[Char]): String =
    def isSpace(char: Char): Boolean = char == ' ' || char == '\t' || char == '\n'
    text.zipWithIndex
      .foldLeft((StringBuilder(), List.empty[String])) {
        case ((chunk, acc), ('\t', _)) =>
          (StringBuilder(), acc ++ textChunkXml(chunk) :+ "<text:tab/>")
        case ((chunk, acc), (' ', index)) =>
          val previous = if index > 0 then Some(text.charAt(index - 1)) else before
          val next     = if index < text.length - 1 then Some(text.charAt(index + 1)) else after
          if previous.exists(!isSpace(_)) && next.exists(!isSpace(_)) then
            chunk.append(' ')
            (chunk, acc)
          else (StringBuilder(), acc ++ textChunkXml(chunk) :+ "<text:s/>")
        case ((chunk, acc), (char, _)) =>
          chunk.append(char)
          (chunk, acc)
      } match
      case (chunk, acc) => (acc ++ textChunkXml(chunk)).mkString

  private def textChunkXml(chunk: StringBuilder): Option[String] =
    Option.when(chunk.nonEmpty)(escapeText(chunk.toString))
