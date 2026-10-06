package com.serenity.manuscript

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

import com.serenity.richtext.{InlineMark, RichTextParagraph, RichTextRun, RichTextStyle}
import org.commonmark.Extension
import org.commonmark.ext.gfm.tables.{TableCell, TableRow, TablesExtension}
import org.commonmark.node.{
  BlockQuote,
  BulletList,
  Code,
  Emphasis,
  FencedCodeBlock,
  HardLineBreak,
  Heading,
  HtmlBlock,
  HtmlInline,
  IndentedCodeBlock,
  ListBlock,
  Node,
  OrderedList,
  Paragraph,
  SoftLineBreak,
  StrongEmphasis,
  Text,
  ThematicBreak
}
import org.commonmark.parser.Parser

/** Markdown source to manuscript sections, read from the CommonMark AST so that `#` inside a code block is never taken
  * for a heading.
  */
object MarkdownManuscript:

  /** Inline code keeps this family so compile transforms can leave it alone; writers set their own body font. */
  val CodeFontFamily: String = "monospace"

  private val parser: Parser =
    Parser.builder().extensions(List[Extension](TablesExtension.create()).asJava).build()

  def sections(markdown: String, rules: SectionRules): Vector[Section] =
    SectionAssembly.assemble(rules, children(parser.parse(markdown)).flatMap(elements(rules, _, ParagraphKind.Body)))

  private def elements(rules: SectionRules, node: Node, kind: ParagraphKind): List[SourceElement] =
    node match
      case heading: Heading =>
        val runs = inlineRuns(heading)
        // `#` alone is an empty ATX heading to CommonMark, but it is how manuscript writers type a scene break.
        if ManuscriptText.plainText(runs).trim.isEmpty then List(SourceElement.Content(Block.SceneBreak))
        else List(SourceElement.Heading(heading.getLevel, runs))
      case _: ThematicBreak =>
        List(SourceElement.Content(Block.SceneBreak))
      case paragraph: Paragraph =>
        paragraphElement(rules, inlineRuns(paragraph), kind).toList
      case quote: BlockQuote =>
        children(quote).flatMap(elements(rules, _, ParagraphKind.BlockQuote))
      case code: FencedCodeBlock =>
        List(preformatted(code.getLiteral))
      case code: IndentedCodeBlock =>
        List(preformatted(code.getLiteral))
      case html: HtmlBlock =>
        HtmlMarkup.parts(html.getLiteral).flatMap {
          case HtmlMarkup.Part.Text(runs) => paragraphElement(rules, runs, kind).toList
          case HtmlMarkup.Part.SceneBreak => List(SourceElement.Content(Block.SceneBreak))
        }
      case list: ListBlock =>
        listElements(rules, list, kind)
      case row: TableRow =>
        val cells = children(row).collect { case cell: TableCell => inlineRuns(cell) }
        val runs =
          cells.zipWithIndex.flatMap((runs, index) => (if index == 0 then Nil else List(RichTextRun("\t"))) ++ runs)
        paragraphElement(rules, runs, kind).toList
      case other =>
        children(other).flatMap(elements(rules, _, kind))

  private def paragraphElement(
    rules: SectionRules,
    runs: List[RichTextRun],
    kind: ParagraphKind
  ): Option[SourceElement] =
    val text = ManuscriptText.plainText(runs)
    if text.trim.isEmpty then None
    else if rules.isSceneBreak(text) then Some(SourceElement.Content(Block.SceneBreak))
    else Some(SourceElement.Content(Block.Paragraph(RichTextParagraph(runs).normalized.runs, kind)))

  /** The manuscript model has no lists, so each item keeps its marker as text at the start of its first paragraph. */
  private def listElements(rules: SectionRules, list: ListBlock, kind: ParagraphKind): List[SourceElement] =
    val start = list match
      case ordered: OrderedList => Option(ordered.getMarkerStartNumber).map(_.intValue).getOrElse(1)
      case _                    => 1
    children(list).zipWithIndex.flatMap { (item, index) =>
      val marker = list match
        case _: BulletList => "• "
        case _             => s"${start + index}. "
      children(item).flatMap(elements(rules, _, kind)) match
        case SourceElement.Content(Block.Paragraph(runs, paragraphKind)) :: rest =>
          val marked = RichTextParagraph(RichTextRun(marker) :: runs).normalized.runs
          SourceElement.Content(Block.Paragraph(marked, paragraphKind)) :: rest
        case other => other
    }

  private def preformatted(literal: String): SourceElement =
    SourceElement.Content(Block.Preformatted(literal.stripSuffix("\n").split("\n", -1).toVector))

  private def inlineRuns(parent: Node): List[RichTextRun] =
    runsWithin(parent, RichTextStyle.empty)

  /** Inline tags change the style of later siblings; `<u>`/`</u>` are how Serenity's own Markdown save writes
    * underline. See [[HtmlMarkup]] for what each tag means.
    */
  private def runsWithin(parent: Node, style: RichTextStyle): List[RichTextRun] =
    children(parent)
      .foldLeft((Vector.empty[RichTextRun], style)) {
        case ((runs, current), html: HtmlInline) =>
          HtmlMarkup.effect(html.getLiteral) match
            case HtmlMarkup.Effect.LineBreak => (runs :+ RichTextRun("\n", current), current)
            case effect                      => (runs, HtmlMarkup.applied(current, effect))
        case ((runs, current), child) =>
          (runs ++ runsOf(child, current), current)
      }
      ._1
      .toList

  private def runsOf(node: Node, style: RichTextStyle): List[RichTextRun] =
    node match
      case text: Text        => List(RichTextRun(text.getLiteral, style))
      case _: SoftLineBreak  => List(RichTextRun(" ", style))
      case _: HardLineBreak  => List(RichTextRun("\n", style))
      case code: Code        => List(RichTextRun(code.getLiteral, style.withFontFamily(CodeFontFamily)))
      case _: Emphasis       => runsWithin(node, style.withMark(InlineMark.Italic))
      case _: StrongEmphasis => runsWithin(node, style.withMark(InlineMark.Bold))
      case other             => runsWithin(other, style)

  private def children(node: Node): List[Node] =
    @tailrec
    def loop(current: Option[Node], acc: List[Node]): List[Node] =
      current match
        case Some(child) => loop(Option(child.getNext), child :: acc)
        case None        => acc.reverse
    loop(Option(node.getFirstChild), Nil)
