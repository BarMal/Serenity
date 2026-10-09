package com.serenity.richtext

import com.serenity.richtext.OdtStyles.{OfficeNs, TextNs, XlinkNs}
import com.serenity.richtext.XmlDom.{attribute, isElement}
import org.w3c.dom.{Element, Node}

/** Styles for reading, plus (when the part's text could be mapped) the means to keep unmodelled XML verbatim. */
final private[richtext] case class OdtReadContext(styles: OdtStyles, sources: Option[SourceMap]):
  def raw(element: Element): Option[String] = sources.flatMap(_.raw(element))

/** Turns `text:p` and `text:h` into model paragraphs. Inline content the model does not hold (images, notes, bookmarks,
  * comments) becomes an opaque atom; the style names it replaces are remembered as raw properties.
  */
private[richtext] object OdtParagraphReader:
  val StyleNameProperty: String = "style-name"
  val SpanStyleProperty: String = "@span-style"

  private val MarkerElements = Set(
    ("text", "bookmark"),
    ("text", "bookmark-start"),
    ("text", "bookmark-end"),
    ("text", "reference-mark"),
    ("text", "reference-mark-start"),
    ("text", "reference-mark-end"),
    ("text", "alphabetical-index-mark"),
    ("text", "alphabetical-index-mark-start"),
    ("text", "alphabetical-index-mark-end"),
    ("office", "annotation"),
    ("office", "annotation-end")
  )

  private val LinkTargetAttributes = """\s+xlink:(type|href|show|actuate)\s*=\s*("[^"]*"|'[^']*')""".r
  private val StyleAttributes      = """\s+text:(style-name|outline-level)\s*=\s*("[^"]*"|'[^']*')""".r

  def read(element: Element, blockIndex: Int, context: OdtReadContext): RichTextParagraph =
    val styleName      = attribute(element, TextNs, "style-name")
    val paragraphStyle = styleName.flatMap(context.styles.paragraphStyles.get)
    val alignment      = paragraphStyle.map(_.alignment).getOrElse(ParagraphAlignment.Left)
    val role =
      if isElement(element, TextNs, "h") then
        ParagraphRole.Heading(attribute(element, TextNs, "outline-level").flatMap(_.toIntOption).getOrElse(1).max(1))
      else paragraphStyle.flatMap(_.dropCapLines).map(ParagraphRole.dropCap).getOrElse(ParagraphRole.Body)
    val content  = nodeRuns(element.getChildNodes, RichTextStyle.empty, context)
    val imported = RichTextParagraph(content, alignment, role).normalized
    context.sources.fold(imported) { sources =>
      val style =
        styleName.map(name => RawProperty(StyleNameProperty, name, Some(OdtParagraphStyle.of(imported).canonical)))
      imported.copy(source =
        Some(ParagraphSource(blockIndex, attributesOf(element, sources), style.toList, Some(imported)))
      )
    }

  private def withLinkNotes(element: Element, style: RichTextStyle, context: OdtReadContext): RichTextStyle =
    val notes =
      context.sources.flatMap(sources => sources.openTag(element).zip(sources.qualifiedName(element))).toList.flatMap {
        (tag, name) =>
          RawProperty.linkAttributes(
            LinkTargetAttributes.replaceAllIn(tag.stripPrefix("<" + name).stripSuffix(">").stripSuffix("/"), ""),
            style.link
          )
      }
    style.copy(extras = style.extras.filterNot(_.name == RawProperty.LinkAttributes) ++ notes)

  private def attributesOf(element: Element, sources: SourceMap): String =
    val tag  = sources.openTag(element).getOrElse("")
    val name = sources.qualifiedName(element).getOrElse("")
    StyleAttributes.replaceAllIn(tag.stripPrefix("<" + name).stripSuffix(">").stripSuffix("/"), "")

  private def nodeRuns(nodes: org.w3c.dom.NodeList, style: RichTextStyle, context: OdtReadContext): List[RichTextRun] =
    XmlDom.nodes(nodes).flatMap(runsFromNode(_, style, context))

  private def runsFromNode(node: Node, style: RichTextStyle, context: OdtReadContext): List[RichTextRun] =
    node match
      case element: Element => runsFromElement(element, style, context)
      case text if text.getNodeType == Node.TEXT_NODE =>
        Option(text.getNodeValue).filter(_.nonEmpty).map(RichTextRun(_, style)).toList
      case _ => Nil

  private def runsFromElement(element: Element, style: RichTextStyle, context: OdtReadContext): List[RichTextRun] =
    if element.getNamespaceURI == TextNs then
      element.getLocalName match
        case "span" => nodeRuns(element.getChildNodes, spanStyle(element, style, context), context)
        case "a" =>
          val linked = attribute(element, XlinkNs, "href").fold(style)(style.withLink)
          nodeRuns(element.getChildNodes, withLinkNotes(element, linked, context), context)
        case "s" =>
          List(RichTextRun(" " * attribute(element, TextNs, "c").flatMap(_.toIntOption).getOrElse(1).max(1), style))
        case "tab"             => List(RichTextRun("\t", style))
        case "line-break"      => List(RichTextRun.softBreak(style))
        case "soft-page-break" => Nil
        case _                 => opaque(element, style, context)
    else opaque(element, style, context)

  private def opaque(element: Element, style: RichTextStyle, context: OdtReadContext): List[RichTextRun] =
    val marker = element.getNamespaceURI match
      case TextNs   => MarkerElements.contains(("text", element.getLocalName))
      case OfficeNs => MarkerElements.contains(("office", element.getLocalName))
      case _        => false
    context
      .raw(element)
      .map(RichTextRun.opaque(_, visible = !marker, RichTextStyle.empty.copy(link = style.link)))
      .toList

  /** A span's own style applied over the style around it. The name of a style this file does not define itself (one
    * from `styles.xml`) is kept, so that an unchanged run is written back with it; a style defined here is fully
    * described by the model's formatting and is written again as an automatic style when its paragraph is rewritten.
    */
  private def spanStyle(element: Element, base: RichTextStyle, context: OdtReadContext): RichTextStyle =
    attribute(element, TextNs, "style-name").fold(base) { name =>
      context.styles.textStyles.get(name).fold(withNamedStyle(base, name))(OdtStyles.mergeStyles(base, _))
    }

  private def withNamedStyle(base: RichTextStyle, name: String): RichTextStyle =
    base.copy(extras =
      RawProperty(SpanStyleProperty, name, Some(OdtStyles.canonical(base))) ::
        base.extras.filterNot(_.name == SpanStyleProperty)
    )
